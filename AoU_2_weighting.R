# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,dplyr,tidyr,survival,survey,cobalt,mice,ggplot2,viridis,table1,grplasso,randomForestSRC,RColorBrewer)

'%!in%' <- function(x,y)!('%in%'(x,y))

table1_weight = function(var_list,df.clean,weight.var){
  
  design <- svydesign(id = ~1, weights = ~get(weight.var), data = df.clean)
  
  res = c()
  for (var in var_list) {
    if(is.numeric(df.clean[,var] %>% unlist())){
      mean_var <- svymean(as.formula(paste("~", var)), design, na.rm = TRUE)
      mean_var_value <- coef(mean_var)
      mean_var_se <- SE(mean_var)
      res = res %>%
        bind_rows(.,data.frame(variable = var)) %>%
        bind_rows(.,data_frame(variable = 'Mean',stat = sprintf("%0.1f (%0.1f)", mean_var_value, mean_var_se)))
    } else{
      prop_var <- svytable(as.formula(paste("~", var)), design)
      prop_var_values <- prop.table(prop_var) * 100
      prop_df = data_frame(variable = as.character(names(prop_var)),stat = sprintf("%0.1f (%0.1f%%)", prop_var, prop_var_values))
      
      res = res %>%
        bind_rows(.,data.frame(variable = var)) %>%
        bind_rows(.,prop_df)
    }
  }
  return(res)
}

age_specific_IR = function(df.ir,cut.offs,event,weight.var){
  # function to calculate interval overlaps between two age intervals
  overlap_time = function(start1,stop1,start2,stop2) {
    if(sum(stop1<=start1) >0 | sum(stop2<=start2) > 0) stop('Stop time must be greater than start times')
    return(ifelse(pmin(stop1,stop2) - pmax(start1,start2)<0,0,pmin(stop1,stop2) - pmax(start1,start2)))
  }
  # function to assign event index to the correct age intervals
  event_within_interval = function(start,stop,event.age,event.ind){
    if(sum(stop<=start) >0) stop('Stop time must be greater than start times')
    ind = ifelse(event.ind==0,0,ifelse(event.age>=start & event.age<stop,1,0))
  }
  # function to split person-time
  split_time = function(df.ir,cut.offs,event){
    if(cut.offs[1] != 18) cut.offs = c(18, cut.offs)
    if(cut.offs[length(cut.offs)] != Inf) cut.offs = c(cut.offs,Inf)
    n = length(cut.offs)
    for (i in 1:(n-1)) {
      ptname = paste0("ir.pt",cut.offs[i],"_",cut.offs[i+1])
      eventname = paste0("ir.event",cut.offs[i],"_",cut.offs[i+1])
      df.ir[[ptname]] = with(df.ir, overlap_time(agestart,agestop,cut.offs[i],cut.offs[i+1]))
      df.ir[[eventname]] = with(df.ir, event_within_interval(cut.offs[i],cut.offs[i+1],agestop,get(event)))
    }
    return(df.ir)
  }
  # split follow-up time
  df.ir = split_time(df.ir,cut.offs,event)
  if(!is.null(weight.var)){
    df.ir = df.ir %>% mutate_at(.funs = funs(. * get(weight.var)), .cols = vars(starts_with("ir.")))
  }
  # summarize 
  df.sum =  df.ir %>%
    summarise_at(vars(starts_with('ir.')), sum)
  # wide to long
  df.final = df.sum %>% 
    select(starts_with("ir.pt")) %>% 
    pivot_longer(
      cols = starts_with("ir.pt"),
      names_to = "Group",
      names_prefix = "ir.pt",
      values_to = "pt",
      values_drop_na = TRUE) %>% 
    full_join(df.sum %>% 
                select(starts_with("ir.event")) %>% 
                pivot_longer(
                  cols = starts_with("ir.event"),
                  names_to = "Group",
                  names_prefix = "ir.event",
                  values_to = "n",
                  values_drop_na = TRUE),by = join_by(Group))
  # calculate IR and SE
  df.final = df.final %>% 
    mutate(Group = sub('\\_', '-', Group)) %>%
    mutate(Group = sub('\\-Inf', '+', Group)) %>%
    mutate(IR = n/pt, SE = sqrt(n)/pt)
  return(df.final)
}

cox.mortality = function(df,exposure,timescale = 'time',weight.var){
  #res = list()
  # set covariates
  if(timescale=='time'){
    covariates = c('race.eth','sex','baseline.age')
  }
  else if(timescale=='age'){
    covariates = c('race.eth','sex')
  }
  # in case exposure is age, sex, or race
  if(exposure %in% covariates) covariates = setdiff(covariates,exposure)
  # Cox model
  if(timescale=='time'){
    mod = coxph(as.formula(paste0('Surv(tstart,tstop,death) ~',paste0(c(exposure,covariates),collapse = '+'))), weights = df[[weight.var]], data = df)
  }
  else if(timescale=='age'){
    mod = coxph(as.formula(paste0('Surv(agestart,agestop,death) ~',paste0(c(exposure,covariates),collapse = '+'))), weights = df[[weight.var]], data = df)
  }
  #res$model = mod
  # extract summary stat of the cox model
  mod.sum = bind_cols(level=summary(mod)$coef%>%rownames(),summary(mod)$coef,exp(confint(mod)))
  # exposure only
  mod.sum = mod.sum %>% filter(str_detect(level, paste0("^",exposure)))
  mod.sum = bind_cols(Exposure = exposure,mod.sum)
  
  
  if(is.numeric(df %>% dplyr::select(exposure) %>% unlist())){
    stat = df %>% summarise(n = n(),nmor = sum(death)) %>% mutate(level = exposure) %>% select(level, n, nmor)
  } else{
    stat = df %>% group_by_at(exposure) %>% summarise(n = n(),nmor = sum(death)) %>% select(level = exposure, n, nmor)
    stat = stat %>% mutate(level = paste0(exposure,level))
  }
  
  mod.sum = mod.sum %>% left_join(stat,by = 'level')
  names(mod.sum) = c("Exposure","level","logHR","HR","SElogHR","robust se","z_value","p_value","lci","uci","N","Death")
  #res$summary = mod.sum
  #return(res)
  return(mod.sum)
}

cv.grplasso = function(x, y, index, weights, model = LogReg(), nfolds=5, foldid=NULL, nlambda=40) {
  if(is.null(foldid)) {
    foldid = sample(rep(seq(nfolds), length = length(y)))
    
  }
  lambda.max = lambdamax(x, y, index = index, weights = weights,
                         model = model)
  lambda.min = 0.001 * lambda.max 
  
  #lambda=exp(seq(log(lambda.max), log(lambda.min), length.out = nlambda))
  #lambda = seq(lambda.max, lambda.min, length.out = nlambda)
  #lambda=exp(exp(seq(log(log(lambda.max)), log(log(lambda.min)), length.out = nlambda)))
  lambda=exp(exp(seq(log(log(lambda.max)), log(log(lambda.min)), length.out = nlambda)))
  
  # Fit the solution path on the lambda grid
  se=function(x) sqrt(var(x)/length(x)) 
  pred.mat = matrix(NA, ncol=length(lambda),nrow=length(y))
  
  for (i in seq(nfolds)) {
    print(paste(i,"th fold",sep=""))
    which = foldid == i
    y_sub = y[!which]
    weights_sub = weights[!which]
    
    fit = grplasso(x =x[!which, , drop = FALSE], y = y_sub, index = index, weights = weights_sub, lambda = lambda, model = model)
    pred.mat[which,]= predict(fit, x[which,,drop=FALSE],type='response')
    
  }  
  
  acc = sapply(1:length(lambda), function(curi) { 
    acc=mean(((y==0)&(pred.mat[,curi]<0.129389649022329)) | ((y==1)&(pred.mat[,curi]>0.129389649022329)))
    acc
  }) # 0.129389649022329 is the weighted prob for being selected into the cohort_AoU
  # calculated by: sum(df.merge$cohort_AoU*df.merge$survey.weight.down)/sum(df.merge$survey.weight.down)
  acc2= data.frame(acc=acc)
  acc2$lambda = lambda
  
  id=which.max(acc2$acc)
  lambda.min = acc2$lambda[id]
  
  test = grplasso(x =x, y=y, index=index, weights = weights, lambda=lambda.min,model=model)
  
  list(lambda=lambda, acc=acc2$acc, grplasso.fit=test, lambda.min=lambda.min, foldid=foldid)  
}


# Load AoU and NHANES raw and imputed data

name_of_file_in_bucket <- 'mi_list_AoU.RDS'
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ", my_bucket, "/AoU_comparison/", name_of_file_in_bucket, " ."), intern=T)
mi.list.AoU  <- readRDS(name_of_file_in_bucket)

name_of_file_in_bucket <- 'mi_list_NHANES.RDS'
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ", my_bucket, "/AoU_comparison/", name_of_file_in_bucket, " ."), intern=T)
mi.list.NHANES  <- readRDS(name_of_file_in_bucket)
# Extract covariate balance, propensity scores, weighted Table 1, mortality rates, and Cox models over imputations

extract_results = function(i,model = c('basic','basic_int','demo','demo_int','health','health_int','clinical','clinical_int','lasso','lasso_int','rfsrc')){
  print(paste0('Start of model: ',model,"; iteration: ",i))
  
  # merge two data
  df.merge = mi.list.NHANES[[i]] %>% mutate(cohort = 'NHANES') %>%
    bind_rows(mi.list.AoU[[i]] %>% mutate(interview.yr = as.factor(interview.yr)) %>% mutate(cohort = 'AoU'))
  
  # make factors
  df.merge = df.merge %>%
    mutate(across(where(is.character), as.factor))
  
  # create cohort indicator
  df.merge = df.merge %>% 
    mutate(cohort_AoU = if_else(cohort == 'AoU',1,0)) 
  
  # Variable balance
  SMD_before = bal.tab(df.merge[,c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes')], 
                       treat = df.merge$cohort_AoU,
                       weights = df.merge$survey.weight,
                       continuous = 'std',
                       binary = 'std')
  
  # down-sampling zeros (NHANES) using minimal survey weight
  sum.weight = df.merge %>% filter(cohort == 'NHANES') %>% summarise(min = min(survey.weight),mean = mean(survey.weight),median = median(survey.weight))
  
  df.merge = df.merge %>% 
    mutate(survey.weight.down = if_else(cohort == 'AoU',survey.weight,survey.weight/(sum.weight$min))) 
  
  # survey design (prepared for weighted logistic regression- glm results don't make sense maybe due to weighting definition)
  df.merge.svy = svydesign(ids = ~1, weights = ~survey.weight.down, data=df.merge)
  
  if(model=='basic'){
    ps.model = svyglm(cohort_AoU ~ baseline.age+sex+race.eth, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='basic_int'){
    ps.model = svyglm(cohort_AoU ~ (baseline.age+sex+race.eth)^2, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='demo'){
    ps.model = svyglm(cohort_AoU ~ baseline.age+sex+race.eth+education+income+marital.status+birth.country, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='demo_int'){
    ps.model = svyglm(cohort_AoU ~ (baseline.age+sex+race.eth+education+income+marital.status+birth.country)^2, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='health'){
    ps.model = svyglm(cohort_AoU ~ baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='health_int'){
    ps.model = svyglm(cohort_AoU ~ (baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health)^2, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='clinical'){
    ps.model = svyglm(cohort_AoU ~ baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health+hypertension+diabetes+chd+cancer, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='clinical_int'){
    ps.model = svyglm(cohort_AoU ~ (baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health+hypertension+diabetes+chd+cancer)^2, df.merge.svy, family = "binomial")
    ps.model.coef = data.frame(var = ps.model$coefficients %>% names()) %>% bind_cols(coef=ps.model$coefficients)
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predict(ps.model,type = 'response') %>% as.vector)
  } else if(model=='lasso'){
    # dummy variables
    x = model.matrix(cohort_AoU ~ baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health+asthma+chd+congestive.heart.failure+heart.attack+cancer+hypertension+stroke+diabetes, data = df.merge)
    # y
    y = df.merge$cohort_AoU
    # weight
    w = df.merge$survey.weight.down
    # variable names
    var.name = colnames(x) %>% substr(.,1,4) %>% gsub("[A-Z]", "", .) 
    # group index
    var.number = sapply(seq_along(var.name), function(i) length(unique(var.name[1:i])))
    # cross validation
    mod = cv.grplasso(x=x,y=y,index=var.number,weights=w)
    # coef of best model
    coef_grplasso = mod$grplasso.fit$coefficients
    ps.model.coef = data.frame(var = mod$grplasso.fit$coefficients %>% row.names()) %>% bind_cols(coef=mod$grplasso.fit$coefficients %>% as.vector)
    # prediction of Xb
    xb = x %*% coef_grplasso %>% as.vector()
    # prediction of prob
    prob = exp(xb)/(exp(xb)+1)
    # prediction from lasso model
    df.merge = df.merge %>% 
      mutate(ps = prob)
    # save i to monitor progress
    my_dataframe <- data.frame(index=i)
    destination_filename <- 'lasso_i.RDS'
    saveRDS(my_dataframe, destination_filename)
    
  } else if(model=='lasso_int'){
    # dummy variables
    x = model.matrix(cohort_AoU ~ (baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health+asthma+chd+congestive.heart.failure+heart.attack+cancer+hypertension+stroke+diabetes)^2, data = df.merge)
    # y
    y = df.merge$cohort_AoU
    # weight
    w = df.merge$survey.weight.down
    # function to extract var names with interactions
    extract_var_name = function(x) {
      if (str_detect(x, ":")) {
        result <- paste0(str_sub(x, 1, 4),str_extract(x, ":.{4}"))
      } else {
        result <- str_sub(x, 1, 4)
      }
      return(result)
    }
    # variable names
    var.name = colnames(x) %>% sapply(extract_var_name) %>% gsub("[A-Z]", "", .) 
    # group index
    var.number = sapply(seq_along(var.name), function(i) length(unique(var.name[1:i])))
    # cross validation
    mod = cv.grplasso(x=x,y=y,index=var.number,weights=w)
    # coef of best model
    coef_grplasso = mod$grplasso.fit$coefficients
    ps.model.coef = data.frame(var = mod$grplasso.fit$coefficients %>% row.names()) %>% bind_cols(coef=mod$grplasso.fit$coefficients %>% as.vector)
    # prediction of Xb
    xb = x %*% coef_grplasso %>% as.vector()
    # prediction of prob
    prob = exp(xb)/(exp(xb)+1)
    # prediction from lasso model
    df.merge = df.merge %>% 
      mutate(ps = prob)
    # save i to monitor progress
    my_dataframe <- data.frame(index=i)
    destination_filename <- 'lasso_i.RDS'
    saveRDS(my_dataframe, destination_filename)
    
  } else if(model == 'rfsrc'){
    rf_model = rfsrc(formula = cohort_AoU ~ baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+health.insurance+general.health+asthma+chd+congestive.heart.failure+heart.attack+cancer+hypertension+stroke+diabetes,        # Outcome ~ predictors
                     data = df.merge,                        # Data frame
                     case.wt = df.merge$survey.weight.down,  # Survey weights
                     ntree = 500,                            # Number of trees
                     importance = TRUE,                      # Measure variable importance
                     family = "class"                        # Classification for binary outcome
    )
    
    
    ps.model.coef = data.frame(var = names(rf_model$importance),Importance = rf_model$importance)
    # prediction
    predictions = predict(rf_model, newdata = df.merge)$predicted 
    # prediction from logistic model
    df.merge = df.merge %>% 
      mutate(ps = predictions %>% as.vector)
    # save i to monitor progress
    my_dataframe <- data.frame(index=i)
    destination_filename <- 'rfsrc_i.RDS'
    saveRDS(my_dataframe, destination_filename)
    
  } else {
    print("Model must be one of c('basic','basic_int','demo','demo_int','health','health_int','clinical','clinical_int','lasso','lasso_int','rfsrc').")
  }    
  
  
  # trim
  trim_up = quantile(df.merge %>% filter(ps<1) %>% pull(ps),0.99)
  trim_down = quantile(df.merge %>% filter(ps>0) %>% pull(ps),0.01)      
  df.merge = df.merge %>%
    mutate(ps_untrim = ps) %>%
    mutate(ps = if_else(ps>trim_up,trim_up,if_else(ps<trim_down,trim_down,ps)))
  
  # only assign propensity score to AoU
  df.merge = df.merge %>% 
    mutate(ps_AoU = if_else(cohort_AoU==1,ps,1))
  
  # ps to weights
  weight_sum = df.merge %>% group_by(cohort) %>% summarise(weight_sum = sum(survey.weight.down))
  prob_AoU = weight_sum %>% filter(cohort =='AoU') %>% .$weight_sum / sum(weight_sum$weight_sum)
  df.merge = df.merge %>% 
    mutate(inv_stabilized_weight_AoU = if_else(cohort_AoU==1,prob_AoU/ps_AoU,1)) %>%
    mutate(inv_stabilized_odds_AoU = if_else(cohort_AoU==1,(prob_AoU/(1-prob_AoU))/(ps_AoU/(1-ps_AoU)),1))
  
  # multiply weights
  df.merge = df.merge %>% 
    mutate(final_weight_IPTW = survey.weight * inv_stabilized_weight_AoU) %>%
    mutate(final_weight_IOSW = survey.weight * inv_stabilized_odds_AoU)
  
  # smaller df to save data                      
  df.ps = df.merge %>% select(cohort,ps,ps_untrim,inv_stabilized_weight_AoU,inv_stabilized_odds_AoU,survey.weight,final_weight_IPTW,final_weight_IOSW)
  
  
  SMD_after_IPTW = bal.tab(df.merge[,c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes')], 
                           treat = df.merge$cohort_AoU,
                           weights = df.merge$final_weight_IPTW,
                           continuous = 'std',
                           binary = 'std')
  
  SMD_after_IOSW = bal.tab(df.merge[,c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes')], 
                           treat = df.merge$cohort_AoU,
                           weights = df.merge$final_weight_IOSW,
                           continuous = 'std',
                           binary = 'std')
  
  df.ehr.weighted = df.merge %>% filter(cohort_AoU==1) %>% mutate(unweight = 1)
  df.NHANES.imp = mi.list.NHANES[[i]] %>% mutate(unweight = 1)
  
  # Weighted Table 1
  table1_AoU_unweighted    = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes'),df.ehr.weighted,'unweight') 
  table1_AoU_weighted_IPTW = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes'),df.ehr.weighted,'final_weight_IPTW') 
  table1_AoU_weighted_IOSW = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes'),df.ehr.weighted,'final_weight_IOSW') 
  table1_NHANES_weighted   = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes'),df.NHANES.imp,'survey.weight') 
  table1_NHANES_unweighted = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes'),df.NHANES.imp,'unweight') 
  
  # Mortality rate
  IR_AoU_weighted_IPTW = age_specific_IR(df.ehr.weighted,c(18,30,40,50,60,70,80),'death',weight.var = 'final_weight_IPTW')
  IR_AoU_weighted_IOSW = age_specific_IR(df.ehr.weighted,c(18,30,40,50,60,70,80),'death',weight.var = 'final_weight_IOSW')
  
  # Cox model
  mod.AoU.unweighted.baseline.age = cox.mortality(df.ehr.weighted %>% mutate(baseline.age = baseline.age/10),'baseline.age',weight.var = 'unweight')
  mod.AoU.unweighted.race.eth = cox.mortality(df.ehr.weighted,'race.eth',weight.var = 'unweight')
  mod.AoU.unweighted.sex = cox.mortality(df.ehr.weighted,'sex',weight.var = 'unweight')
  mod.AoU.unweighted.education = cox.mortality(df.ehr.weighted,'education',weight.var = 'unweight')
  mod.AoU.unweighted.income = cox.mortality(df.ehr.weighted,'income',weight.var = 'unweight')
  mod.AoU.unweighted.marital.status = cox.mortality(df.ehr.weighted,'marital.status',weight.var = 'unweight')
  mod.AoU.unweighted.birth.country = cox.mortality(df.ehr.weighted,'birth.country',weight.var = 'unweight')
  mod.AoU.unweighted.smoking = cox.mortality(df.ehr.weighted,'smoking',weight.var = 'unweight')
  mod.AoU.unweighted.alcohol = cox.mortality(df.ehr.weighted,'alcohol',weight.var = 'unweight')                                      
  mod.AoU.unweighted.health.insurance = cox.mortality(df.ehr.weighted,'health.insurance',weight.var = 'unweight')
  mod.AoU.unweighted.general.health = cox.mortality(df.ehr.weighted,'general.health',weight.var = 'unweight')
  mod.AoU.unweighted.asthma = cox.mortality(df.ehr.weighted,'asthma',weight.var = 'unweight')
  mod.AoU.unweighted.chd = cox.mortality(df.ehr.weighted,'chd',weight.var = 'unweight')
  mod.AoU.unweighted.cancer = cox.mortality(df.ehr.weighted,'cancer',weight.var = 'unweight')
  mod.AoU.unweighted.congestive.heart.failure = cox.mortality(df.ehr.weighted,'congestive.heart.failure',weight.var = 'unweight')
  mod.AoU.unweighted.heart.attack = cox.mortality(df.ehr.weighted,'heart.attack',weight.var = 'unweight')
  mod.AoU.unweighted.hypertension = cox.mortality(df.ehr.weighted,'hypertension',weight.var = 'unweight')
  mod.AoU.unweighted.obesity = cox.mortality(df.ehr.weighted,'obesity',weight.var = 'unweight')
  mod.AoU.unweighted.stroke = cox.mortality(df.ehr.weighted,'stroke',weight.var = 'unweight')
  mod.AoU.unweighted.diabetes = cox.mortality(df.ehr.weighted,'diabetes',weight.var = 'unweight')
  
  mod.AoU.IPTW.baseline.age = cox.mortality(df.ehr.weighted %>% mutate(baseline.age = baseline.age/10),'baseline.age',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.race.eth = cox.mortality(df.ehr.weighted,'race.eth',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.sex = cox.mortality(df.ehr.weighted,'sex',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.education = cox.mortality(df.ehr.weighted,'education',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.income = cox.mortality(df.ehr.weighted,'income',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.marital.status = cox.mortality(df.ehr.weighted,'marital.status',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.birth.country = cox.mortality(df.ehr.weighted,'birth.country',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.smoking = cox.mortality(df.ehr.weighted,'smoking',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.alcohol = cox.mortality(df.ehr.weighted,'alcohol',weight.var = 'final_weight_IPTW')                                      
  mod.AoU.IPTW.health.insurance = cox.mortality(df.ehr.weighted,'health.insurance',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.general.health = cox.mortality(df.ehr.weighted,'general.health',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.asthma = cox.mortality(df.ehr.weighted,'asthma',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.chd = cox.mortality(df.ehr.weighted,'chd',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.cancer = cox.mortality(df.ehr.weighted,'cancer',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.congestive.heart.failure = cox.mortality(df.ehr.weighted,'congestive.heart.failure',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.heart.attack = cox.mortality(df.ehr.weighted,'heart.attack',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.hypertension = cox.mortality(df.ehr.weighted,'hypertension',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.obesity = cox.mortality(df.ehr.weighted,'obesity',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.stroke = cox.mortality(df.ehr.weighted,'stroke',weight.var = 'final_weight_IPTW')
  mod.AoU.IPTW.diabetes = cox.mortality(df.ehr.weighted,'diabetes',weight.var = 'final_weight_IPTW')
  
  mod.AoU.IOSW.baseline.age = cox.mortality(df.ehr.weighted %>% mutate(baseline.age = baseline.age/10),'baseline.age',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.race.eth = cox.mortality(df.ehr.weighted,'race.eth',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.sex = cox.mortality(df.ehr.weighted,'sex',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.education = cox.mortality(df.ehr.weighted,'education',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.income = cox.mortality(df.ehr.weighted,'income',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.marital.status = cox.mortality(df.ehr.weighted,'marital.status',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.birth.country = cox.mortality(df.ehr.weighted,'birth.country',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.smoking = cox.mortality(df.ehr.weighted,'smoking',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.alcohol = cox.mortality(df.ehr.weighted,'alcohol',weight.var = 'final_weight_IOSW')                                      
  mod.AoU.IOSW.health.insurance = cox.mortality(df.ehr.weighted,'health.insurance',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.general.health = cox.mortality(df.ehr.weighted,'general.health',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.asthma = cox.mortality(df.ehr.weighted,'asthma',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.chd = cox.mortality(df.ehr.weighted,'chd',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.cancer = cox.mortality(df.ehr.weighted,'cancer',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.congestive.heart.failure = cox.mortality(df.ehr.weighted,'congestive.heart.failure',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.heart.attack = cox.mortality(df.ehr.weighted,'heart.attack',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.hypertension = cox.mortality(df.ehr.weighted,'hypertension',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.obesity = cox.mortality(df.ehr.weighted,'obesity',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.stroke = cox.mortality(df.ehr.weighted,'stroke',weight.var = 'final_weight_IOSW')
  mod.AoU.IOSW.diabetes = cox.mortality(df.ehr.weighted,'diabetes',weight.var = 'final_weight_IOSW')
  
  mod.NHANES.baseline.age = cox.mortality(df.NHANES.imp %>% mutate(baseline.age = baseline.age/10),'baseline.age',weight.var = 'survey.weight')
  mod.NHANES.race.eth = cox.mortality(df.NHANES.imp,'race.eth',weight.var = 'survey.weight')
  mod.NHANES.sex = cox.mortality(df.NHANES.imp,'sex',weight.var = 'survey.weight')
  mod.NHANES.education = cox.mortality(df.NHANES.imp,'education',weight.var = 'survey.weight')
  mod.NHANES.income = cox.mortality(df.NHANES.imp,'income',weight.var = 'survey.weight')
  mod.NHANES.marital.status = cox.mortality(df.NHANES.imp,'marital.status',weight.var = 'survey.weight')
  mod.NHANES.birth.country = cox.mortality(df.NHANES.imp,'birth.country',weight.var = 'survey.weight')
  mod.NHANES.smoking = cox.mortality(df.NHANES.imp,'smoking',weight.var = 'survey.weight')
  mod.NHANES.alcohol = cox.mortality(df.NHANES.imp,'alcohol',weight.var = 'survey.weight')                                      
  mod.NHANES.health.insurance = cox.mortality(df.NHANES.imp,'health.insurance',weight.var = 'survey.weight')
  mod.NHANES.general.health = cox.mortality(df.NHANES.imp,'general.health',weight.var = 'survey.weight')
  mod.NHANES.asthma = cox.mortality(df.NHANES.imp,'asthma',weight.var = 'survey.weight')
  mod.NHANES.chd = cox.mortality(df.NHANES.imp,'chd',weight.var = 'survey.weight')
  mod.NHANES.cancer = cox.mortality(df.NHANES.imp,'cancer',weight.var = 'survey.weight')
  mod.NHANES.congestive.heart.failure = cox.mortality(df.NHANES.imp,'congestive.heart.failure',weight.var = 'survey.weight')
  mod.NHANES.heart.attack = cox.mortality(df.NHANES.imp,'heart.attack',weight.var = 'survey.weight')
  mod.NHANES.hypertension = cox.mortality(df.NHANES.imp,'hypertension',weight.var = 'survey.weight')
  mod.NHANES.obesity = cox.mortality(df.NHANES.imp,'obesity',weight.var = 'survey.weight')
  mod.NHANES.stroke = cox.mortality(df.NHANES.imp,'stroke',weight.var = 'survey.weight')
  mod.NHANES.diabetes = cox.mortality(df.NHANES.imp,'diabetes',weight.var = 'survey.weight')
  
  return(
    list(
      #ps.model=ps.model,
      ps.model.coef=ps.model.coef,
      df.ps=df.ps,
      
      SMD_before=SMD_before,
      SMD_after_IPTW=SMD_after_IPTW,
      SMD_after_IOSW=SMD_after_IOSW,
      
      table1_AoU_unweighted = table1_AoU_unweighted,
      table1_AoU_weighted_IPTW = table1_AoU_weighted_IPTW,
      table1_AoU_weighted_IOSW = table1_AoU_weighted_IOSW,
      table1_NHANES_weighted = table1_NHANES_weighted,
      table1_NHANES_unweighted = table1_NHANES_unweighted,
      
      IR_AoU_weighted_IPTW = IR_AoU_weighted_IPTW,
      IR_AoU_weighted_IOSW = IR_AoU_weighted_IOSW,
      
      mod.AoU.unweighted.baseline.age = mod.AoU.unweighted.baseline.age,
      mod.AoU.unweighted.race.eth = mod.AoU.unweighted.race.eth,
      mod.AoU.unweighted.sex = mod.AoU.unweighted.sex,
      mod.AoU.unweighted.education = mod.AoU.unweighted.education,
      mod.AoU.unweighted.income = mod.AoU.unweighted.income,
      mod.AoU.unweighted.marital.status = mod.AoU.unweighted.marital.status,
      mod.AoU.unweighted.birth.country = mod.AoU.unweighted.birth.country,
      mod.AoU.unweighted.smoking = mod.AoU.unweighted.smoking,
      mod.AoU.unweighted.alcohol = mod.AoU.unweighted.alcohol,
      mod.AoU.unweighted.health.insurance = mod.AoU.unweighted.health.insurance,
      mod.AoU.unweighted.general.health = mod.AoU.unweighted.general.health,
      mod.AoU.unweighted.asthma = mod.AoU.unweighted.asthma,
      mod.AoU.unweighted.chd = mod.AoU.unweighted.chd,
      mod.AoU.unweighted.cancer = mod.AoU.unweighted.cancer,
      mod.AoU.unweighted.congestive.heart.failure = mod.AoU.unweighted.congestive.heart.failure,
      mod.AoU.unweighted.heart.attack = mod.AoU.unweighted.heart.attack,
      mod.AoU.unweighted.hypertension = mod.AoU.unweighted.hypertension,
      mod.AoU.unweighted.obesity = mod.AoU.unweighted.obesity,
      mod.AoU.unweighted.stroke = mod.AoU.unweighted.stroke,
      mod.AoU.unweighted.diabetes = mod.AoU.unweighted.diabetes,
      
      mod.AoU.IPTW.baseline.age = mod.AoU.IPTW.baseline.age,
      mod.AoU.IPTW.race.eth = mod.AoU.IPTW.race.eth,
      mod.AoU.IPTW.sex = mod.AoU.IPTW.sex,
      mod.AoU.IPTW.education = mod.AoU.IPTW.education,
      mod.AoU.IPTW.income = mod.AoU.IPTW.income,
      mod.AoU.IPTW.marital.status = mod.AoU.IPTW.marital.status,
      mod.AoU.IPTW.birth.country = mod.AoU.IPTW.birth.country,
      mod.AoU.IPTW.smoking = mod.AoU.IPTW.smoking,
      mod.AoU.IPTW.alcohol = mod.AoU.IPTW.alcohol,
      mod.AoU.IPTW.health.insurance = mod.AoU.IPTW.health.insurance,
      mod.AoU.IPTW.general.health = mod.AoU.IPTW.general.health,
      mod.AoU.IPTW.asthma = mod.AoU.IPTW.asthma,
      mod.AoU.IPTW.chd = mod.AoU.IPTW.chd,
      mod.AoU.IPTW.cancer = mod.AoU.IPTW.cancer,
      mod.AoU.IPTW.congestive.heart.failure = mod.AoU.IPTW.congestive.heart.failure,
      mod.AoU.IPTW.heart.attack = mod.AoU.IPTW.heart.attack,
      mod.AoU.IPTW.hypertension = mod.AoU.IPTW.hypertension,
      mod.AoU.IPTW.obesity = mod.AoU.IPTW.obesity,
      mod.AoU.IPTW.stroke = mod.AoU.IPTW.stroke,
      mod.AoU.IPTW.diabetes = mod.AoU.IPTW.diabetes,
      
      mod.AoU.IOSW.baseline.age = mod.AoU.IOSW.baseline.age,
      mod.AoU.IOSW.race.eth = mod.AoU.IOSW.race.eth,
      mod.AoU.IOSW.sex = mod.AoU.IOSW.sex,
      mod.AoU.IOSW.education = mod.AoU.IOSW.education,
      mod.AoU.IOSW.income = mod.AoU.IOSW.income,
      mod.AoU.IOSW.marital.status = mod.AoU.IOSW.marital.status,
      mod.AoU.IOSW.birth.country = mod.AoU.IOSW.birth.country,
      mod.AoU.IOSW.smoking = mod.AoU.IOSW.smoking,
      mod.AoU.IOSW.alcohol = mod.AoU.IOSW.alcohol,
      mod.AoU.IOSW.health.insurance = mod.AoU.IOSW.health.insurance,
      mod.AoU.IOSW.general.health = mod.AoU.IOSW.general.health,
      mod.AoU.IOSW.asthma = mod.AoU.IOSW.asthma,
      mod.AoU.IOSW.chd = mod.AoU.IOSW.chd,
      mod.AoU.IOSW.cancer = mod.AoU.IOSW.cancer,
      mod.AoU.IOSW.congestive.heart.failure = mod.AoU.IOSW.congestive.heart.failure,
      mod.AoU.IOSW.heart.attack = mod.AoU.IOSW.heart.attack,
      mod.AoU.IOSW.hypertension = mod.AoU.IOSW.hypertension,
      mod.AoU.IOSW.obesity = mod.AoU.IOSW.obesity,
      mod.AoU.IOSW.stroke = mod.AoU.IOSW.stroke,
      mod.AoU.IOSW.diabetes = mod.AoU.IOSW.diabetes,
      
      mod.NHANES.baseline.age = mod.NHANES.baseline.age,
      mod.NHANES.race.eth = mod.NHANES.race.eth,
      mod.NHANES.sex = mod.NHANES.sex,
      mod.NHANES.education = mod.NHANES.education,
      mod.NHANES.income = mod.NHANES.income,
      mod.NHANES.marital.status = mod.NHANES.marital.status,
      mod.NHANES.birth.country = mod.NHANES.birth.country,
      mod.NHANES.smoking = mod.NHANES.smoking,
      mod.NHANES.alcohol = mod.NHANES.alcohol,
      mod.NHANES.health.insurance = mod.NHANES.health.insurance,
      mod.NHANES.general.health = mod.NHANES.general.health,
      mod.NHANES.asthma = mod.NHANES.asthma,
      mod.NHANES.chd = mod.NHANES.chd,
      mod.NHANES.cancer = mod.NHANES.cancer,
      mod.NHANES.congestive.heart.failure = mod.NHANES.congestive.heart.failure,
      mod.NHANES.heart.attack = mod.NHANES.heart.attack,
      mod.NHANES.hypertension = mod.NHANES.hypertension,
      mod.NHANES.obesity = mod.NHANES.obesity,
      mod.NHANES.stroke = mod.NHANES.stroke,
      mod.NHANES.diabetes = mod.NHANES.diabetes))
}
##############################################################################
# 10 models
##############################################################################

res_MI_basic = lapply(1:40, extract_results, model = 'basic')
# Save results
my_dataframe <- res_MI_basic
destination_filename <- 'res_MI_basic_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_basic_int = lapply(1:40, extract_results, model = 'basic_int')
# Save results
my_dataframe <- res_MI_basic_int
destination_filename <- 'res_MI_basic_int_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_demo = lapply(1:40, extract_results, model = 'demo')
# Save results
my_dataframe <- res_MI_demo
destination_filename <- 'res_MI_demo_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_demo_int = lapply(1:40, extract_results, model = 'demo_int')
# Save results
my_dataframe <- res_MI_demo_int
destination_filename <- 'res_MI_demo_int_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_health = lapply(1:40, extract_results, model = 'health')
# Save results
my_dataframe <- res_MI_health
destination_filename <- 'res_MI_health_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_health_int = lapply(1:40, extract_results, model = 'health_int')
# Save results
my_dataframe <- res_MI_health_int
destination_filename <- 'res_MI_health_int_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_clinical = lapply(1:40, extract_results, model = 'clinical')
# Save results
my_dataframe <- res_MI_clinical
destination_filename <- 'res_MI_clinical_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_clinical_int = lapply(1:40, extract_results, model = 'clinical_int')
# Save results
my_dataframe <- res_MI_clinical_int
destination_filename <- 'res_MI_clinical_int_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_lasso = lapply(1:40, extract_results, model = 'lasso')
# Save results
my_dataframe <- res_MI_lasso
destination_filename <- 'res_MI_lasso_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

# res_MI_lasso_int = lapply(1:40, extract_results, model = 'lasso_int')
# # Save results
# my_dataframe <- res_MI_lasso_int
# destination_filename <- 'res_MI_lasso_int_trim_newstroke.RDS'
# saveRDS(my_dataframe, destination_filename)
# my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
# system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

res_MI_rfsrc = lapply(1:40, extract_results, model = 'rfsrc')
# Save results
my_dataframe <- res_MI_rfsrc
destination_filename <- 'res_MI_rfsrc_trim_newstroke.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)