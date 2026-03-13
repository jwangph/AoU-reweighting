# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, survival,survey,cobalt,mice,ggplot2,viridis,table1,forestploter,grid,cowplot,tidyr,RColorBrewer)

# install.packages('export')
library(export)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Function to pool results from MI
pool_MI = function(res_MI,var_name){
  
  extract_coefficients = function(sublist,var_name) {
    if(grepl("unweighted", var_name, fixed=TRUE)){
      data.frame(level = sublist[[var_name]][,'level'] %>% unlist,
                 estimate = sublist[[var_name]][,'logHR'] %>% unlist,
                 SE = sublist[[var_name]][,'SElogHR'] %>% unlist,
                 var = sublist[[var_name]][,'SElogHR']^2 %>% unlist,
                 N = sublist[[var_name]][,'N'] %>% unlist)
    } else{
      data.frame(level = sublist[[var_name]][,'level'] %>% unlist,
                 estimate = sublist[[var_name]][,'logHR'] %>% unlist,
                 SE = sublist[[var_name]][,'robust se'] %>% unlist,
                 var = sublist[[var_name]][,'robust se']^2 %>% unlist,
                 N = sublist[[var_name]][,'N'] %>% unlist)
    }
  }
  
  pool_coefficients = function(sublist){
    res = pool.scalar(Q = sublist[,'estimate'], U = sublist[,'var'])
    return(within(res, rm(qhat,u)) %>% unlist)
  }
  
  temp.df = do.call(rbind, lapply(res_MI,extract_coefficients,var_name = var_name))
  temp.list = split(temp.df,temp.df$level)
  
  res = do.call(rbind, lapply(temp.list,pool_coefficients)) %>% as.data.frame
  res = res %>% 
    mutate(logHR = qbar, HR = exp(qbar),var = t, SElogHR = sqrt(t), lci = exp(qbar - 1.96*sqrt(t)),uci = exp(qbar + 1.96*sqrt(t))) %>% 
    mutate(Exposure = gsub(".*(S\\.|W\\.|ted\\.)(.*)", "\\2", var_name)) %>%
    mutate(level = rownames(res)) %>%
    mutate(level = gsub(Exposure,"",level)) %>%
    mutate(group = gsub("^.|.$", "", gsub(paste(Exposure,'mod',sep='|'),"",var_name))) %>%
    select(Exposure,level,group,logHR, HR,var, SElogHR, lci, uci) %>%
    mutate(across(where(is.character), as.factor))
  return(res)
}

# Function to clean df.meta
escalc_res = function(df.meta,AoU = 'IPTW'){
  
  exposure.list = df.meta %>% .$Exposure %>% unique()
  
  level.list = list(race.eth = c('Non-Hispanic Asian','Non-Hispanic Black','Hispanic','Other'),
                    sex = c('Female','Male'),
                    education = c('High school','Some college','College or above'),
                    income = c('Less than 10k','10-25k','25-35k','More than 75k'),
                    marital.status = c('Living with partner','Divorced','Separated','Widowed','Never married'),
                    smoking = c('Yes'),
                    alcohol = c('Yes'),
                    asthma = c('Yes'),
                    chd = c('Yes'),
                    cancer = c('Yes'),
                    congestive.heart.failure = c('Yes'),
                    heart.attack = c('Yes'),
                    hypertension = c('Yes'),
                    obesity = c('Yes'),
                    stroke = c('Yes'),
                    diabetes = c('Yes'))
  res = c()
  for (exp in exposure.list) {
    temp = df.meta %>% filter(Exposure==exp) 
    if(!is.null(level.list[[exp]])){
      temp = temp %>% mutate(level =  factor(level, levels = level.list[[exp]]))
    }
    temp = temp %>% 
      group_by(level) %>%
      arrange(group, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(level = as.character(level))
    
    if(AoU == 'IPTW'){
      temp.wide = temp %>% filter(group == 'NHANES') %>% select(Exposure, level, HR1 = HR, lci1=lci,uci1=uci,SElogHR1 = SElogHR) %>%
        full_join(temp %>% filter(group == 'AoU.IPTW') %>% select(Exposure, level, HR2 = HR, lci2=lci,uci2=uci,SElogHR2 = SElogHR),by = c('Exposure', 'level')) %>%
        full_join(temp %>% filter(group == 'AoU.unweighted') %>% select(Exposure, level, HR3 = HR, lci3=lci,uci3=uci,SElogHR3 = SElogHR),by = c('Exposure', 'level'))
      
    } else if(AoU == 'IOSW'){
      temp.wide = temp %>% filter(group == 'NHANES') %>% select(Exposure, level, HR1 = HR, lci1=lci,uci1=uci,SElogHR1 = SElogHR) %>%
        full_join(temp %>% filter(group == 'AoU.IOSW') %>% select(Exposure, level, HR2 = HR, lci2=lci,uci2=uci,SElogHR2 = SElogHR),by = c('Exposure', 'level')) %>%
        full_join(temp %>% filter(group == 'AoU.unweighted') %>% select(Exposure, level, HR3 = HR, lci3=lci,uci3=uci,SElogHR3 = SElogHR),by = c('Exposure', 'level'))
    }
    
    
    if(temp.wide$level[1] =='Yes'){
      res = res %>% bind_rows(data.frame(Subgroup = exp) %>% bind_rows(bind_cols(data.frame(Subgroup = exp),temp.wide)))
    } else{
      res = res %>% bind_rows(data.frame(Subgroup = exp) %>% bind_rows(temp.wide))
    }  
    
  }
  
  return(res)
  
}

# Make first character capital
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Read results

df.meta.plot1.list = df.meta.plot2.list = df.meta.plot3.list = list()
SMD_IOSW1.list = SMD_IOSW2.list = SMD_IOSW3.list = list()

for(model in c('basic','basic_int','demo','demo_int','health','health_int','clinical','clinical_int','lasso','rfsrc')){
  # Read MI results
  name_of_file_in_bucket <- paste0('res_MI_',model,'_trim_newstroke.RDS')
  #my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
  #system(paste0("gsutil cp ", my_bucket, "/AoU_HRS_comparison/", name_of_file_in_bucket, " ."), intern=T)
  res_MI  <- readRDS(name_of_file_in_bucket)
  
  
 
  # Pool MI results using Robin's rule
  res_MI_names = names(res_MI[[1]])
  mod_names = res_MI_names[grep("^mod", res_MI_names)]
  
  df.meta = do.call(rbind, lapply(mod_names,pool_MI,res_MI=res_MI)) %>% as.data.frame
  
  
  # Plot Cox results
  # Need to choose method: IPTW vs IOSW
  
  df.meta.plot = escalc_res(df.meta,AoU = 'IOSW') %>%
    mutate(Subgroup = ifelse(is.na(Subgroup),paste0("    ",level),Subgroup)) %>%
    mutate(Subgroup = firstup(Subgroup)) %>%
    mutate(`HR (95% CI)` = ifelse(!is.na(HR1),paste0(sprintf("%.2f (%.2f to %.2f)",HR1, lci1, uci1),' \n',sprintf("%.2f (%.2f to %.2f)",HR2, lci2, uci2),' \n',sprintf("%.2f (%.2f to %.2f)",HR3, lci3, uci3)),'')) 
  
  
  df.meta.plot = df.meta.plot %>% 
    mutate(Subgroup = if_else(!is.na(Exposure) & Exposure == 'baseline.age','Baseline age (per decade)',Subgroup)) %>%
    filter(!(Subgroup == 'Baseline.age' & is.na(Exposure))) %>%
    mutate(Subgroup = if_else(Subgroup == 'Race.eth','Race/ethnicity (ref. Non-Hispanic White)',Subgroup)) %>%
    mutate(Subgroup = if_else(Subgroup == '    Male','Male (vs. female)',Subgroup)) %>%
    filter(!Subgroup == 'Sex') %>%
    mutate(Subgroup = if_else(Subgroup == 'Education','Education (ref. less than high school)',Subgroup)) %>%
    mutate(Subgroup = if_else(Subgroup == 'Income','Income (ref. 35-75k)',Subgroup)) %>%
    mutate(Subgroup = if_else(Subgroup == 'Marital.status','Marital status (ref. married)',Subgroup)) %>%
    filter(!Subgroup == 'Birth.country') %>%
    mutate(Subgroup = if_else(Subgroup == '    Outside US','Born outside U.S.',Subgroup)) %>%
    filter(!(Subgroup == 'Smoking' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Alcohol' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Health.insurance' & is.na(Exposure))) %>%
    mutate(Subgroup = if_else(Subgroup == 'Health.insurance','Health insurance',Subgroup)) %>%
    mutate(Subgroup = if_else(Subgroup == 'General.health','General health (ref. excellent)',Subgroup)) %>%
    filter(!(Subgroup == 'Asthma' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Chd' & is.na(Exposure))) %>%
    mutate(Subgroup = if_else(Subgroup == 'Chd','Coronary heart disease',Subgroup)) %>%
    filter(!(Subgroup == 'Cancer' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Congestive.heart.failure' & is.na(Exposure))) %>%
    mutate(Subgroup = if_else(Subgroup == 'Congestive.heart.failure','Congestive heart failure',Subgroup)) %>%
    filter(!(Subgroup == 'Heart.attack' & is.na(Exposure))) %>%
    mutate(Subgroup = if_else(Subgroup == 'Heart.attack','Heart attack',Subgroup)) %>%
    filter(!(Subgroup == 'Hypertension' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Obesity' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Stroke' & is.na(Exposure))) %>%
    filter(!(Subgroup == 'Diabetes' & is.na(Exposure))) %>%
    mutate(Subgroup = if_else(Subgroup == 'Diabetes','Diabetes mellitus',Subgroup))
  
  df.meta.plot = df.meta.plot %>% 
    mutate(RHR2 = HR2/HR1, SElogRHR2 = sqrt(SElogHR2^2+SElogHR1^2), RHR3 = HR3/HR1, SElogRHR3 = sqrt(SElogHR3^2+SElogHR1^2)) %>%
    mutate(lciRHR2 = RHR2/exp(1.96*SElogRHR2),uciRHR2 = RHR2*exp(1.96*SElogRHR2),
           lciRHR3 = RHR3/exp(1.96*SElogRHR3),uciRHR3 = RHR3*exp(1.96*SElogRHR3))
  
  
  df.meta.plot = df.meta.plot %>% 
    mutate(`RHR (95% CI)` = ifelse(!is.na(HR1),paste0('',' \n',sprintf("%.2f (%.2f to %.2f)",RHR2, lciRHR2, uciRHR2),' \n',sprintf("%.2f (%.2f to %.2f)",RHR3, lciRHR3, uciRHR3)),'')) #%>%
  
  df.meta.plot = df.meta.plot %>% 
    mutate(Subgroup = ifelse(is.na(HR1),paste0('\n',Subgroup,'\n'),Subgroup)) 
  
  df.meta.plot$` ` = "                                                   "
  
  df.meta.plot$`  ` = "                                                   "
  
  
  df.meta.plot1 = df.meta.plot %>% slice(1:23)
  
  df.meta.plot2 = df.meta.plot %>% slice(24:31)
  
  df.meta.plot3 = df.meta.plot %>% slice(32:40)
  
  
  
  df.meta.plot1.list[[model]] = df.meta.plot1
  df.meta.plot2.list[[model]] = df.meta.plot2
  df.meta.plot3.list[[model]] = df.meta.plot3
  
  
  
  
  # Plot variable balance (mean over imputations)
  mean.bal.before = do.call(cbind, lapply(res_MI,function(x) x[['SMD_before']]$Balance$Diff.Adj)) %>% rowMeans()
  mean.bal.after.IOSW = do.call(cbind, lapply(res_MI,function(x) x[['SMD_after_IOSW']]$Balance$Diff.Adj)) %>% rowMeans()
  
  ## add SMD before weighting
  SMD = res_MI[[1]][['SMD_before']]
  SMD$Balance$Diff.Un = mean.bal.before
  SMD_IOSW = SMD         
  SMD_IOSW$Balance$Diff.Adj = mean.bal.after.IOSW
  
  ## Change variable name lables
  var.names = row.names(SMD_IOSW$Balance)
  var.names[var.names=='baseline.age'] = 'Baseline age'
  var.names[var.names=='sex_Male'] = 'Male'
  var.names[var.names=='race.eth_Non-Hispanic White'] = 'Race/ethnicity: Non-Hispanic White'
  var.names[var.names=='race.eth_Hispanic'] = 'Race/ethnicity: Hispanic'
  var.names[var.names=='race.eth_Non-Hispanic Asian'] = 'Race/ethnicity: Non-Hispanic Asian'
  var.names[var.names=='race.eth_Non-Hispanic Black'] = 'Race/ethnicity: Non-Hispanic Black'
  var.names[var.names=='race.eth_Other'] = 'Race/ethnicity: Other'
  var.names[var.names=='education_Less than high school'] = 'Education: Less than high school'
  var.names[var.names=='education_College or above'] = 'Education: College or above'
  var.names[var.names=='education_High school'] = 'Education: High school'
  var.names[var.names=='education_Some college'] = 'Education: Some college'
  var.names[var.names=='income_35-75k'] = 'Income: 35-75k'
  var.names[var.names=='income_10-25k'] = 'Income: 10-25k'
  var.names[var.names=='income_25-35k'] = 'Income: 25-35k'
  var.names[var.names=='income_More than 75k'] = 'Income: More than 75k'
  var.names[var.names=='income_Less than 10k'] = 'Income: Less than 10k'
  var.names[var.names=='marital.status_Married'] = 'Marital status: Married'
  var.names[var.names=='marital.status_Divorced'] = 'Marital status: Divorced'
  var.names[var.names=='marital.status_Living with partner'] = 'Marital status: Living with partner'
  var.names[var.names=='marital.status_Never married'] = 'Marital status: Never married'
  var.names[var.names=='marital.status_Separated'] = 'Marital status: Separated'
  var.names[var.names=='marital.status_Widowed'] = 'Marital status: Widowed'
  var.names[var.names=='birth.country_Outside US'] = 'Born outside U.S.'
  var.names[var.names=='smoking_Yes'] = 'Smoking'
  var.names[var.names=='alcohol_Yes'] = 'Alcohol'
  var.names[var.names=='health.insurance_Yes'] = 'Health insurance'
  var.names[var.names=='general.health_Excellent'] = 'General health: Excellent'
  var.names[var.names=='general.health_Fair'] = 'General health: Fair'
  var.names[var.names=='general.health_Good'] = 'General health: Good'
  var.names[var.names=='general.health_Poor'] = 'General health: Poor'
  var.names[var.names=='general.health_Very good'] = 'General health: Very good'
  var.names[var.names=='asthma_Yes'] = 'Asthma'
  var.names[var.names=='chd_Yes'] = 'Coronary heart disease'
  var.names[var.names=='congestive.heart.failure_Yes'] = 'Congestive heart failure'
  var.names[var.names=='heart.attack_Yes'] = 'Heart attack'
  var.names[var.names=='cancer_Yes'] = 'Cancer'
  var.names[var.names=='hypertension_Yes'] = 'Hypertension'
  var.names[var.names=='obesity_Yes'] = 'Obesity'
  var.names[var.names=='stroke_Yes'] = 'Stroke'
  var.names[var.names=='diabetes_Yes'] = 'Diabetes mellitus'
  
  ## Change orders
  var.order = order(match(var.names, c('Baseline age','Male',
                                       'Race/ethnicity: Non-Hispanic White','Race/ethnicity: Non-Hispanic Asian','Race/ethnicity: Non-Hispanic Black','Race/ethnicity: Hispanic','Race/ethnicity: Other',
                                       'Education: Less than high school','Education: High school','Education: Some college','Education: College or above',
                                       'Income: Less than 10k','Income: 10-25k','Income: 25-35k','Income: 35-75k','Income: More than 75k',
                                       'Marital status: Married','Marital status: Living with partner','Marital status: Divorced','Marital status: Separated','Marital status: Widowed','Marital status: Never married',
                                       'Born outside U.S.','Smoking','Alcohol','Health insurance',
                                       'General health: Excellent','General health: Very good','General health: Good','General health: Fair','General health: Poor',
                                       'Asthma','Coronary heart disease','Congestive heart failure','Heart attack','Cancer','Hypertension','Obesity','Stroke','Diabetes mellitus')))
  
  ## Change names and order
  row.names(SMD_IOSW$Balance) = var.names
  SMD_IOSW$Balance = SMD_IOSW$Balance[var.order,]
  
  SMD_IOSW1 = SMD_IOSW
  SMD_IOSW1$Balance = SMD_IOSW1$Balance[1:23,]
  SMD_IOSW2 = SMD_IOSW
  SMD_IOSW2$Balance = SMD_IOSW2$Balance[24:31,]
  SMD_IOSW3 = SMD_IOSW
  SMD_IOSW3$Balance = SMD_IOSW3$Balance[32:40,]
  
  
  
  SMD_IOSW1.list[[model]] = SMD_IOSW1
  SMD_IOSW2.list[[model]] = SMD_IOSW2
  SMD_IOSW3.list[[model]] = SMD_IOSW3
  
  
  
  # check lasso selection results
  if(model == 'lasso'){
    lasso_sel = lapply(1:40,function(i) res_MI[[i]]$ps.model.coef %>% filter(coef!=0) %>% pull(var) %>% str_extract(., "^[^A-Z]*") %>% unique %>% str_remove_all(., "\\("))
  }
}


##############################################################################
# Table 1
##############################################################################

# Pool Table 1
extract_count_and_percentage = function(table) {
  # Use a regular expression to extract the count and percentage
  var = table$variable
  count = as.numeric(sub("\\s*\\(.*", "", table$stat))
  table.new = data.frame(variable = table$variable, count = count)
  return(table.new)
}

extract_average_table1 = function(table_name){
  tab = res_MI[[1]][[table_name]] %>% select(variable) %>% bind_cols(count = do.call(bind_cols, lapply(res_MI,function(x) x[[table_name]] %>% extract_count_and_percentage %>% select('count'))) %>% rowMeans())
  age.sd = as.numeric(sub(".*\\((\\d+\\.?\\d*)\\).*", "\\1", res_MI[[1]][[table_name]][2,2]))
  total = tab %>% filter(variable %in% c("Female","Male")) %>% pull(count) %>% sum
  tab = tab %>% mutate(total = total) %>% mutate(prop = count/total, age.sd = age.sd)
  tab = tab %>% mutate(stat = if_else(is.na(count),'',if_else(variable == 'Mean',sprintf("%.1f (%.1f)",count,age.sd),sprintf("%.0f (%.1f%%)",count, prop*100))))
  tab = tab %>% bind_rows(data.frame(variable = 'Total',stat = as.character(total)))
  tab = tab %>% filter(variable != 'No' & variable != 'Female' & variable != 'sex' & variable != 'baseline.age' & variable != 'Outside US' & variable != 'birth.country')
  tab = tab %>% mutate(variable = if_else(variable == 'Mean','Baseline age',variable))
  tab = tab %>% mutate(variable = if_else(variable == 'Yes',NA,variable))
  tab = tab %>% fill(variable, .direction = "down")
  tab = tab %>% filter(!is.na(count) | variable %in% c('race.eth','education','income','marital.status','US','general.health','Total')) %>% select(variable,stat)
  ord = order(match(tab$variable, c('Baseline age','Male',
                                    'race.eth','Non-Hispanic White','Non-Hispanic Asian','Non-Hispanic Black','Hispanic','Other',
                                    'education','Less than high school','High school','Some college','College or above',
                                    'income','Less than 10k','10-25k','25-35k','35-75k','More than 75k',
                                    'marital.status','Married','Living with partner','Divorced','Separated','Widowed','Never married',
                                    'US','smoking','alcohol','health.insurance',
                                    'general.health','Excellent','Very good','Good','Fair','Poor',
                                    'asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes','Total')))
  tab = tab[ord,]
  names(tab)[2] = table_name
  return(tab)
}

tab1 = extract_average_table1('table1_NHANES_unweighted') %>%
  full_join(extract_average_table1('table1_NHANES_weighted')) %>%
  full_join(extract_average_table1('table1_AoU_unweighted')) %>%
  full_join(extract_average_table1('table1_AoU_weighted_IOSW')) 

write.csv(tab1,file = paste0('MI/',model,'_table1_newstroke.csv'))




##############################################################################
# Figure 1
##############################################################################

diff1 = SMD_IOSW1.list[[1]]$Balance %>% select(Type,Unadjusted=Diff.Un) %>%
  bind_cols(`Base` = SMD_IOSW1.list[[1]]$Balance$Diff.Adj) %>%
  bind_cols(`Base-interaction` = SMD_IOSW1.list[[2]]$Balance$Diff.Adj) %>%
  bind_cols(`Clinical` = SMD_IOSW1.list[[7]]$Balance$Diff.Adj) %>%
  bind_cols(`Clinical-interaction` = SMD_IOSW1.list[[8]]$Balance$Diff.Adj) %>%
  rownames_to_column(var = "Variable") %>%
  select(-Type) %>%
  pivot_longer(cols = -Variable, names_to = "Adjustment", values_to = "Difference") %>%
  mutate(Variable.ord = factor(Variable,levels=c('Baseline age','Male',
                                                 'Race/ethnicity: Non-Hispanic White','Race/ethnicity: Non-Hispanic Asian','Race/ethnicity: Non-Hispanic Black','Race/ethnicity: Hispanic','Race/ethnicity: Other',
                                                 'Education: Less than high school','Education: High school','Education: Some college','Education: College or above',
                                                 'Income: Less than 10k','Income: 10-25k','Income: 25-35k','Income: 35-75k','Income: More than 75k',
                                                 'Marital status: Married','Marital status: Living with partner','Marital status: Divorced','Marital status: Separated','Marital status: Widowed','Marital status: Never married',
                                                 'Born outside U.S.'))) %>%
  arrange(Variable.ord) %>%
  mutate(Adjustment = factor(Adjustment,levels=c('Unadjusted','Base','Base-interaction','Clinical','Clinical-interaction')))


diff2 = SMD_IOSW2.list[[1]]$Balance %>% select(Type,Unadjusted=Diff.Un) %>%
  bind_cols(`Base` = SMD_IOSW2.list[[1]]$Balance$Diff.Adj) %>%
  bind_cols(`Base-interaction` = SMD_IOSW2.list[[2]]$Balance$Diff.Adj) %>%
  bind_cols(`Clinical` = SMD_IOSW2.list[[7]]$Balance$Diff.Adj) %>%
  bind_cols(`Clinical-interaction` = SMD_IOSW2.list[[8]]$Balance$Diff.Adj) %>%
  rownames_to_column(var = "Variable") %>%
  select(-Type) %>%
  pivot_longer(cols = -Variable, names_to = "Adjustment", values_to = "Difference") %>%
  mutate(Variable.ord = factor(Variable,levels=c('Smoking','Alcohol','Health insurance',
                                                 'General health: Excellent','General health: Very good','General health: Good','General health: Fair','General health: Poor'))) %>%
  arrange(Variable.ord) %>%
  mutate(Adjustment = factor(Adjustment,levels=c('Unadjusted','Base','Base-interaction','Clinical','Clinical-interaction')))


diff3 = SMD_IOSW3.list[[1]]$Balance %>% select(Type,Unadjusted=Diff.Un) %>%
  bind_cols(`Base` = SMD_IOSW3.list[[1]]$Balance$Diff.Adj) %>%
  bind_cols(`Base-interaction` = SMD_IOSW3.list[[2]]$Balance$Diff.Adj) %>%
  bind_cols(`Clinical` = SMD_IOSW3.list[[7]]$Balance$Diff.Adj) %>%
  bind_cols(`Clinical-interaction` = SMD_IOSW3.list[[8]]$Balance$Diff.Adj) %>%
  rownames_to_column(var = "Variable") %>%
  select(-Type) %>%
  pivot_longer(cols = -Variable, names_to = "Adjustment", values_to = "Difference") %>%
  mutate(Variable.ord = factor(Variable,levels=c('Asthma','Coronary heart disease','Congestive heart failure','Heart attack','Cancer','Hypertension','Obesity','Stroke','Diabetes mellitus'))) %>%
  arrange(Variable.ord) %>%
  mutate(Adjustment = factor(Adjustment,levels=c('Unadjusted','Base','Base-interaction','Clinical','Clinical-interaction')))

diff1 = diff1 %>% mutate(Adjustment = fct_recode(Adjustment,"Unweighted" = "Unadjusted"))
diff2 = diff2 %>% mutate(Adjustment = fct_recode(Adjustment,"Unweighted" = "Unadjusted"))
diff3 = diff3 %>% mutate(Adjustment = fct_recode(Adjustment,"Unweighted" = "Unadjusted"))


# Define color map
model_colors <- c(
  'Unweighted' = 'black',
  'Base' = viridis(10)[1],
  'Base-interaction' = viridis(10)[4],
  'Clinical' = viridis(10)[8],
  'Clinical-interaction' = '#FFB300' #viridis(10)[10]
)

# Set y position without jitter
diff1 = diff1 %>%
  filter(Adjustment %in% names(model_colors)) %>%
  mutate(y_position = -(as.numeric(Variable.ord) * 2.5))

# y-axis ticks and horizontal lines between groups
ybreaks1 = diff1 %>% filter(Adjustment == 'Clinical') %>% pull(y_position)
ylines1 = diff1 %>% filter(Adjustment == 'Clinical') %>% slice(1:(n() - 1)) %>% pull(y_position) - 1.2

# Create plot
d1 = ggplot(diff1, aes(x = Difference, y = y_position, color = Adjustment)) +
  geom_point(size = 2.5, alpha = 0.75) +
  theme_minimal() +
  labs(
    title = "Sociodemographic factors balance: All of Us vs NHANES",
    x = "Standardized Mean Differences",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks.length = unit(0.1, "cm"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  scale_y_continuous(
    breaks = ybreaks1,
    labels = levels(diff1$Variable.ord)
  ) +
  scale_color_manual(values = model_colors) +
  xlim(-0.6, 0.6) +
  coord_cartesian(ylim = c(min(diff1$y_position) + 1.2, max(diff1$y_position) - 1.2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dotted", color = "gray", size = 1) +
  geom_hline(yintercept = ylines1, linetype = "dotted", color = "gray", size = 0.5) +
  guides(
    color = guide_legend(override.aes = list(size = 3))
  )



diff2 = diff2 %>%
  filter(Adjustment %in% names(model_colors)) %>%
  mutate(y_position = -(as.numeric(Variable.ord) * 2.5))

ybreaks2 = diff2 %>% filter(Adjustment == 'Clinical') %>% pull(y_position)
ylines2 = diff2 %>% filter(Adjustment == 'Clinical') %>% slice(1:(n() - 1)) %>% pull(y_position) - 1.2

d2= ggplot(diff2, aes(x = Difference, y = y_position, color = Adjustment)) +
  geom_point(size = 2.5, alpha = 0.75) +
  theme_minimal() +
  labs(
    title = "Self-reported health factors balance: All of Us vs NHANES",
    x = "Standardized Mean Differences",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks.length = unit(0.1, "cm"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  scale_y_continuous(
    breaks = ybreaks2,
    labels = levels(diff2$Variable.ord)
  ) +
  scale_color_manual(values = model_colors) +
  xlim(-0.6, 0.6) +
  coord_cartesian(ylim = c(min(diff2$y_position) - 0.4 , max(diff2$y_position) + 0.4 )) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dotted", color = "gray", size = 1) +
  geom_hline(yintercept = ylines2, linetype = "dotted", color = "gray", size = 0.5) +
  guides(color = guide_legend(override.aes = list(size = 3)))





diff3 = diff3 %>%
  filter(Adjustment %in% names(model_colors)) %>%
  mutate(y_position = -(as.numeric(Variable.ord) * 2.5))

ybreaks3 = diff3 %>% filter(Adjustment == 'Clinical') %>% pull(y_position)
ylines3 = diff3 %>% filter(Adjustment == 'Clinical') %>% slice(1:(n() - 1)) %>% pull(y_position) - 1.2

d3 = ggplot(diff3, aes(x = Difference, y = y_position, color = Adjustment)) +
  geom_point(size = 2.5, alpha = 0.75) +
  theme_minimal() +
  labs(
    title = "Clinical factors balance: All of Us vs NHANES",
    x = "Standardized Mean Differences",
    y = ""
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks.length = unit(0.1, "cm"),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = 'bold')
  ) +
  scale_y_continuous(
    breaks = ybreaks3,
    labels = levels(diff3$Variable.ord)
  ) +
  scale_color_manual(values = model_colors) +
  xlim(-0.6, 0.6) +
  coord_cartesian(ylim = c(min(diff3$y_position) - 0.3, max(diff3$y_position) + 0.3)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dotted", color = "gray", size = 1) +
  geom_hline(yintercept = ylines3, linetype = "dotted", color = "gray", size = 0.5) +
  guides(color = guide_legend(override.aes = list(size = 3)))





pdf('MI/all_bal_newstroke.pdf', width = 8,height = 15)  
plot_grid(d1, d2, d3, labels = c('A','B','C'),ncol = 1, align = "v",rel_heights = c(23,9,10))
dev.off()









##############################################################################
# Figure 2
# Plot HR, RHR, and Improvement in RHR
##############################################################################


df.meta.plot1 = df.meta.plot1.list$basic %>% 
  dplyr::rename(HR_NHANES = HR1,lci_NHANES = lci1, uci_NHANES = uci1,HR_AoU = HR3, lci_AoU = lci3, uci_AoU = uci3, RHR_AoU = RHR3, lciRHR_AoU = lciRHR3, uciRHR_AoU = uciRHR3, HR_basic = HR2, lci_basic = lci2, uci_basic = uci2, RHR_basic = RHR2, lciRHR_basic = lciRHR2, uciRHR_basic = uciRHR2) %>% 
  left_join(df.meta.plot1.list$basic_int %>% select(Subgroup, HR_basic_int = HR2, lci_basic_int = lci2, uci_basic_int = uci2, RHR_basic_int = RHR2, lciRHR_basic_int = lciRHR2, uciRHR_basic_int = uciRHR2), by = 'Subgroup') %>%
  left_join(df.meta.plot1.list$clinical %>% select(Subgroup, HR_clinical = HR2, lci_clinical = lci2, uci_clinical = uci2, RHR_clinical = RHR2, lciRHR_clinical = lciRHR2, uciRHR_clinical = uciRHR2), by = 'Subgroup') %>%
  left_join(df.meta.plot1.list$clinical_int %>% select(Subgroup, HR_clinical_int = HR2, lci_clinical_int = lci2, uci_clinical_int = uci2, RHR_clinical_int = RHR2, lciRHR_clinical_int = lciRHR2, uciRHR_clinical_int = uciRHR2), by = 'Subgroup') 


df.meta.plot1 = df.meta.plot1  %>% 
  mutate(`HR (95% CI)` = ifelse(!is.na(HR_NHANES),
                                paste0(sprintf("%.2f (%.2f to %.2f)",HR_NHANES, lci_NHANES, uci_NHANES),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_AoU, lci_AoU, uci_AoU),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_basic, lci_basic, uci_basic),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_basic_int, lci_basic_int, uci_basic_int),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_clinical, lci_clinical, uci_clinical),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_clinical_int, lci_clinical_int, uci_clinical_int)),'')) 

df.meta.plot1 = df.meta.plot1  %>% 
  mutate(`   ` = "     ") %>%
  mutate(`RHR (95% CI)` = ifelse(!is.na(RHR_AoU),
                                 paste0('',' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_AoU, lciRHR_AoU, uciRHR_AoU),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_basic, lciRHR_basic, uciRHR_basic),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_basic_int, lciRHR_basic_int, uciRHR_basic_int),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_clinical, lciRHR_clinical, uciRHR_clinical),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_clinical_int, lciRHR_clinical_int, uciRHR_clinical_int)),'')) 


df.meta.plot2 = df.meta.plot2.list$basic %>% 
  dplyr::rename(HR_NHANES = HR1,lci_NHANES = lci1, uci_NHANES = uci1,HR_AoU = HR3, lci_AoU = lci3, uci_AoU = uci3, RHR_AoU = RHR3, lciRHR_AoU = lciRHR3, uciRHR_AoU = uciRHR3, HR_basic = HR2, lci_basic = lci2, uci_basic = uci2, RHR_basic = RHR2, lciRHR_basic = lciRHR2, uciRHR_basic = uciRHR2) %>% 
  left_join(df.meta.plot2.list$basic_int %>% select(Subgroup, HR_basic_int = HR2, lci_basic_int = lci2, uci_basic_int = uci2, RHR_basic_int = RHR2, lciRHR_basic_int = lciRHR2, uciRHR_basic_int = uciRHR2), by = 'Subgroup') %>%
  left_join(df.meta.plot2.list$clinical %>% select(Subgroup, HR_clinical = HR2, lci_clinical = lci2, uci_clinical = uci2, RHR_clinical = RHR2, lciRHR_clinical = lciRHR2, uciRHR_clinical = uciRHR2), by = 'Subgroup') %>%
  left_join(df.meta.plot2.list$clinical_int %>% select(Subgroup, HR_clinical_int = HR2, lci_clinical_int = lci2, uci_clinical_int = uci2, RHR_clinical_int = RHR2, lciRHR_clinical_int = lciRHR2, uciRHR_clinical_int = uciRHR2), by = 'Subgroup') 


df.meta.plot2 = df.meta.plot2  %>% 
  mutate(`   ` = "     ") %>%
  mutate(`HR (95% CI)` = ifelse(!is.na(HR_NHANES),
                                paste0(sprintf("%.2f (%.2f to %.2f)",HR_NHANES, lci_NHANES, uci_NHANES),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_AoU, lci_AoU, uci_AoU),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_basic, lci_basic, uci_basic),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_basic_int, lci_basic_int, uci_basic_int),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_clinical, lci_clinical, uci_clinical),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_clinical_int, lci_clinical_int, uci_clinical_int)),'')) 

df.meta.plot2 = df.meta.plot2  %>% 
  mutate(`RHR (95% CI)` = ifelse(!is.na(RHR_AoU),
                                 paste0('',' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_AoU, lciRHR_AoU, uciRHR_AoU),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_basic, lciRHR_basic, uciRHR_basic),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_basic_int, lciRHR_basic_int, uciRHR_basic_int),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_clinical, lciRHR_clinical, uciRHR_clinical),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_clinical_int, lciRHR_clinical_int, uciRHR_clinical_int)),'')) 


var.order = order(match(df.meta.plot2$Subgroup, c('Smoking','Alcohol','Health insurance','\nGeneral health (ref. excellent)\n','    Very good','    Good','    Fair','    Poor')))
df.meta.plot2 = df.meta.plot2[var.order,]

df.meta.plot3 = df.meta.plot3.list$basic %>% 
  dplyr::rename(HR_NHANES = HR1,lci_NHANES = lci1, uci_NHANES = uci1,HR_AoU = HR3, lci_AoU = lci3, uci_AoU = uci3, RHR_AoU = RHR3, lciRHR_AoU = lciRHR3, uciRHR_AoU = uciRHR3, HR_basic = HR2, lci_basic = lci2, uci_basic = uci2, RHR_basic = RHR2, lciRHR_basic = lciRHR2, uciRHR_basic = uciRHR2) %>% 
  left_join(df.meta.plot3.list$basic_int %>% select(Subgroup, HR_basic_int = HR2, lci_basic_int = lci2, uci_basic_int = uci2, RHR_basic_int = RHR2, lciRHR_basic_int = lciRHR2, uciRHR_basic_int = uciRHR2), by = 'Subgroup') %>%
  left_join(df.meta.plot3.list$clinical %>% select(Subgroup, HR_clinical = HR2, lci_clinical = lci2, uci_clinical = uci2, RHR_clinical = RHR2, lciRHR_clinical = lciRHR2, uciRHR_clinical = uciRHR2), by = 'Subgroup') %>%
  left_join(df.meta.plot3.list$clinical_int %>% select(Subgroup, HR_clinical_int = HR2, lci_clinical_int = lci2, uci_clinical_int = uci2, RHR_clinical_int = RHR2, lciRHR_clinical_int = lciRHR2, uciRHR_clinical_int = uciRHR2), by = 'Subgroup') 


df.meta.plot3 = df.meta.plot3  %>% 
  mutate(`   ` = "     ") %>%
  mutate(`HR (95% CI)` = ifelse(!is.na(HR_NHANES),
                                paste0(sprintf("%.2f (%.2f to %.2f)",HR_NHANES, lci_NHANES, uci_NHANES),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_AoU, lci_AoU, uci_AoU),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_basic, lci_basic, uci_basic),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_basic_int, lci_basic_int, uci_basic_int),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_clinical, lci_clinical, uci_clinical),' \n',
                                       sprintf("%.2f (%.2f to %.2f)",HR_clinical_int, lci_clinical_int, uci_clinical_int)),'')) 

df.meta.plot3 = df.meta.plot3  %>% 
  mutate(`RHR (95% CI)` = ifelse(!is.na(RHR_AoU),
                                 paste0('',' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_AoU, lciRHR_AoU, uciRHR_AoU),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_basic, lciRHR_basic, uciRHR_basic),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_basic_int, lciRHR_basic_int, uciRHR_basic_int),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_clinical, lciRHR_clinical, uciRHR_clinical),' \n',
                                        sprintf("%.2f (%.2f to %.2f)",RHR_clinical_int, lciRHR_clinical_int, uciRHR_clinical_int)),'')) 

# add improvement 

improvement_RHR = function(RHR1,RHR0){
  prop = (log(RHR0)-log(RHR1))/log(RHR0)
  prop = if_else(prop<=1,prop,2-prop)
  return(prop)
}

df.meta.plot1 = df.meta.plot1 %>% 
  mutate(imp_basic = improvement_RHR(RHR_basic,RHR_AoU)) %>%
  mutate(imp_basic_int = improvement_RHR(RHR_basic_int,RHR_AoU)) %>%
  mutate(imp_clinical = improvement_RHR(RHR_clinical,RHR_AoU)) %>%
  mutate(imp_clinical_int = improvement_RHR(RHR_clinical_int,RHR_AoU)) %>%
  mutate(`Improvement\nin RHR*` = ifelse(!is.na(RHR_AoU),paste0('',' \n','',' \n',
                                                                sprintf("%.1f%%",imp_basic*100),' \n',
                                                                sprintf("%.1f%%",imp_basic_int*100),' \n',
                                                                sprintf("%.1f%%",imp_clinical*100),' \n',
                                                                sprintf("%.1f%%",imp_clinical_int*100)),''))

df.meta.plot2 = df.meta.plot2 %>% 
  mutate(imp_basic = improvement_RHR(RHR_basic,RHR_AoU)) %>%
  mutate(imp_basic_int = improvement_RHR(RHR_basic_int,RHR_AoU)) %>%
  mutate(imp_clinical = improvement_RHR(RHR_clinical,RHR_AoU)) %>%
  mutate(imp_clinical_int = improvement_RHR(RHR_clinical_int,RHR_AoU)) %>%
  mutate(`Improvement\nin RHR*` = ifelse(!is.na(RHR_AoU),paste0('',' \n','',' \n',
                                                                sprintf("%.1f%%",imp_basic*100),' \n',
                                                                sprintf("%.1f%%",imp_basic_int*100),' \n',
                                                                sprintf("%.1f%%",imp_clinical*100),' \n',
                                                                sprintf("%.1f%%",imp_clinical_int*100)),''))

df.meta.plot3 = df.meta.plot3 %>% 
  mutate(imp_basic = improvement_RHR(RHR_basic,RHR_AoU)) %>%
  mutate(imp_basic_int = improvement_RHR(RHR_basic_int,RHR_AoU)) %>%
  mutate(imp_clinical = improvement_RHR(RHR_clinical,RHR_AoU)) %>%
  mutate(imp_clinical_int = improvement_RHR(RHR_clinical_int,RHR_AoU)) %>%
  mutate(`Improvement\nin RHR*` = ifelse(!is.na(RHR_AoU),paste0('',' \n','',' \n',
                                                                sprintf("%.1f%%",imp_basic*100),' \n',
                                                                sprintf("%.1f%%",imp_basic_int*100),' \n',
                                                                sprintf("%.1f%%",imp_clinical*100),' \n',
                                                                sprintf("%.1f%%",imp_clinical_int*100)),''))


df.meta.plot1 = df.meta.plot1 %>% mutate(`Improvement\nin RHR*` = if_else(!is.na(RHR_AoU),if_else(RHR_AoU >=1.2 | RHR_AoU <=0.8,`Improvement\nin RHR*`,'n/a**'),''))
df.meta.plot2 = df.meta.plot2 %>% mutate(`Improvement\nin RHR*` = if_else(!is.na(RHR_AoU),if_else(RHR_AoU >=1.2 | RHR_AoU <=0.8,`Improvement\nin RHR*`,'n/a**'),''))
df.meta.plot3 = df.meta.plot3 %>% mutate(`Improvement\nin RHR*` = if_else(!is.na(RHR_AoU),if_else(RHR_AoU >=1.2 | RHR_AoU <=0.8,`Improvement\nin RHR*`,'n/a**'),''))



# plot 
p1 = forestploter::forest(df.meta.plot1 %>% select(Subgroup,` `,`HR (95% CI)`,`  `,`RHR (95% CI)`,`   `,`Improvement\nin RHR*`),
                          est   = list(df.meta.plot1$HR_NHANES,rep(NA,nrow(df.meta.plot1)),
                                       df.meta.plot1$HR_AoU,df.meta.plot1$RHR_AoU,
                                       df.meta.plot1$HR_basic,df.meta.plot1$RHR_basic,
                                       df.meta.plot1$HR_basic_int,df.meta.plot1$RHR_basic_int,
                                       df.meta.plot1$HR_clinical,df.meta.plot1$RHR_clinical,
                                       df.meta.plot1$HR_clinical_int,df.meta.plot1$RHR_clinical_int),
                          lower = list(df.meta.plot1$lci_NHANES,rep(NA,nrow(df.meta.plot1)),
                                       df.meta.plot1$lci_AoU,df.meta.plot1$lciRHR_AoU,
                                       df.meta.plot1$lci_basic,df.meta.plot1$lciRHR_basic,
                                       df.meta.plot1$lci_basic_int,df.meta.plot1$lciRHR_basic_int,
                                       df.meta.plot1$lci_clinical,df.meta.plot1$lciRHR_clinical,
                                       df.meta.plot1$lci_clinical_int,df.meta.plot1$lciRHR_clinical_int), 
                          upper = list(df.meta.plot1$uci_NHANES,rep(NA,nrow(df.meta.plot1)),
                                       df.meta.plot1$uci_AoU,df.meta.plot1$uciRHR_AoU,
                                       df.meta.plot1$uci_basic,df.meta.plot1$uciRHR_basic,
                                       df.meta.plot1$uci_basic_int,df.meta.plot1$uciRHR_basic_int,
                                       df.meta.plot1$uci_clinical,df.meta.plot1$uciRHR_clinical,
                                       df.meta.plot1$uci_clinical_int,df.meta.plot1$uciRHR_clinical_int), 
                          sizes = 0.5,
                          ci_column = c(2,4),
                          ref_line = 1,
                          xlim = c(0, 3.5),
                          nudge_y = 0.14,
                          theme = forest_theme(core = list(padding = unit(c(8, 10), "mm"),bg_params=list(fill = c("grey92","white"))),
                                               # core = list(bg_params=list(fill = c("white"))),
                                               refline_lty = "solid",
                                               legend_name = "Data",
                                               legend_value = c("NHANES","All of Us (unweighted)","All of Us (base)","All of Us (base-interaction)","All of Us (clinical)","All of Us (clinical-interaction)"),
                                               ci_col = c("red3","black",viridis(10)[c(1,4,8)],'#FFB300'), #brewer.pal(n = 8, name = "Dark2"), # viridis(10)[c(1,4,8,10)]
                                               ci_Theight = 0.02,
                                               arrow_type = "closed")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5))


p2 = forestploter::forest(df.meta.plot2 %>% select(Subgroup,` `,`HR (95% CI)`,`  `,`RHR (95% CI)`,`   `,`Improvement\nin RHR*`),
                          est   = list(df.meta.plot2$HR_NHANES,rep(NA,nrow(df.meta.plot2)),
                                       df.meta.plot2$HR_AoU,df.meta.plot2$RHR_AoU,
                                       df.meta.plot2$HR_basic,df.meta.plot2$RHR_basic,
                                       df.meta.plot2$HR_basic_int,df.meta.plot2$RHR_basic_int,
                                       df.meta.plot2$HR_clinical,df.meta.plot2$RHR_clinical,
                                       df.meta.plot2$HR_clinical_int,df.meta.plot2$RHR_clinical_int),
                          lower = list(df.meta.plot2$lci_NHANES,rep(NA,nrow(df.meta.plot2)),
                                       df.meta.plot2$lci_AoU,df.meta.plot2$lciRHR_AoU,
                                       df.meta.plot2$lci_basic,df.meta.plot2$lciRHR_basic,
                                       df.meta.plot2$lci_basic_int,df.meta.plot2$lciRHR_basic_int,
                                       df.meta.plot2$lci_clinical,df.meta.plot2$lciRHR_clinical,
                                       df.meta.plot2$lci_clinical_int,df.meta.plot2$lciRHR_clinical_int), 
                          upper = list(df.meta.plot2$uci_NHANES,rep(NA,nrow(df.meta.plot2)),
                                       df.meta.plot2$uci_AoU,df.meta.plot2$uciRHR_AoU,
                                       df.meta.plot2$uci_basic,df.meta.plot2$uciRHR_basic,
                                       df.meta.plot2$uci_basic_int,df.meta.plot2$uciRHR_basic_int,
                                       df.meta.plot2$uci_clinical,df.meta.plot2$uciRHR_clinical,
                                       df.meta.plot2$uci_clinical_int,df.meta.plot2$uciRHR_clinical_int), 
                          sizes = 0.5,
                          ci_column = c(2,4),
                          ref_line = 1,
                          xlim = c(0, 3.5),
                          nudge_y = 0.14,
                          theme = forest_theme(core = list(padding = unit(c(8, 10), "mm"),bg_params=list(fill = c("grey92","white"))),
                                               # core = list(bg_params=list(fill = c("white"))),
                                               refline_lty = "solid",
                                               legend_name = "Data",
                                               legend_value = c("NHANES","All of Us (unweighted)","All of Us (base)","All of Us (base-interaction)","All of Us (clinical)","All of Us (clinical-interaction)"),
                                               ci_col = c("red3","black",viridis(10)[c(1,4,8)],'#FFB300'), #brewer.pal(n = 8, name = "Dark2"), # viridis(10)[c(1,4,8,10)]
                                               ci_Theight = 0.02,
                                               arrow_type = "closed")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5))


p3 = forestploter::forest(df.meta.plot3 %>% select(Subgroup,` `,`HR (95% CI)`,`  `,`RHR (95% CI)`,`   `,`Improvement\nin RHR*`),
                          est   = list(df.meta.plot3$HR_NHANES,rep(NA,nrow(df.meta.plot3)),
                                       df.meta.plot3$HR_AoU,df.meta.plot3$RHR_AoU,
                                       df.meta.plot3$HR_basic,df.meta.plot3$RHR_basic,
                                       df.meta.plot3$HR_basic_int,df.meta.plot3$RHR_basic_int,
                                       df.meta.plot3$HR_clinical,df.meta.plot3$RHR_clinical,
                                       df.meta.plot3$HR_clinical_int,df.meta.plot3$RHR_clinical_int),
                          lower = list(df.meta.plot3$lci_NHANES,rep(NA,nrow(df.meta.plot3)),
                                       df.meta.plot3$lci_AoU,df.meta.plot3$lciRHR_AoU,
                                       df.meta.plot3$lci_basic,df.meta.plot3$lciRHR_basic,
                                       df.meta.plot3$lci_basic_int,df.meta.plot3$lciRHR_basic_int,
                                       df.meta.plot3$lci_clinical,df.meta.plot3$lciRHR_clinical,
                                       df.meta.plot3$lci_clinical_int,df.meta.plot3$lciRHR_clinical_int), 
                          upper = list(df.meta.plot3$uci_NHANES,rep(NA,nrow(df.meta.plot3)),
                                       df.meta.plot3$uci_AoU,df.meta.plot3$uciRHR_AoU,
                                       df.meta.plot3$uci_basic,df.meta.plot3$uciRHR_basic,
                                       df.meta.plot3$uci_basic_int,df.meta.plot3$uciRHR_basic_int,
                                       df.meta.plot3$uci_clinical,df.meta.plot3$uciRHR_clinical,
                                       df.meta.plot3$uci_clinical_int,df.meta.plot3$uciRHR_clinical_int), 
                          sizes = 0.5,
                          ci_column = c(2,4),
                          ref_line = 1,
                          xlim = c(0, 3),
                          nudge_y = 0.14,
                          theme = forest_theme(core = list(padding = unit(c(8, 10), "mm"),bg_params=list(fill = c("grey92","white"))),
                                               # core = list(bg_params=list(fill = c("white"))),
                                               refline_lty = "solid",
                                               legend_name = "Data",
                                               legend_value = c("NHANES","All of Us (unweighted)","All of Us (base)","All of Us (base-interaction)","All of Us (clinical)","All of Us (clinical-interaction)"),
                                               ci_col = c("red3","black",viridis(10)[c(1,4,8)],'#FFB300'), #brewer.pal(n = 8, name = "Dark2"), # viridis(10)[c(1,4,8,10)]
                                               ci_Theight = 0.02,
                                               arrow_type = "closed")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5))



pdf(paste0('MI/all_cox_one_newstroke.pdf'), width = 18,height = 45)
#plot_grid(p1, p2, p3, ncol = 1,align = "v",labels = c('A','B','C'),rel_heights = c(nrow(df.meta.plot1), nrow(df.meta.plot2)+1, nrow(df.meta.plot3)+1))
p1
p2
p3
dev.off()


