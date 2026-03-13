rm(list=ls())
library(haven)
library(readr)
library(tidyr)
library(survival)
library(table1)
library(ggplot2)
library(table1)
library(stringr)
library(survey)
library(dplyr)
library(mice)

select = dplyr::select
left_join = dplyr::left_join

# functions
age_specific_IR = function(df.ir,cut.offs,event,weighted = F){
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
  if(weighted){
    df.ir = df.ir %>% mutate_at(.funs = funs(. * survey.weight), .cols = vars(starts_with("ir.")))
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
cox.mortality = function(df,exposure,timescale = 'time'){
  res = list()
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
    mod = coxph(as.formula(paste0('Surv(tstart,tstop,death) ~',paste0(c(exposure,covariates),collapse = '+'))), weights = survey.weight, data = df)
  }
  else if(timescale=='age'){
    mod = coxph(as.formula(paste0('Surv(agestart,agestop,death) ~',paste0(c(exposure,covariates),collapse = '+'))), weights = survey.weight, data = df)
  }
  res$model = mod
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
  res$summary = mod.sum
  return(res)
}


df.clean = readRDS('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/df_clean_NHANES.RDS')

# df.clean = df.clean %>%
#   filter(stroke != 'Missing' & chd != 'Missing' & congestive.heart.failure != 'Missing' & heart.attack != 'Missing' & asthma != 'Missing' & cancer != 'Missing' & hypertension != 'Missing' & diabetes != 'Missing' & obesity  != 'Missing') 

df.clean = df.clean %>%
  mutate(across(where(is.factor), droplevels))


df.NHANES = df.clean
df.NHANES = df.NHANES %>%
  mutate(across(where(is.character), as.factor))

# Make 'Missing' to NA
df.NHANES = df.NHANES %>%
  mutate(across(where(is.factor), ~ factor(replace(., . == "Missing", NA)))) 



# Multiple imputation: NHANES 

mi.NHANES = mice(df.NHANES, maxit = 0)
predM.NHANES <- mi.NHANES$predictorMatrix
meth.NHANES <- mi.NHANES$method

predM.NHANES[,c('SEQN','interview.yr','marijuana','deaf','cataract.surgery','death','fu.time','agestart','agestop','tstart','tstop')] = 0
predM.NHANES[c('SEQN','interview.yr','marijuana','deaf','cataract.surgery','death','fu.time','agestart','agestop','tstart','tstop'),] = 0

mi.NHANES = mice(df.NHANES, m = 40, method = meth.NHANES, predictorMatrix = predM.NHANES, seed = 1)

mi.list.NHANES = lapply(1:mi.NHANES$m, function(i) complete(mi.NHANES, i))

saveRDS(mi.list.NHANES,'/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/mi_list_NHANES.RDS')





