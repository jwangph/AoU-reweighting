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
library(viridis)
library(forestploter)
library(grid)
library(caret)

select = dplyr::select
left_join = dplyr::left_join
firstup = function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


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
  
  
  if(is.numeric(df %>% dplyr::select(all_of(exposure)) %>% unlist())){
    stat = df %>% summarise(n = round(sum(survey.weight)),nmor = round(sum(death*survey.weight))) %>% mutate(level = exposure) %>% select(level, n, nmor) # unweighted: n = n(),nmor = sum(death)
  } else{
    stat = df %>% group_by_at(exposure) %>% summarise(n = round(sum(survey.weight)),nmor = round(sum(death*survey.weight))) %>% select(level = exposure, n, nmor) # unweighted: n = n(),nmor = sum(death)
    stat = stat %>% mutate(level = paste0(exposure,level))
  }
  
  mod.sum = mod.sum %>% left_join(stat,by = 'level')
  names(mod.sum) = c("Exposure","level","logHR","HR","SElogHR","robust se","z_value","p_value","lci","uci","N","Death")
  res$summary = mod.sum
  return(res)
}


data.clean = readRDS('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/data_clean.RDS')
mort = readRDS('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/mort.RDS')



# HCU
for (i in 1:length(data.clean)) {
  data.clean[[i]] = data.clean[[i]] %>%
    mutate(HCU = as.numeric(HCU)) 
  cut.offs = data.clean[[i]] %>% pull(HCU) %>% quantile(c(0.33,0.67),na.rm=T)
  cut.offs = c(-Inf,cut.offs,Inf)
  data.clean[[i]] = data.clean[[i]] %>%  
    mutate(HCU.group = cut(HCU,breaks = cut.offs,labels = c('Low','Middle','High')))
}


# Merge different cohorts
df.clean = bind_rows(data.clean) 
mort.clean = bind_rows(mort) 


################################################################################
# Data cleaning
################################################################################

df.clean = df.clean %>% left_join(mort.clean %>% select(SEQN = seqn,death = mortstat,fu.time),by = join_by(SEQN))
df.clean = df.clean %>% filter(!is.na(death)) %>% filter(fu.time > 0)

hist(df.clean$baseline.age,50)
hist(df.clean$interview.yr %>% as.numeric()-df.clean$baseline.age,50)

df.clean = df.clean %>% mutate(obesity = ifelse(is.na(BMI), NA, ifelse(BMI > 30, 'Yes', 'No')))

# Missing values
df.clean = df.clean %>% 
  mutate(across(c(education, income,marital.status,smoking,alcohol,asthma,chd,congestive.heart.failure,heart.attack,cancer,hypertension,obesity,stroke,diabetes,general.health,birth.country,marijuana,health.insurance,HCU,deaf,cataract.surgery), function(x) ifelse(is.na(x),'Missing',x))) %>% 
  mutate(across(c(education, income,marital.status,smoking,alcohol,asthma,chd,congestive.heart.failure,heart.attack,cancer,hypertension,obesity,stroke,diabetes,general.health,birth.country,marijuana,health.insurance,HCU,deaf,cataract.surgery), function(x) ifelse(x == 'NA','Missing',x)))


# reference level for categorical variables
df.clean = df.clean %>% 
  mutate(sex = relevel(as.factor(sex), ref='Female')) %>%
  mutate(race.eth = relevel(as.factor(race.eth), ref='Non-Hispanic White')) %>%
  mutate(education = relevel(as.factor(education), ref='Less than high school')) %>%
  mutate(smoking = relevel(as.factor(smoking), ref='No')) %>%
  mutate(alcohol = relevel(as.factor(alcohol), ref='No')) %>%
  mutate(income = relevel(as.factor(income), ref='35-75k')) %>%
  mutate(marital.status = relevel(as.factor(marital.status), ref='Married')) %>%
  mutate(stroke = relevel(as.factor(stroke), ref='No')) %>%
  mutate(chd = relevel(as.factor(chd), ref='No')) %>%
  mutate(congestive.heart.failure = relevel(as.factor(congestive.heart.failure), ref='No')) %>%
  mutate(heart.attack = relevel(as.factor(heart.attack), ref='No')) %>%
  mutate(asthma = relevel(as.factor(asthma), ref='No')) %>%
  mutate(cancer = relevel(as.factor(cancer), ref='No')) %>%
  mutate(hypertension = relevel(as.factor(hypertension), ref='No')) %>%
  mutate(diabetes = relevel(as.factor(diabetes), ref='No')) %>%
  mutate(obesity = relevel(as.factor(obesity), ref='No')) %>%
  mutate(general.health = relevel(as.factor(general.health), ref='Excellent')) %>% 
  mutate(birth.country = relevel(as.factor(birth.country), ref='US')) %>%
  mutate(marijuana = relevel(as.factor(marijuana), ref='No')) %>%
  mutate(health.insurance = relevel(as.factor(health.insurance), ref='No')) %>%
  mutate(deaf = relevel(as.factor(deaf), ref='No')) %>%
  mutate(cataract.surgery = relevel(as.factor(cataract.surgery), ref='No')) 

  


# set up time variables
df.clean = df.clean %>%
  mutate(agestart = baseline.age, agestop = baseline.age + fu.time) %>%
  mutate(tstart = 0, tstop = fu.time) 

# Hispanic
df.clean = df.clean %>%
  mutate(race.eth = dplyr::recode(race.eth, 'Mexican American'= 'Hispanic','Other Hispanic' = 'Hispanic'))

# Income
df.clean = df.clean %>%
  mutate(income = dplyr::recode(income, 'More than 100k'= 'More than 75k','75-100k' = 'More than 75k'))

#saveRDS(df.clean, '/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/df_clean_NHANES.RDS')



df.clean.1718 = df.clean %>% 
  filter(interview.yr %in% c(2017,2018)) %>%
  mutate(survey.weight = survey.weight*10)

################################################################################
# Table 1
################################################################################


table1_weight = function(var_list,df.clean){
  
  design <- svydesign(id = ~1, weights = ~survey.weight, data = df.clean)
  
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
      prop_df = data_frame(variable = as.character(names(prop_var)),stat = sprintf("%d (%0.1f%%)", as.integer(round(prop_var)), prop_var_values))
      if(length(prop_df$variable) == 3){
        if(sum(prop_df$variable == c('No','Missing','Yes'))==3){
        prop_df = prop_df[c(3,1,2),]
        }
      }
      res = res %>%
        bind_rows(.,data.frame(variable = var)) %>%
        bind_rows(.,prop_df)
    }
  }
  return(res)
}

df.clean.table1 = df.clean %>%
  mutate(race.eth = factor(race.eth, levels = c(    'Non-Hispanic White',
                                                    'Non-Hispanic Asian',
                                                    'Non-Hispanic Black',
                                                    'Hispanic',
                                                    'Other',
                                                    'Missing')),
         education = factor(education, levels = c(    'Less than high school',
                                                      'High school', 
                                                      'Some college',
                                                      'College or above','Missing')),
         income = factor(income, levels = c(    'Less than 10k',
                                                '10-25k',
                                                '25-35k',
                                                '35-75k',
                                                'More than 75k','Missing')),
         marital.status = factor(marital.status, levels = c(    'Married',
                                                                'Living with partner',
                                                                'Divorced',
                                                                'Separated',
                                                                'Widowed',
                                                                'Never married','Missing')),
         birth.country = factor(birth.country, levels = c('US','Outside US','Missing')),
         smoking = factor(smoking, levels = c('Yes','No','Missing')),
         alcohol = factor(alcohol, levels = c('Yes','No','Missing')),
         marijuana = factor(marijuana, levels = c('Yes','No','Missing')),
         deaf = factor(deaf, levels = c('Yes','No','Missing')),
         health.insurance = factor(health.insurance, levels = c('Yes','No','Missing')),
         general.health = factor(general.health, levels = c(    'Excellent',
                                                                'Very good',
                                                                'Good',
                                                                'Fair',
                                                                'Poor',
                                                                'Missing')),
         asthma = factor(asthma, levels = c('Yes','No','Missing')),
         chd = factor(chd, levels = c('Yes','No','Missing')),
         congestive.heart.failure = factor(congestive.heart.failure, levels = c('Yes','No','Missing')),
         heart.attack = factor(heart.attack, levels = c('Yes','No','Missing')),
         cancer = factor(cancer, levels = c('Yes','No','Missing')),
         hypertension = factor(hypertension, levels = c('Yes','No','Missing')),
         obesity = factor(obesity, levels = c('Yes','No','Missing')),
         stroke = factor(stroke, levels = c('Yes','No','Missing')),
         diabetes = factor(diabetes, levels = c('Yes','No','Missing')),
         cataract.surgery = factor(cataract.surgery, levels = c('Yes','No','Missing'))
  )


df.clean.1718.table1 = df.clean.1718 %>%
  mutate(race.eth = factor(race.eth, levels = c(    'Non-Hispanic White',
                                                    'Non-Hispanic Asian',
                                                    'Non-Hispanic Black',
                                                    'Hispanic',
                                                    'Other',
                                                    'Missing')),
         education = factor(education, levels = c(    'Less than high school',
                                                      'High school', 
                                                      'Some college',
                                                      'College or above','Missing')),
         income = factor(income, levels = c(    'Less than 10k',
                                                '10-25k',
                                                '25-35k',
                                                '35-75k',
                                                'More than 75k','Missing')),
         marital.status = factor(marital.status, levels = c(    'Married',
                                                                'Living with partner',
                                                                'Divorced',
                                                                'Separated',
                                                                'Widowed',
                                                                'Never married','Missing')),
         birth.country = factor(birth.country, levels = c('US','Outside US','Missing')),
         smoking = factor(smoking, levels = c('Yes','No','Missing')),
         alcohol = factor(alcohol, levels = c('Yes','No','Missing')),
         marijuana = factor(marijuana, levels = c('Yes','No','Missing')),
         deaf = factor(deaf, levels = c('Yes','No','Missing')),
         health.insurance = factor(health.insurance, levels = c('Yes','No','Missing')),
         general.health = factor(general.health, levels = c(    'Excellent',
                                                                'Very good',
                                                                'Good',
                                                                'Fair',
                                                                'Poor',
                                                                'Missing')),
         asthma = factor(asthma, levels = c('Yes','No','Missing')),
         chd = factor(chd, levels = c('Yes','No','Missing')),
         congestive.heart.failure = factor(congestive.heart.failure, levels = c('Yes','No','Missing')),
         heart.attack = factor(heart.attack, levels = c('Yes','No','Missing')),
         cancer = factor(cancer, levels = c('Yes','No','Missing')),
         hypertension = factor(hypertension, levels = c('Yes','No','Missing')),
         obesity = factor(obesity, levels = c('Yes','No','Missing')),
         stroke = factor(stroke, levels = c('Yes','No','Missing')),
         diabetes = factor(diabetes, levels = c('Yes','No','Missing')),
         cataract.surgery = factor(cataract.surgery, levels = c('Yes','No','Missing'))
  )

t1_unw = table1(~baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+marijuana+deaf+health.insurance+general.health+asthma+chd+congestive.heart.failure+heart.attack+cancer+hypertension+obesity+stroke+diabetes+cataract.surgery,df.clean.table1) %>% as.data.frame()
names(t1_unw) = c('Var','Stat')
t1_w = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','marijuana','deaf','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes','cataract.surgery'),df.clean.table1)
names(t1_w) = c('Var','Stat')

t1_unw %>% rbind(t1_w) %>% write.csv('/Users/jwang30/Downloads/table1_NHANES.csv')
  

t1_unw_1718 = table1(~baseline.age+sex+race.eth+education+income+marital.status+birth.country+smoking+alcohol+marijuana+deaf+health.insurance+general.health+asthma+chd+congestive.heart.failure+heart.attack+cancer+hypertension+obesity+stroke+diabetes+cataract.surgery,df.clean.1718.table1) %>% as.data.frame()
names(t1_unw_1718) = c('Var','Stat')
t1_w_1718 = table1_weight(c('baseline.age','sex','race.eth','education','income','marital.status','birth.country','smoking','alcohol','marijuana','deaf','health.insurance','general.health','asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','obesity','stroke','diabetes','cataract.surgery'),df.clean.1718.table1)
names(t1_w_1718) = c('Var','Stat')

t1_unw_1718 %>% rbind(t1_w_1718) %>% write.csv('/Users/jwang30/Downloads/table1_NHANES_1718.csv')



# Number of deaths and median of follow-ups
df.clean %>% summarise(unwDeath = sum(death), wDeath = sum(death*survey.weight), unwFU = median(fu.time))
svyquantile(~fu.time,svydesign(id = ~1, weights = ~survey.weight, data = df.clean),.5)



################################################################################
# Incidence rate
################################################################################

# Unweighted
age_specific_IR(df.clean,c(18,30,40,50,60,70,80),'death') %>%
  ggplot(aes(x=Group,y=IR*1000))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(1)) +
  xlab("Age groups") +
  ylab("Mortality rate per 1,000 person-years") +
  theme_bw() + 
  # scale_fill_brewer(palette = "Dark2",name = "Sex") +
  # facet_wrap(Data~.,scales='free_x',ncol = 3) + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    axis.title.y = element_text(size=19),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) 


# Weighted
age_specific_IR(df.clean,c(18,30,40,50,60,70,80),'death',weighted = T) %>%
  ggplot(aes(x=Group,y=IR*1000))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(1)) +
  xlab("Age groups") +
  ylab("Mortality rate per 1,000 person-years") +
  theme_bw() + 
  # scale_fill_brewer(palette = "Dark2",name = "Sex") +
  # facet_wrap(Data~.,scales='free_x',ncol = 3) + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    axis.title.y = element_text(size=19),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) 



df.ir.overall = age_specific_IR(df.clean,c(18,30,40,50,60,70,80),'death',weighted = T)
df.ir.Female = age_specific_IR(df.clean %>% filter(sex == 'Female'),c(18,30,40,50,60,70,80),'death',weighted = T)
df.ir.Male = age_specific_IR(df.clean %>% filter(sex == 'Male'),c(18,30,40,50,60,70,80),'death',weighted = T)
df.ir.sex.NHANES = df.ir.overall %>% bind_cols(Sex="Overall") %>%
  bind_rows(df.ir.Female %>% bind_cols(Sex="Female")) %>%
  bind_rows(df.ir.Male %>% bind_cols(Sex="Male"))

saveRDS(df.ir.sex.NHANES, '/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/df_ir_NHANES.RDS')



### By cohort

df.ir.1 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('1999','2000')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.2 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2001','2002')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.3 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2003','2004')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.4 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2005','2006')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.5 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2007','2008')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.6 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2009','2010')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.7 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2011','2012')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.8 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2013','2014')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.9 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2015','2016')),c(18,30,40,50,60,70,80),'death',weighted = F)
df.ir.10 = age_specific_IR(df.clean %>% filter(interview.yr %in% c('2017','2018')),c(18,30,40,50,60,70,80),'death',weighted = F)


df.ir.cohort = df.ir.1 %>% bind_cols(Cohort="A") %>%
  bind_rows(df.ir.2 %>% bind_cols(Cohort="B")) %>%
  bind_rows(df.ir.3 %>% bind_cols(Cohort="C")) %>%
  bind_rows(df.ir.4 %>% bind_cols(Cohort="D")) %>%
  bind_rows(df.ir.5 %>% bind_cols(Cohort="E")) %>%
  bind_rows(df.ir.6 %>% bind_cols(Cohort="F")) %>%
  bind_rows(df.ir.7 %>% bind_cols(Cohort="G")) %>%
  bind_rows(df.ir.8 %>% bind_cols(Cohort="H")) %>%
  bind_rows(df.ir.9 %>% bind_cols(Cohort="I")) %>%
  bind_rows(df.ir.10 %>% bind_cols(Cohort="J")) 


df.ir.cohort %>% 
  ggplot(aes(x=Group,y=IR*1000,fill=Cohort))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(.9)) +
  xlab("Age groups") +
  # ylab("Mortality rate per\n1,000 person-years") +
  ylab(" ")+
  theme_bw() + 
  #scale_fill_manual(values=c("red3","royalblue3")) + 
  # facet_wrap(Data~.,scales='free_x',ncol = 3) + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) +
  coord_cartesian(ylim=c(0,110)) 





#######################################
### 5yr interval 
### By FU time (without overlaps)
# plot age & FU distributions
df.clean = df.clean %>%
  mutate(cohort = if_else(interview.yr %in% c('1999','2000'),'A',
                          if_else(interview.yr %in% c('2001','2002'),'B',
                                  if_else(interview.yr %in% c('2003','2004'),'C',
                                          if_else(interview.yr %in% c('2005','2006'),'D',
                                                  if_else(interview.yr %in% c('2007','2008'),'E',
                                                          if_else(interview.yr %in% c('2009','2010'),'F',
                                                                  if_else(interview.yr %in% c('2011','2012'),'G',
                                                                          if_else(interview.yr %in% c('2013','2014'),'H',
                                                                                  if_else(interview.yr %in% c('2015','2016'),'I',
                                                                                          if_else(interview.yr %in% c('2017','2018'),'J',NA))))))))))) 


df.clean %>% 
  filter(!is.na(cohort)) %>%
  ggplot(aes(x=agestart,y = agestop,color = cohort)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") + 
  geom_point(alpha = 0.4) +
  theme_bw() +
  scale_color_brewer(palette = "PuOr") +
  facet_wrap(~ cohort)


# split FU time
split_time_fu = function(data,cut.offs,event){
  overlap_time = function(start1,stop1,start2,stop2) {
    if(sum(stop1<=start1) >0 | sum(stop2<=start2) > 0) stop('Stop time must be greater than start times')
    return(if_else(pmin(stop1,stop2) - pmax(start1,start2)<0,0,pmin(stop1,stop2) - pmax(start1,start2)))
  }
  
  data = data %>% mutate(overlap = if_else(overlap_time(0,fu.time,cut.offs[1],cut.offs[2]) > 0, 1, 0))
  data = data %>% filter(overlap == 1)
  data = data %>% mutate(agestart.new = agestart + cut.offs[1]) %>%
    mutate(agestop.new = pmin(agestop,agestart + cut.offs[2])) 
  data = data %>% mutate(event = if_else(get(event)==0,0,if_else(fu.time>=cut.offs[1] & fu.time<cut.offs[2],1,0)))
  return(data)
}


age_specific_IR_new = function(df.ir,cut.offs,weighted = F){
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
      df.ir[[ptname]] = with(df.ir, overlap_time(agestart.new,agestop.new,cut.offs[i],cut.offs[i+1]))
      df.ir[[eventname]] = with(df.ir, event_within_interval(cut.offs[i],cut.offs[i+1],agestop.new,event))
    }
    return(df.ir)
  }
  # split follow-up time
  df.ir = split_time(df.ir,cut.offs,'event')
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


df.clean$fu.time %>% summary

df.clean.split = list()
for(i in seq(0,19,by=5)){
  cut.offs = c(i,i+5)
  df.name = paste0('df.clean.',paste0(cut.offs,collapse = ''))
  df.clean.split[[df.name]] = split_time_fu(df.clean,cut.offs,'death')
}



df.ir.a = age_specific_IR_new(df.clean.split$df.clean.05,c(25,35,45,55,65,75,85),weighted = T)
df.ir.b = age_specific_IR_new(df.clean.split$df.clean.510,c(25,35,45,55,65,75,85),weighted = T)
df.ir.c = age_specific_IR_new(df.clean.split$df.clean.1015,c(25,35,45,55,65,75,85),weighted = T)
df.ir.d = age_specific_IR_new(df.clean.split$df.clean.1520,c(25,35,45,55,65,75,85),weighted = T)


df.ir.fu = df.ir.a %>% bind_cols(FU="0-5yr") %>%
  bind_rows(df.ir.b %>% bind_cols(FU="5-10yr")) %>%
  bind_rows(df.ir.c %>% bind_cols(FU="10-15yr")) %>%
  bind_rows(df.ir.d %>% bind_cols(FU="15-20yr")) 
 

df.ir.US = data.frame(Group = c("18-25","25-35","35-45","45-55","55-65","65-75","75-85","85+"),
                      IR = c(79.5,163.4,255.4,453.3,992.1,1978.7,4708.2,14389.6)/100000)

df.ir.fu %>% 
  bind_rows(df.ir.US %>% mutate(FU ='US')) %>% 
  mutate(FU = factor(FU,levels=c('0-5yr','5-10yr','10-15yr','15-20yr','US'))) %>%
  ggplot(aes(x=Group,y=IR*1000,fill=FU))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(.9)) +
  xlab("Age groups") +
  # ylab("Mortality rate per\n1,000 person-years") +
  ylab(" ")+
  theme_bw() + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) +
  coord_cartesian(ylim=c(0,160)) 






### By FU time (with overlaps)

fu.adjust = function(fu,df.clean){
  
  df.clean.1 = df.clean %>% filter(death == 1)
  df.clean.0 = df.clean %>% filter(death == 0)
  
  df.clean.1 = df.clean.1 %>% 
    mutate(death = if_else(fu.time <= fu,1,0)) %>%
    mutate(fu.time = if_else(fu.time <= fu,fu.time,fu)) %>%
    mutate(agestop = agestart + fu.time)
  
  df.clean.0 = df.clean.0 %>% 
    mutate(fu.time = if_else(fu.time <= fu,fu.time,fu)) %>%
    mutate(agestop = agestart + fu.time)
  
  return(df.clean.1 %>% bind_rows(df.clean.0))
}

df.ir.a = age_specific_IR(fu.adjust(5,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.b = age_specific_IR(fu.adjust(10,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.c = age_specific_IR(fu.adjust(15,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.d = age_specific_IR(fu.adjust(20,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)



df.ir.fu = df.ir.a %>% bind_cols(FU="5yr") %>%
  bind_rows(df.ir.b %>% bind_cols(FU="10yr")) %>%
  bind_rows(df.ir.c %>% bind_cols(FU="15yr")) %>%
  bind_rows(df.ir.d %>% bind_cols(FU="20yr")) 

df.ir.US = data.frame(Group = c("18-25","25-35","35-45","45-55","55-65","65-75","75-85","85+"),
                      IR = c(79.5,163.4,255.4,453.3,992.1,1978.7,4708.2,14389.6)/100000)

df.ir.fu %>% 
  bind_rows(df.ir.US %>% mutate(FU ='US')) %>% 
  mutate(FU = factor(FU,levels=c('5yr','10yr','15yr','20yr','US'))) %>%
  ggplot(aes(x=Group,y=IR*1000,fill=FU))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(.9)) +
  xlab("Age groups") +
  # ylab("Mortality rate per\n1,000 person-years") +
  ylab(" ")+
  theme_bw() + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) +
  coord_cartesian(ylim=c(0,160)) 





#######################################
### 1yr interval 
### By FU time (without overlaps)
# plot age & FU distributions
df.clean = df.clean %>%
  mutate(cohort = if_else(interview.yr %in% c('1999','2000'),'A',
                          if_else(interview.yr %in% c('2001','2002'),'B',
                                  if_else(interview.yr %in% c('2003','2004'),'C',
                                          if_else(interview.yr %in% c('2005','2006'),'D',
                                                  if_else(interview.yr %in% c('2007','2008'),'E',
                                                          if_else(interview.yr %in% c('2009','2010'),'F',
                                                                  if_else(interview.yr %in% c('2011','2012'),'G',
                                                                          if_else(interview.yr %in% c('2013','2014'),'H',
                                                                                  if_else(interview.yr %in% c('2015','2016'),'I',
                                                                                          if_else(interview.yr %in% c('2017','2018'),'J',NA))))))))))) 


df.clean %>% 
  filter(!is.na(cohort)) %>%
  ggplot(aes(x=agestart,y = agestop,color = cohort)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") + 
  geom_point(alpha = 0.4) +
  theme_bw() +
  scale_color_brewer(palette = "PuOr") +
  facet_wrap(~ cohort)


# split FU time
split_time_fu = function(data,cut.offs,event){
  overlap_time = function(start1,stop1,start2,stop2) {
    if(sum(stop1<=start1) >0 | sum(stop2<=start2) > 0) stop('Stop time must be greater than start times')
    return(if_else(pmin(stop1,stop2) - pmax(start1,start2)<0,0,pmin(stop1,stop2) - pmax(start1,start2)))
  }
  
  data = data %>% mutate(overlap = if_else(overlap_time(0,fu.time,cut.offs[1],cut.offs[2]) > 0, 1, 0))
  data = data %>% filter(overlap == 1)
  data = data %>% mutate(agestart.new = agestart + cut.offs[1]) %>%
    mutate(agestop.new = pmin(agestop,agestart + cut.offs[2])) 
  data = data %>% mutate(event = if_else(get(event)==0,0,if_else(fu.time>=cut.offs[1] & fu.time<cut.offs[2],1,0)))
  return(data)
}


age_specific_IR_new = function(df.ir,cut.offs,weighted = F){
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
      df.ir[[ptname]] = with(df.ir, overlap_time(agestart.new,agestop.new,cut.offs[i],cut.offs[i+1]))
      df.ir[[eventname]] = with(df.ir, event_within_interval(cut.offs[i],cut.offs[i+1],agestop.new,event))
    }
    return(df.ir)
  }
  # split follow-up time
  df.ir = split_time(df.ir,cut.offs,'event')
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


df.clean$fu.time %>% summary

df.clean.split = list()
for(i in 0:19){
  cut.offs = c(i,i+1)
  df.name = paste0('df.clean.',paste0(cut.offs,collapse = ''))
  df.clean.split[[df.name]] = split_time_fu(df.clean,cut.offs,'death')
}



df.ir.a = age_specific_IR_new(df.clean.split$df.clean.01,c(25,35,45,55,65,75,85),weighted = T)
df.ir.b = age_specific_IR_new(df.clean.split$df.clean.12,c(25,35,45,55,65,75,85),weighted = T)
df.ir.c = age_specific_IR_new(df.clean.split$df.clean.23,c(25,35,45,55,65,75,85),weighted = T)
df.ir.d = age_specific_IR_new(df.clean.split$df.clean.34,c(25,35,45,55,65,75,85),weighted = T)
df.ir.e = age_specific_IR_new(df.clean.split$df.clean.45,c(25,35,45,55,65,75,85),weighted = T)
df.ir.f = age_specific_IR_new(df.clean.split$df.clean.56,c(25,35,45,55,65,75,85),weighted = T)
df.ir.g = age_specific_IR_new(df.clean.split$df.clean.67,c(25,35,45,55,65,75,85),weighted = T)
df.ir.h = age_specific_IR_new(df.clean.split$df.clean.78,c(25,35,45,55,65,75,85),weighted = T)
df.ir.i = age_specific_IR_new(df.clean.split$df.clean.89,c(25,35,45,55,65,75,85),weighted = T)
df.ir.j = age_specific_IR_new(df.clean.split$df.clean.910,c(25,35,45,55,65,75,85),weighted = T)
df.ir.k = age_specific_IR_new(df.clean.split$df.clean.1011,c(25,35,45,55,65,75,85),weighted = T)
df.ir.l = age_specific_IR_new(df.clean.split$df.clean.1112,c(25,35,45,55,65,75,85),weighted = T)
df.ir.m = age_specific_IR_new(df.clean.split$df.clean.1213,c(25,35,45,55,65,75,85),weighted = T)
df.ir.n = age_specific_IR_new(df.clean.split$df.clean.1314,c(25,35,45,55,65,75,85),weighted = T)
df.ir.o = age_specific_IR_new(df.clean.split$df.clean.1415,c(25,35,45,55,65,75,85),weighted = T)
df.ir.p = age_specific_IR_new(df.clean.split$df.clean.1516,c(25,35,45,55,65,75,85),weighted = T)
df.ir.q = age_specific_IR_new(df.clean.split$df.clean.1617,c(25,35,45,55,65,75,85),weighted = T)
df.ir.r = age_specific_IR_new(df.clean.split$df.clean.1718,c(25,35,45,55,65,75,85),weighted = T)
df.ir.s = age_specific_IR_new(df.clean.split$df.clean.1819,c(25,35,45,55,65,75,85),weighted = T)
df.ir.t = age_specific_IR_new(df.clean.split$df.clean.1920,c(25,35,45,55,65,75,85),weighted = T)


df.ir.fu = df.ir.a %>% bind_cols(FU="0-1yr") %>%
  bind_rows(df.ir.b %>% bind_cols(FU="1-2yr")) %>%
  bind_rows(df.ir.c %>% bind_cols(FU="2-3yr")) %>%
  bind_rows(df.ir.d %>% bind_cols(FU="3-4yr")) %>%
  bind_rows(df.ir.e %>% bind_cols(FU="4-5yr")) %>%
  bind_rows(df.ir.f %>% bind_cols(FU="5-6yr")) %>%
  bind_rows(df.ir.g %>% bind_cols(FU="6-7yr")) %>%
  bind_rows(df.ir.h %>% bind_cols(FU="7-8yr")) %>%
  bind_rows(df.ir.i %>% bind_cols(FU="8-9yr")) %>%
  bind_rows(df.ir.j %>% bind_cols(FU="9-10yr")) %>%
  bind_rows(df.ir.k %>% bind_cols(FU="10-11yr")) %>%
  bind_rows(df.ir.l %>% bind_cols(FU="11-12yr")) %>%
  bind_rows(df.ir.m %>% bind_cols(FU="12-13yr")) %>%
  bind_rows(df.ir.n %>% bind_cols(FU="13-14yr")) %>%
  bind_rows(df.ir.o %>% bind_cols(FU="14-15yr")) %>%
  bind_rows(df.ir.p %>% bind_cols(FU="15-16yr")) %>%
  bind_rows(df.ir.q %>% bind_cols(FU="16-17yr")) %>%
  bind_rows(df.ir.r %>% bind_cols(FU="17-18yr")) %>%
  bind_rows(df.ir.s %>% bind_cols(FU="18-19yr")) %>%
  bind_rows(df.ir.t %>% bind_cols(FU="19-20yr")) 

df.ir.US = data.frame(Group = c("18-25","25-35","35-45","45-55","55-65","65-75","75-85","85+"),
                      IR = c(79.5,163.4,255.4,453.3,992.1,1978.7,4708.2,14389.6)/100000)

df.ir.fu %>% 
  bind_rows(df.ir.US %>% mutate(FU ='US')) %>% 
  mutate(FU = factor(FU,levels=c(paste0(0:19,'-',1:20,'yr'),'US'))) %>%
  ggplot(aes(x=Group,y=IR*1000,fill=FU))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(.9)) +
  xlab("Age groups") +
  # ylab("Mortality rate per\n1,000 person-years") +
  ylab(" ")+
  theme_bw() + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) +
  coord_cartesian(ylim=c(0,160)) 






### By FU time (with overlaps)

fu.adjust = function(fu,df.clean){
  
  df.clean.1 = df.clean %>% filter(death == 1)
  df.clean.0 = df.clean %>% filter(death == 0)
  
  df.clean.1 = df.clean.1 %>% 
    mutate(death = if_else(fu.time <= fu,1,0)) %>%
    mutate(fu.time = if_else(fu.time <= fu,fu.time,fu)) %>%
    mutate(agestop = agestart + fu.time)
  
  df.clean.0 = df.clean.0 %>% 
    mutate(fu.time = if_else(fu.time <= fu,fu.time,fu)) %>%
    mutate(agestop = agestart + fu.time)
  
  return(df.clean.1 %>% bind_rows(df.clean.0))
}

df.ir.a = age_specific_IR(fu.adjust(1,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.b = age_specific_IR(fu.adjust(2,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.c = age_specific_IR(fu.adjust(3,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.d = age_specific_IR(fu.adjust(4,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.e = age_specific_IR(fu.adjust(5,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.f = age_specific_IR(fu.adjust(6,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.g = age_specific_IR(fu.adjust(7,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.h = age_specific_IR(fu.adjust(8,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.i = age_specific_IR(fu.adjust(9,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.j = age_specific_IR(fu.adjust(10,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.k = age_specific_IR(fu.adjust(11,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.l = age_specific_IR(fu.adjust(12,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.m = age_specific_IR(fu.adjust(13,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.n = age_specific_IR(fu.adjust(14,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.o = age_specific_IR(fu.adjust(15,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.p = age_specific_IR(fu.adjust(16,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.q = age_specific_IR(fu.adjust(17,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.r = age_specific_IR(fu.adjust(18,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.s = age_specific_IR(fu.adjust(19,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)
df.ir.t = age_specific_IR(fu.adjust(20,df.clean),c(25,35,45,55,65,75,85),'death',weighted = T)


df.ir.fu = df.ir.a %>% bind_cols(FU="1yr") %>%
  bind_rows(df.ir.b %>% bind_cols(FU="2yr")) %>%
  bind_rows(df.ir.c %>% bind_cols(FU="3yr")) %>%
  bind_rows(df.ir.d %>% bind_cols(FU="4yr")) %>%
  bind_rows(df.ir.e %>% bind_cols(FU="5yr")) %>%
  bind_rows(df.ir.f %>% bind_cols(FU="6yr")) %>%
  bind_rows(df.ir.g %>% bind_cols(FU="7yr")) %>%
  bind_rows(df.ir.h %>% bind_cols(FU="8yr")) %>%
  bind_rows(df.ir.i %>% bind_cols(FU="9yr")) %>%
  bind_rows(df.ir.j %>% bind_cols(FU="10yr")) %>%
  bind_rows(df.ir.k %>% bind_cols(FU="11yr")) %>%
  bind_rows(df.ir.l %>% bind_cols(FU="12yr")) %>%
  bind_rows(df.ir.m %>% bind_cols(FU="13yr")) %>%
  bind_rows(df.ir.n %>% bind_cols(FU="14yr")) %>%
  bind_rows(df.ir.o %>% bind_cols(FU="15yr")) %>%
  bind_rows(df.ir.p %>% bind_cols(FU="16yr")) %>%
  bind_rows(df.ir.q %>% bind_cols(FU="17yr")) %>%
  bind_rows(df.ir.r %>% bind_cols(FU="18yr")) %>%
  bind_rows(df.ir.s %>% bind_cols(FU="19yr")) %>%
  bind_rows(df.ir.t %>% bind_cols(FU="20yr")) 
  
df.ir.US = data.frame(Group = c("18-25","25-35","35-45","45-55","55-65","65-75","75-85","85+"),
                      IR = c(79.5,163.4,255.4,453.3,992.1,1978.7,4708.2,14389.6)/100000)
  
df.ir.fu %>% 
  bind_rows(df.ir.US %>% mutate(FU ='US')) %>% 
  mutate(FU = factor(FU,levels=c(paste0(1:20,'yr'),'US'))) %>%
  ggplot(aes(x=Group,y=IR*1000,fill=FU))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(.9)) +
  xlab("Age groups") +
  # ylab("Mortality rate per\n1,000 person-years") +
  ylab(" ")+
  theme_bw() + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) +
  coord_cartesian(ylim=c(0,160)) 
  





  
##### By general health

df.ir.overall = age_specific_IR(df.clean,c(18,30,40,50,60,70,80),'death')
df.ir.excellentverygood = age_specific_IR(df.clean %>% filter(general.health %in% c('Excellent','Very good')),c(18,30,40,50,60,70,80),'death')
df.ir.good = age_specific_IR(df.clean %>% filter(general.health == 'Good'),c(18,30,40,50,60,70,80),'death')
df.ir.fair = age_specific_IR(df.clean %>% filter(general.health == 'Fair'),c(18,30,40,50,60,70,80),'death')
df.ir.poor = age_specific_IR(df.clean %>% filter(general.health == 'Poor'),c(18,30,40,50,60,70,80),'death')

df.ir.general.health = df.ir.overall %>% bind_cols(`General health`="Overall") %>%
  bind_rows(df.ir.excellentverygood %>% bind_cols(`General health`="Excellent or very good")) %>%
  bind_rows(df.ir.good %>% bind_cols(`General health`="Good")) %>%
  bind_rows(df.ir.fair %>% bind_cols(`General health`="Fair")) %>%
  bind_rows(df.ir.poor %>% bind_cols(`General health`="Poor")) 


df.ir.general.health %>% 
  mutate(`General health` = factor(`General health`,levels=c('Overall','Excellent or very good','Good','Fair','Poor'))) %>% 
  ggplot(aes(x=Group,y=IR*1000,fill=`General health`))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=(IR-1.96*SE)*1000, ymax=(IR+1.96*SE)*1000), width=.2,
                position=position_dodge(0.9)) +
  xlab("Age groups") +
  # ylab("Mortality rate per\n1,000 person-years") +
  ylab(" ")+
  theme_bw() + 
  scale_fill_viridis(discrete = TRUE) +
  #scale_fill_brewer(palette = "Dark2") +
  #scale_fill_manual(values=c("red3","royalblue3","green3",'pink','orange')) + 
  # facet_wrap(Data~.,scales='free_x',ncol = 3) + 
  theme(
    axis.title.x = element_text(size=19,vjust = 0.1),
    # axis.title.y = element_blank(),
    axis.text.y = element_text(size=19),
    axis.text.x = element_text(vjust = 0.98,size=19),
    strip.text.x = element_text(size=19),
    strip.text.y = element_text(size=19),
    legend.text = element_text(size=19),
    legend.title = element_text(size=19),
    title = element_text(size=19),
    legend.key.height = unit(3, "line")
  ) +
  coord_cartesian(ylim=c(0,150)) 



################################################################################
# Survival analysis
################################################################################



mod.baseline.age = cox.mortality(df.clean %>% mutate(baseline.age = baseline.age/10),'baseline.age')
mod.race.eth = cox.mortality(df.clean,'race.eth')
mod.sex = cox.mortality(df.clean,'sex')
mod.education = cox.mortality(df.clean,'education')
mod.income = cox.mortality(df.clean,'income')
mod.marital.status = cox.mortality(df.clean,'marital.status')
mod.birth.country = cox.mortality(df.clean,'birth.country')
mod.smoking = cox.mortality(df.clean,'smoking')
mod.alcohol = cox.mortality(df.clean,'alcohol')                                      
mod.marijuana = cox.mortality(df.clean,'marijuana')
mod.deaf = cox.mortality(df.clean,'deaf')
mod.health.insurance = cox.mortality(df.clean,'health.insurance')
mod.general.health = cox.mortality(df.clean,'general.health')
mod.BMI = cox.mortality(df.clean,'BMI')                                              
mod.asthma = cox.mortality(df.clean,'asthma')
mod.chd = cox.mortality(df.clean,'chd')
mod.cancer = cox.mortality(df.clean,'cancer')
mod.congestive.heart.failure = cox.mortality(df.clean,'congestive.heart.failure')
mod.heart.attack = cox.mortality(df.clean,'heart.attack')
mod.hypertension = cox.mortality(df.clean,'hypertension')
mod.obesity = cox.mortality(df.clean,'obesity')
mod.stroke = cox.mortality(df.clean,'stroke')
mod.diabetes = cox.mortality(df.clean,'diabetes')
mod.cataract.surgery = cox.mortality(df.clean,'cataract.surgery')




#########################
# Cox assumption test
exposure.list = c("baseline.age", "race.eth", "sex", "education", "income", "marital.status",
                  "birth.country", "smoking", "alcohol", "marijuana", "deaf", "health.insurance",
                  "general.health", "BMI", "asthma", "chd", "cancer", "congestive.heart.failure",
                  "heart.attack", "hypertension", "obesity", "stroke", "diabetes", "cataract.surgery")
covariates = c('race.eth','sex','baseline.age')
zph = data.frame(exposure=NULL,p=NULL)
p_list = list()
for (exposure in exposure.list) {
  cox_model = coxph(as.formula(paste0('Surv(tstart,tstop,death) ~',paste0(c(exposure,covariates),collapse = '+'))),  data = df.clean)
  test = cox.zph(cox_model)
  zph = zph %>% bind_rows(data.frame(exposure,p=as.numeric(test$table['GLOBAL','p'])))
  
  pdf(paste0('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/schoenfeld_',gsub("\\.", "_", exposure),'.pdf'), width = 6,height = 6)
  print(ggcoxdiagnostics(cox_model, type = "schoenfeld", ggtheme = theme_minimal()))
  dev.off()
  
}

zph = zph %>% mutate(violated = p<=0.05) 








 # Combine coef summary for plot
combine_summary = function(res,group='NHANES'){
  return(res$summary %>% bind_cols(group = group) )
}


df.plot.NHANES = combine_summary(mod.baseline.age) %>%
  bind_rows(combine_summary(mod.race.eth)) %>%
  bind_rows(combine_summary(mod.sex)) %>%
  bind_rows(combine_summary(mod.education)) %>%
  bind_rows(combine_summary(mod.income)) %>%
  bind_rows(combine_summary(mod.marital.status)) %>%
  bind_rows(combine_summary(mod.birth.country)) %>%
  bind_rows(combine_summary(mod.smoking)) %>%
  bind_rows(combine_summary(mod.alcohol)) %>%
  bind_rows(combine_summary(mod.marijuana)) %>%
  bind_rows(combine_summary(mod.deaf)) %>%
  bind_rows(combine_summary(mod.health.insurance)) %>%
  bind_rows(combine_summary(mod.general.health)) %>%
  bind_rows(combine_summary(mod.asthma)) %>%
  bind_rows(combine_summary(mod.chd)) %>%
  bind_rows(combine_summary(mod.cancer)) %>%
  bind_rows(combine_summary(mod.congestive.heart.failure)) %>%
  bind_rows(combine_summary(mod.heart.attack)) %>%
  bind_rows(combine_summary(mod.hypertension)) %>%
  bind_rows(combine_summary(mod.obesity)) %>%
  bind_rows(combine_summary(mod.stroke)) %>%
  bind_rows(combine_summary(mod.diabetes)) %>%
  bind_rows(combine_summary(mod.cataract.surgery)) 
  

df.plot.NHANES = df.plot.NHANES %>%
  rowwise() %>%
  mutate(level =  ifelse(level == Exposure,Exposure,gsub(Exposure,"",level))) 


saveRDS(df.plot.NHANES, '/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/plot_NHANES.RDS')



##########################################
# Sensitivity analysis stratified by HCU

mod.baseline.age.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low') %>% mutate(baseline.age = baseline.age/10),'baseline.age')
mod.race.eth.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'race.eth')
mod.sex.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'sex')
mod.education.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'education')
mod.income.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'income')
mod.marital.status.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'marital.status')
mod.birth.country.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'birth.country')
mod.smoking.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'smoking')
mod.alcohol.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'alcohol')                                      
mod.marijuana.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'marijuana')
mod.deaf.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'deaf')
mod.health.insurance.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'health.insurance')
mod.general.health.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'general.health')
mod.BMI.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'BMI')                                              
mod.asthma.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'asthma')
mod.chd.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'chd')
mod.cancer.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'cancer')
mod.congestive.heart.failure.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'congestive.heart.failure')
mod.heart.attack.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'heart.attack')
mod.hypertension.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'hypertension')
mod.obesity.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'obesity')
mod.stroke.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'stroke')
mod.diabetes.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'diabetes')
mod.cataract.surgery.1 = cox.mortality(df.clean %>% filter(HCU.group=='Low'),'cataract.surgery')

mod.baseline.age.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle') %>% mutate(baseline.age = baseline.age/10),'baseline.age')
mod.race.eth.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'race.eth')
mod.sex.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'sex')
mod.education.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'education')
mod.income.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'income')
mod.marital.status.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'marital.status')
mod.birth.country.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'birth.country')
mod.smoking.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'smoking')
mod.alcohol.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'alcohol')                                      
mod.marijuana.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'marijuana')
mod.deaf.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'deaf')
mod.health.insurance.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'health.insurance')
mod.general.health.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'general.health')
mod.BMI.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'BMI')                                              
mod.asthma.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'asthma')
mod.chd.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'chd')
mod.cancer.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'cancer')
mod.congestive.heart.failure.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'congestive.heart.failure')
mod.heart.attack.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'heart.attack')
mod.hypertension.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'hypertension')
mod.obesity.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'obesity')
mod.stroke.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'stroke')
mod.diabetes.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'diabetes')
mod.cataract.surgery.2 = cox.mortality(df.clean %>% filter(HCU.group=='Middle'),'cataract.surgery')

mod.baseline.age.3 = cox.mortality(df.clean %>% filter(HCU.group=='High') %>% mutate(baseline.age = baseline.age/10),'baseline.age')
mod.race.eth.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'race.eth')
mod.sex.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'sex')
mod.education.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'education')
mod.income.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'income')
mod.marital.status.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'marital.status')
mod.birth.country.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'birth.country')
mod.smoking.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'smoking')
mod.alcohol.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'alcohol')                                      
mod.marijuana.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'marijuana')
mod.deaf.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'deaf')
mod.health.insurance.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'health.insurance')
mod.general.health.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'general.health')
mod.BMI.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'BMI')                                              
mod.asthma.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'asthma')
mod.chd.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'chd')
mod.cancer.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'cancer')
mod.congestive.heart.failure.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'congestive.heart.failure')
mod.heart.attack.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'heart.attack')
mod.hypertension.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'hypertension')
mod.obesity.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'obesity')
mod.stroke.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'stroke')
mod.diabetes.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'diabetes')
mod.cataract.surgery.3 = cox.mortality(df.clean %>% filter(HCU.group=='High'),'cataract.surgery')



df.plot.NHANES.str = combine_summary(mod.baseline.age.1,'Low') %>%
  bind_rows(combine_summary(mod.baseline.age.2,'Middle')) %>%
  bind_rows(combine_summary(mod.baseline.age.3,'High')) %>%
  bind_rows(combine_summary(mod.race.eth.1,'Low')) %>%
  bind_rows(combine_summary(mod.race.eth.2,'Middle')) %>%
  bind_rows(combine_summary(mod.race.eth.3,'High')) %>%
  bind_rows(combine_summary(mod.sex.1,'Low')) %>%
  bind_rows(combine_summary(mod.sex.2,'Middle')) %>%
  bind_rows(combine_summary(mod.sex.3,'High')) %>%
  bind_rows(combine_summary(mod.education.1,'Low')) %>%
  bind_rows(combine_summary(mod.education.2,'Middle')) %>%
  bind_rows(combine_summary(mod.education.3,'High')) %>%
  bind_rows(combine_summary(mod.income.1,'Low')) %>%
  bind_rows(combine_summary(mod.income.2,'Middle')) %>%
  bind_rows(combine_summary(mod.income.3,'High')) %>%
  bind_rows(combine_summary(mod.marital.status.1,'Low')) %>%
  bind_rows(combine_summary(mod.marital.status.2,'Middle')) %>%
  bind_rows(combine_summary(mod.marital.status.3,'High')) %>%
  bind_rows(combine_summary(mod.birth.country.1,'Low')) %>%
  bind_rows(combine_summary(mod.birth.country.2,'Middle')) %>%
  bind_rows(combine_summary(mod.birth.country.3,'High')) %>%
  bind_rows(combine_summary(mod.smoking.1,'Low')) %>%
  bind_rows(combine_summary(mod.smoking.2,'Middle')) %>%
  bind_rows(combine_summary(mod.smoking.3,'High')) %>%
  bind_rows(combine_summary(mod.alcohol.1,'Low')) %>%
  bind_rows(combine_summary(mod.alcohol.2,'Middle')) %>%
  bind_rows(combine_summary(mod.alcohol.3,'High')) %>%
  bind_rows(combine_summary(mod.marijuana.1,'Low')) %>%
  bind_rows(combine_summary(mod.marijuana.2,'Middle')) %>%
  bind_rows(combine_summary(mod.marijuana.3,'High')) %>%
  bind_rows(combine_summary(mod.deaf.1,'Low')) %>%
  bind_rows(combine_summary(mod.deaf.2,'Middle')) %>%
  bind_rows(combine_summary(mod.deaf.3,'High')) %>%
  bind_rows(combine_summary(mod.health.insurance.1,'Low')) %>%
  bind_rows(combine_summary(mod.health.insurance.2,'Middle')) %>%
  bind_rows(combine_summary(mod.health.insurance.3,'High')) %>%
  bind_rows(combine_summary(mod.general.health.1,'Low')) %>%
  bind_rows(combine_summary(mod.general.health.2,'Middle')) %>%
  bind_rows(combine_summary(mod.general.health.3,'High')) %>%
  bind_rows(combine_summary(mod.asthma.1,'Low')) %>%
  bind_rows(combine_summary(mod.asthma.2,'Middle')) %>%
  bind_rows(combine_summary(mod.asthma.3,'High')) %>%
  bind_rows(combine_summary(mod.chd.1,'Low')) %>%
  bind_rows(combine_summary(mod.chd.2,'Middle')) %>%
  bind_rows(combine_summary(mod.chd.3,'High')) %>%
  bind_rows(combine_summary(mod.cancer.1,'Low')) %>%
  bind_rows(combine_summary(mod.cancer.2,'Middle')) %>%
  bind_rows(combine_summary(mod.cancer.3,'High')) %>%
  bind_rows(combine_summary(mod.congestive.heart.failure.1,'Low')) %>%
  bind_rows(combine_summary(mod.congestive.heart.failure.2,'Middle')) %>%
  bind_rows(combine_summary(mod.congestive.heart.failure.3,'High')) %>%
  bind_rows(combine_summary(mod.heart.attack.1,'Low')) %>%
  bind_rows(combine_summary(mod.heart.attack.2,'Middle')) %>%
  bind_rows(combine_summary(mod.heart.attack.3,'High')) %>%
  bind_rows(combine_summary(mod.hypertension.1,'Low')) %>%
  bind_rows(combine_summary(mod.hypertension.2,'Middle')) %>%
  bind_rows(combine_summary(mod.hypertension.3,'High')) %>%
  bind_rows(combine_summary(mod.obesity.1,'Low')) %>%
  bind_rows(combine_summary(mod.obesity.2,'Middle')) %>%
  bind_rows(combine_summary(mod.obesity.3,'High')) %>%
  bind_rows(combine_summary(mod.stroke.1,'Low')) %>%
  bind_rows(combine_summary(mod.stroke.2,'Middle')) %>%
  bind_rows(combine_summary(mod.stroke.3,'High')) %>%
  bind_rows(combine_summary(mod.diabetes.1,'Low')) %>%
  bind_rows(combine_summary(mod.diabetes.2,'Middle')) %>%
  bind_rows(combine_summary(mod.diabetes.3,'High')) %>%
  bind_rows(combine_summary(mod.cataract.surgery.1,'Low')) %>%
  bind_rows(combine_summary(mod.cataract.surgery.2,'Middle')) %>%
  bind_rows(combine_summary(mod.cataract.surgery.3,'High'))




df.plot.NHANES.str = df.plot.NHANES.str %>%
  rowwise() %>%
  mutate(level =  ifelse(level == Exposure,Exposure,gsub(Exposure,"",level))) 

escalc_res_str = function(df.meta){
  
  exposure.list = df.meta %>% .$Exposure %>% unique()
  
  level.list = list(race.eth = c('Non-Hispanic Asian','Non-Hispanic Black','Hispanic','Other','Missing'),
                    sex = c('Female','Male','Other or missing'),
                    education = c('High school','Some college','College or above','Missing'),
                    income = c('Less than 10k','10-25k','25-35k','More than 75k','Missing'),
                    marital.status = c('Living with partner','Divorced','Separated','Widowed','Never married','Missing'),
                    birth.country = c('Outside US','Missing'),
                    smoking = c('Yes','Missing'),
                    alcohol = c('Yes','Missing'),
                    marijuana = c('Yes','Missing'),
                    deaf = c('Yes','Missing'),
                    health.insurance = c('Yes','Missing'),
                    general.health = c('Very good','Good','Fair','Poor','Missing'),
                    asthma = c('Yes','Missing'),
                    chd = c('Yes','Missing'),
                    cancer = c('Yes','Missing'),
                    congestive.heart.failure = c('Yes','Missing'),
                    heart.attack = c('Yes','Missing'),
                    hypertension = c('Yes','Missing'),
                    obesity = c('Yes','Missing'),
                    stroke = c('Yes','Missing'),
                    diabetes = c('Yes','Missing'),
                    cataract.surgery = c('Yes','Missing'))
  res = c()
  for (exp in exposure.list) {
    temp = df.meta %>% filter(Exposure==exp) %>% mutate(group = factor(group,levels = c('Low','Middle','High')))
    if(!is.null(level.list[[exp]])){
      temp = temp %>% mutate(level =  factor(level, levels = level.list[[exp]]))
    }
    temp = temp %>% 
      group_by(level) %>%
      arrange(group, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(level = as.character(level))
    
    temp.wide = temp %>% filter(group == 'Low') %>%     select(Exposure, level, HR1 = HR, lci1=lci,uci1=uci,SElogHR1 = SElogHR,N1 = N,Death1=Death) %>%
      full_join(temp %>% filter(group == 'Middle') %>%  select(Exposure, level, HR2 = HR, lci2=lci,uci2=uci,SElogHR2 = SElogHR,N2 = N,Death2=Death),by = c('Exposure', 'level')) %>%
      full_join(temp %>% filter(group == 'High') %>%    select(Exposure, level, HR3 = HR, lci3=lci,uci3=uci,SElogHR3 = SElogHR,N3 = N,Death3=Death),by = c('Exposure', 'level'))
    
    
    res = res %>% bind_rows(data.frame(Subgroup = exp) %>% bind_rows(temp.wide))
    
  }
  
  return(res)
  
}


df.plot.NHANES.str = escalc_res_str(df.plot.NHANES.str) %>%
  mutate(Subgroup = ifelse(is.na(Subgroup),paste0("    ",level),Subgroup)) %>%
  mutate(Subgroup = dplyr::recode(Subgroup,'baseline.age' = 'Baseline age','    baseline.age' = '    Baseline age', 'race.eth' = 'Race/ethnicity', 'marital.status' = 'Marital status','birth.country' = 'Country of birth','chd'='Coronary heart disease','congestive.heart.failure'='Congestive heart failure','heart.attack' = 'Heart attack','cataract.surgery' = 'Cataract surgery','health.insurance' = 'Health insurance','general.health' = 'General health')) %>%
  mutate(Subgroup = firstup(Subgroup)) %>%
  mutate(`HR (95% CI)` = ifelse(is.na(HR1),' \n',sprintf("%.2f (%.2f to %.2f) \n%.2f (%.2f to %.2f) \n%.2f (%.2f to %.2f) ",HR1, lci1, uci1,HR2, lci2, uci2,HR3, lci3, uci3))) %>%
  mutate(N = ifelse(is.na(HR1),' \n',paste0(N1,' \n',N2,' \n',N3))) %>%
  mutate(Death = ifelse(is.na(HR1),' \n',paste0(Death1,' \n',Death2,' \n',Death3)))
df.plot.NHANES.str$` ` = "                                                   "

df.plot.NHANES.str = df.plot.NHANES.str %>% 
  mutate(Subgroup = if_else(!is.na(Exposure) & Exposure == 'baseline.age','Baseline age (per decade)',Subgroup)) %>%
  filter(!(Subgroup == 'Baseline age' & is.na(Exposure))) %>%
  mutate(Subgroup = if_else(Subgroup == 'Race/ethnicity','Race/ethnicity (ref. Non-Hispanic White)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == '    Male','Male (vs. female)',Subgroup)) %>%
  filter(!Subgroup == 'Sex') %>%
  filter(Subgroup != '    Other or missing') %>% # remove sex Other or missing row
  mutate(Subgroup = if_else(Subgroup == 'Education','Education (ref. less than high school)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Income','Income (ref. 35-75k)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Marital status','Marital status (ref. married)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Country of birth','Country of birth (ref. U.S.)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == '    Outside US','    Outside U.S.',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Smoking','Smoking (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Alcohol','Alcohol (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Marijuana','Marijuana (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Deaf','Serious hearing difficulty (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Health insurance','Health insurance (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'General health','General health (ref. excellent)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Asthma','Asthma (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Coronary heart disease','Coronary heart disease (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Cancer','Cancer (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Congestive heart failure','Congestive heart failure (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Heart attack','Heart attack (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Hypertension','Hypertension (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Obesity','Obesity (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Stroke','Stroke (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Diabetes','Diabetes mellitus (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Cataract surgery','Cataract surgery (ref. no)',Subgroup)) 

df.plot.NHANES.str1 = df.plot.NHANES.str %>% slice(1:28)

df.plot.NHANES.str2 = df.plot.NHANES.str %>% slice(29:49)

df.plot.NHANES.str3 = df.plot.NHANES.str %>% slice(50:79)

hcu.p1 = forestploter::forest(df.plot.NHANES.str1 %>% select(Subgroup,N,` `,`HR (95% CI)`),
                              est = list(df.plot.NHANES.str1$HR1,df.plot.NHANES.str1$HR2,df.plot.NHANES.str1$HR3),
                              lower = list(df.plot.NHANES.str1$lci1,df.plot.NHANES.str1$lci2,df.plot.NHANES.str1$lci3), 
                              upper = list(df.plot.NHANES.str1$uci1,df.plot.NHANES.str1$uci2,df.plot.NHANES.str1$uci3), 
                              sizes = 0.5,
                              ci_column = 3,
                              ref_line = 1,
                              arrow_lab = c("Lower risk", "Higher risk"),
                              xlim = c(0, 7),
                              nudge_y = 0.2,
                              theme = forest_theme(core = list(padding = unit(c(4, 5), "mm"),bg_params=list(fill = c("grey92","white"))),
                                                   refline_lty = "solid",
                                                   legend_name = "Healthcare utilization",
                                                   legend_value = c("Low","Middle","High"),
                                                   ci_col = c("#440154FF","#21908CFF","#FDE725FF"),
                                                   ci_Theight = 0.2,
                                                   arrow_label_just = "end",
                                                   arrow_type = "closed")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5))

hcu.p2 = forestploter::forest(df.plot.NHANES.str2 %>% select(Subgroup,N,` `,`HR (95% CI)`),
                              est = list(df.plot.NHANES.str2$HR1,df.plot.NHANES.str2$HR2,df.plot.NHANES.str2$HR3),
                              lower = list(df.plot.NHANES.str2$lci1,df.plot.NHANES.str2$lci2,df.plot.NHANES.str2$lci3), 
                              upper = list(df.plot.NHANES.str2$uci1,df.plot.NHANES.str2$uci2,df.plot.NHANES.str2$uci3), 
                              sizes = 0.5,
                              ci_column = 3,
                              ref_line = 1,
                              arrow_lab = c("Lower risk", "Higher risk"),
                              xlim = c(0, 7),
                              nudge_y = 0.2,
                              theme = forest_theme(core = list(padding = unit(c(4, 5), "mm"),bg_params=list(fill = c("grey92","white"))),
                                                   refline_lty = "solid",
                                                   legend_name = "Healthcare utilization",
                                                   legend_value = c("Low","Middle","High"),
                                                   ci_col = c("#440154FF","#21908CFF","#FDE725FF"),
                                                   ci_Theight = 0.2,
                                                   arrow_label_just = "end",
                                                   arrow_type = "closed")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5))


hcu.p3 = forestploter::forest(df.plot.NHANES.str3 %>% select(Subgroup,N,` `,`HR (95% CI)`),
                              est = list(df.plot.NHANES.str3$HR1,df.plot.NHANES.str3$HR2,df.plot.NHANES.str3$HR3),
                              lower = list(df.plot.NHANES.str3$lci1,df.plot.NHANES.str3$lci2,df.plot.NHANES.str3$lci3), 
                              upper = list(df.plot.NHANES.str3$uci1,df.plot.NHANES.str3$uci2,df.plot.NHANES.str3$uci3), 
                              sizes = 0.5,
                              ci_column = 3,
                              ref_line = 1,
                              arrow_lab = c("Lower risk", "Higher risk"),
                              xlim = c(0, 7),
                              nudge_y = 0.2,
                              theme = forest_theme(core = list(padding = unit(c(4, 5), "mm"),bg_params=list(fill = c("grey92","white"))),
                                                   refline_lty = "solid",
                                                   legend_name = "Healthcare utilization",
                                                   legend_value = c("Low","Middle","High"),
                                                   ci_col = c("#440154FF","#21908CFF","#FDE725FF"),
                                                   ci_Theight = 0.2,
                                                   arrow_label_just = "end",
                                                   arrow_type = "closed")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5))

pdf('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/NHANES_HCU_a.pdf', width = 12,height = 28)
hcu.p1
dev.off()


pdf('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/NHANES_HCU_b.pdf', width = 12,height = 40)
hcu.p2
dev.off()

pdf('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/NHANES_HCU_c.pdf', width = 12,height = 40)
hcu.p3
dev.off()








#########################################
# QBA

df.plot.AoU = readRDS('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/AoU/df_meta_plot_AoU.RDS')

df.plot.AoU = df.plot.AoU %>% 
  filter(Exposure %in% c('asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','stroke','diabetes') & level =='Yes')

PV = data.frame(Exposure = c('asthma','chd','congestive.heart.failure','heart.attack','cancer','hypertension','stroke','diabetes'),PPV=c(0.76,0.46,0.52,0.60,0.80,0.70,0.76,0.70),NPV = c(0.81,0.97,0.98,0.98,0.77,0.77,0.95,0.93))

df.plot.AoU = df.plot.AoU %>% 
  left_join(PV, join_by(Exposure))

df.plot.AoU = df.plot.AoU %>% 
  mutate(HR = (NPV*HR2+PPV-1)/((NPV-1)*HR2+PPV), lci = HR/exp(1.96*SElogHR2), uci = HR*exp(1.96*SElogHR2)) %>%
  select(Exposure,level,HR2,lci2,uci2,PPV,NPV,HR,lci,uci)



##########################################
# Sensitivity analysis 17-18 cycle

mod.1718.baseline.age = cox.mortality(df.clean.1718 %>% mutate(baseline.age = baseline.age/10),'baseline.age')
mod.1718.race.eth = cox.mortality(df.clean.1718,'race.eth')
mod.1718.sex = cox.mortality(df.clean.1718,'sex')
mod.1718.education = cox.mortality(df.clean.1718,'education')
mod.1718.income = cox.mortality(df.clean.1718,'income')
mod.1718.marital.status = cox.mortality(df.clean.1718,'marital.status')
mod.1718.birth.country = cox.mortality(df.clean.1718,'birth.country')
mod.1718.smoking = cox.mortality(df.clean.1718,'smoking')
mod.1718.alcohol = cox.mortality(df.clean.1718,'alcohol')                                      
mod.1718.marijuana = cox.mortality(df.clean.1718,'marijuana')
mod.1718.deaf = cox.mortality(df.clean.1718,'deaf')
mod.1718.health.insurance = cox.mortality(df.clean.1718,'health.insurance')
mod.1718.general.health = cox.mortality(df.clean.1718,'general.health')
mod.1718.BMI = cox.mortality(df.clean.1718,'BMI')                                              
mod.1718.asthma = cox.mortality(df.clean.1718,'asthma')
mod.1718.chd = cox.mortality(df.clean.1718,'chd')
mod.1718.cancer = cox.mortality(df.clean.1718,'cancer')
mod.1718.congestive.heart.failure = cox.mortality(df.clean.1718,'congestive.heart.failure')
mod.1718.heart.attack = cox.mortality(df.clean.1718,'heart.attack')
mod.1718.hypertension = cox.mortality(df.clean.1718,'hypertension')
mod.1718.obesity = cox.mortality(df.clean.1718,'obesity')
mod.1718.stroke = cox.mortality(df.clean.1718,'stroke')
mod.1718.diabetes = cox.mortality(df.clean.1718,'diabetes')
mod.1718.cataract.surgery = cox.mortality(df.clean.1718,'cataract.surgery')



df.plot.NHANES.1718 = combine_summary(mod.1718.baseline.age) %>%
  bind_rows(combine_summary(mod.1718.race.eth)) %>%
  bind_rows(combine_summary(mod.1718.sex)) %>%
  bind_rows(combine_summary(mod.1718.education)) %>%
  bind_rows(combine_summary(mod.1718.income)) %>%
  bind_rows(combine_summary(mod.1718.marital.status)) %>%
  bind_rows(combine_summary(mod.1718.birth.country)) %>%
  bind_rows(combine_summary(mod.1718.smoking)) %>%
  bind_rows(combine_summary(mod.1718.alcohol)) %>%
  bind_rows(combine_summary(mod.1718.marijuana)) %>%
  bind_rows(combine_summary(mod.1718.deaf)) %>%
  bind_rows(combine_summary(mod.1718.health.insurance)) %>%
  bind_rows(combine_summary(mod.1718.general.health)) %>%
  bind_rows(combine_summary(mod.1718.asthma)) %>%
  bind_rows(combine_summary(mod.1718.chd)) %>%
  bind_rows(combine_summary(mod.1718.cancer)) %>%
  bind_rows(combine_summary(mod.1718.congestive.heart.failure)) %>%
  bind_rows(combine_summary(mod.1718.heart.attack)) %>%
  bind_rows(combine_summary(mod.1718.hypertension)) %>%
  bind_rows(combine_summary(mod.1718.obesity)) %>%
  bind_rows(combine_summary(mod.1718.stroke)) %>%
  bind_rows(combine_summary(mod.1718.diabetes)) %>%
  bind_rows(combine_summary(mod.1718.cataract.surgery)) 


df.plot.NHANES.1718 = df.plot.NHANES.1718 %>%
  mutate(group='NHANES (2017-2018)') 

df.plot.NHANES = df.plot.NHANES %>%
  mutate(group='NHANES (1999-2018)') 


df.plot.NHANES.1718 = df.plot.NHANES.1718 %>%
  rowwise() %>%
  mutate(level =  ifelse(level == Exposure,Exposure,gsub(Exposure,"",level))) 


df.meta = df.plot.NHANES %>% bind_rows(df.plot.NHANES.1718)


escalc_res = function(df.meta){
  
  exposure.list = df.meta %>% .$Exposure %>% unique()
  
  level.list = list(race.eth = c('Non-Hispanic Asian','Non-Hispanic Black','Hispanic','Other','Missing'),
                    sex = c('Female','Male','Other or missing'),
                    education = c('High school','Some college','College or above','Missing'),
                    income = c('Less than 10k','10-25k','25-35k','More than 75k','Missing'),
                    marital.status = c('Living with partner','Divorced','Separated','Widowed','Never married','Missing'),
                    birth.country = c('Outside US','Missing'),
                    smoking = c('Yes','Missing'),
                    alcohol = c('Yes','Missing'),
                    marijuana = c('Yes','Missing'),
                    deaf = c('Yes','Missing'),
                    health.insurance = c('Yes','Missing'),
                    general.health = c('Very good','Good','Fair','Poor','Missing'),
                    asthma = c('Yes','Missing'),
                    chd = c('Yes','Missing'),
                    cancer = c('Yes','Missing'),
                    congestive.heart.failure = c('Yes','Missing'),
                    heart.attack = c('Yes','Missing'),
                    hypertension = c('Yes','Missing'),
                    obesity = c('Yes','Missing'),
                    stroke = c('Yes','Missing'),
                    diabetes = c('Yes','Missing'),
                    cataract.surgery = c('Yes','Missing'))
  res = c()
  for (exp in exposure.list) {
    temp = df.meta %>% filter(Exposure==exp) %>% mutate(group = factor(group,levels = c("NHANES (1999-2018)","NHANES (2017-2018)")))
    if(!is.null(level.list[[exp]])){
      temp = temp %>% mutate(level =  factor(level, levels = level.list[[exp]]))
    }
    temp = temp %>% 
      group_by(level) %>%
      arrange(group, .by_group = TRUE) %>%
      ungroup() %>%
      mutate(level = as.character(level))
    
    temp.wide = temp %>% filter(group == 'NHANES (1999-2018)') %>% select(Exposure, level, HR1 = HR, lci1=lci,uci1=uci,SElogHR1 = SElogHR,N1 = N,Death1=Death) %>%
      full_join(temp %>% filter(group == 'NHANES (2017-2018)') %>% select(Exposure, level, HR2 = HR, lci2=lci,uci2=uci,SElogHR2 = SElogHR,N2 = N,Death2=Death),by = c('Exposure', 'level'))
    
    #temp.wide = temp.wide %>% mutate(`Important difference` = if_else(abs((log(HR2) - log(HR1))/log(HR1)) < 0.2,'','*'))
    temp.wide = temp.wide %>% mutate(`Important difference` = if_else(HR2 / HR1 > 1.2 | HR2 / HR1 < 0.8,'*',''))
    res = res %>% bind_rows(data.frame(Subgroup = exp) %>% bind_rows(temp.wide))
    
  }
  
  return(res)
  
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

df.meta.plot = escalc_res(df.meta) %>%
  mutate(Subgroup = ifelse(is.na(Subgroup),paste0("    ",level),Subgroup)) %>%
  mutate(Subgroup = dplyr::recode(Subgroup,'baseline.age' = 'Baseline age','    baseline.age' = '    Baseline age', 'race.eth' = 'Race/ethnicity', 'marital.status' = 'Marital status','birth.country' = 'Country of birth','chd'='Coronary heart disease','congestive.heart.failure'='Congestive heart failure','heart.attack' = 'Heart attack','cataract.surgery' = 'Cataract surgery','health.insurance' = 'Health insurance','general.health' = 'General health')) %>%
  mutate(Subgroup = firstup(Subgroup)) %>%
  mutate(`HR (95% CI)` = ifelse(!is.na(HR1),ifelse(!is.na(HR2),paste0(sprintf("%.2f (%.2f to %.2f)",HR1, lci1, uci1),' \n',sprintf("%.2f (%.2f to %.2f)",HR2, lci2, uci2)),paste0(sprintf("%.2f (%.2f to %.2f)",HR1, lci1, uci1),' \n')),ifelse(!is.na(HR2),paste0(' \n',sprintf("%.2f (%.2f to %.2f)",HR2, lci2, uci2)),' \n'))) %>%
  mutate(N = ifelse(!is.na(N1),ifelse(!is.na(N2),paste0(N1,' \n',N2),paste0(N1,' \n')),ifelse(!is.na(N2),paste0(' \n',N2),' \n'))) %>%
  mutate(Death = ifelse(!is.na(Death1),ifelse(!is.na(Death2),paste0(Death1,' \n',Death2),paste0(Death1,' \n')),ifelse(!is.na(Death2),paste0(' \n',Death2),' \n'))) 
df.meta.plot$` ` = "                                                   "


df.meta.plot = df.meta.plot %>% 
  mutate(Subgroup = if_else(!is.na(Exposure) & Exposure == 'baseline.age','Baseline age (per decade)',Subgroup)) %>%
  filter(!(Subgroup == 'Baseline age' & is.na(Exposure))) %>%
  mutate(Subgroup = if_else(Subgroup == 'Race/ethnicity','Race/ethnicity (ref. Non-Hispanic White)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == '    Male','Male (vs. female)',Subgroup)) %>%
  filter(!Subgroup == 'Sex') %>%
  filter(Subgroup != '    Other or missing') %>% # remove sex Other or missing row
  mutate(Subgroup = if_else(Subgroup == 'Education','Education (ref. less than high school)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Income','Income (ref. 35-75k)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Marital status','Marital status (ref. married)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Country of birth','Country of birth (ref. U.S.)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == '    Outside US','    Outside U.S.',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Smoking','Smoking (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Alcohol','Alcohol (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Marijuana','Marijuana (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Deaf','Serious hearing difficulty (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Health insurance','Health insurance (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'General health','General health (ref. excellent)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Asthma','Asthma (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Coronary heart disease','Coronary heart disease (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Cancer','Cancer (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Congestive heart failure','Congestive heart failure (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Heart attack','Heart attack (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Hypertension','Hypertension (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Obesity','Obesity (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Stroke','Stroke (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Diabetes','Diabetes mellitus (ref. no)',Subgroup)) %>%
  mutate(Subgroup = if_else(Subgroup == 'Cataract surgery','Cataract surgery (ref. no)',Subgroup)) 


df.meta.plot1 = df.meta.plot %>% slice(1:28)

df.meta.plot2 = df.meta.plot %>% slice(29:49)

df.meta.plot3 = df.meta.plot %>% slice(50:79)


library(forestploter)
library(grid)

p1 = forestploter::forest(df.meta.plot1 %>% select(Subgroup,N,Death,` `,`HR (95% CI)`) ,
                          est = list(df.meta.plot1$HR1,df.meta.plot1$HR2),
                          lower = list(df.meta.plot1$lci1,df.meta.plot1$lci2), 
                          upper = list(df.meta.plot1$uci1,df.meta.plot1$uci2), 
                          sizes = 0.5,
                          ci_column = 4,
                          ref_line = 1,
                          arrow_lab = c("Lower risk", "Higher risk"),
                          xlim = c(0, 2.5),
                          nudge_y = 0.4,
                          theme = forest_theme(core = list(padding = unit(c(4, 5), "mm"),bg_params=list(fill = c(rep('grey92',1),rep('white',6),rep('grey92',1),rep('white',5),rep('grey92',6),rep('white',7),rep('grey92',3)))),
                                               # core = list(bg_params=list(fill = c("white"))),    
                                               refline_lty = "solid",
                                               legend_name = "Data",
                                               legend_value = c("NHANES (1999-2018)","NHANES (2017-2018)"),
                                               # ci_col = c("#8491B4FF","#B09C8599"),
                                               ci_col = c("red3","royalblue3"),
                                               ci_Theight = 0.2,
                                               arrow_label_just = "end",
                                               arrow_type = "closed")) %>%
  edit_plot(., row = nrow(df.meta.plot1)+1, gp = gpar(fontface = "bold")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5)) %>%
  edit_plot(., col = 6, which = "text",hjust = unit(0.5, "npc"),x = unit(0.5, "npc"))



p2 = forestploter::forest(df.meta.plot2 %>% select(Subgroup,N,Death,` `,`HR (95% CI)`) ,
                          est = list(df.meta.plot2$HR1,df.meta.plot2$HR2),
                          lower = list(df.meta.plot2$lci1,df.meta.plot2$lci2), 
                          upper = list(df.meta.plot2$uci1,df.meta.plot2$uci2), 
                          sizes = 0.5,
                          ci_column = 4,
                          ref_line = 1,
                          arrow_lab = c("Lower risk", "Higher risk"),
                          xlim = c(0, 5),
                          nudge_y = 0.4,
                          theme = forest_theme(core = list(padding = unit(c(4, 5), "mm"),bg_params=list(fill = c(rep('grey92',3),rep('white',3),rep('grey92',3),rep('white',3),rep('grey92',3),rep('white',6)))),
                                               # core = list(bg_params=list(fill = c("white"))),
                                               refline_lty = "solid",
                                               legend_name = "Data",
                                               legend_value = c("NHANES (1999-2018)","NHANES (2017-2018)"),
                                               # ci_col = c("#8491B4FF","#B09C8599"),
                                               ci_col = c("red3","royalblue3"),
                                               ci_Theight = 0.2,
                                               arrow_label_just = "end",
                                               arrow_type = "closed")) %>%
  edit_plot(., row = nrow(df.meta.plot2)+1, gp = gpar(fontface = "bold")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5)) %>%
  edit_plot(., col = 6, which = "text",hjust = unit(0.5, "npc"),x = unit(0.5, "npc"))

p3 = forestploter::forest(df.meta.plot3 %>% select(Subgroup,N,Death,` `,`HR (95% CI)`) ,
                          est = list(df.meta.plot3$HR1,df.meta.plot3$HR2),
                          lower = list(df.meta.plot3$lci1,df.meta.plot3$lci2), 
                          upper = list(df.meta.plot3$uci1,df.meta.plot3$uci2), 
                          sizes = 0.5,
                          ci_column = 4,
                          ref_line = 1,
                          arrow_lab = c("Lower risk", "Higher risk"),
                          xlim = c(0, 5),
                          nudge_y = 0.4,
                          theme = forest_theme(core = list(padding = unit(c(4, 5), "mm"),bg_params=list(fill = c(rep('grey92',3),rep('white',3),rep('grey92',3),rep('white',3),rep('grey92',3),rep('white',3),rep('grey92',3),rep('white',3),rep('grey92',3),rep('white',3)))),
                                               # core = list(bg_params=list(fill = c("white"))),
                                               refline_lty = "solid",
                                               legend_name = "Data",
                                               legend_value = c("NHANES (1999-2018)","NHANES (2017-2018)"),
                                               # ci_col = c("#8491B4FF","#B09C8599"),
                                               ci_col = c("red3","royalblue3"),
                                               ci_Theight = 0.2,
                                               arrow_label_just = "end",
                                               arrow_type = "closed")) %>%
  edit_plot(., row = nrow(df.meta.plot3)+1, gp = gpar(fontface = "bold")) %>%
  add_border(., part = "header", row = 1,gp = gpar(lwd = 1.5)) %>%
  edit_plot(., col = 6, which = "text",hjust = unit(0.5, "npc"),x = unit(0.5, "npc"))


pdf('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/NHANES1718_a.pdf', width = 14,height = 21)
p1
dev.off()


pdf('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/NHANES1718_b.pdf', width = 14,height = 21)
p2
dev.off()


pdf('/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Manuscript Paper 1/Tables and figures/R&R/NHANES1718_c.pdf', width = 14,height = 21)
p3
dev.off()





