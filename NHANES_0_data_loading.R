rm(list=ls())
library(haven)
library(readr)
library(dplyr)


################################################################################
# Read continuous NHANES data
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/continuousnhanes/default.aspx
################################################################################
# Letters denote different waves
# 1999-2000: no letter
# 2001-2002: B
# 2003-2004: C
# 2005-2006: D
# 2007-2008: E
# 2009-2010: F
# 2011-2012: G
# 2013-2014: H
# 2015-2016: I
# 2017-2018: J

# years = c("1999-2000","2001-2002","2003-2004","2005-2006","2007-2008","2009-2010","2011-2012","2013-2014","2015-2016","2017-2018")
years = c("1999","2001","2003","2005","2007","2009","2011","2013","2015","2017")
letters = c("","B","C","D","E","F","G","H","I","J")


data = list()
for (i in 1:length(years)) {
  print(paste0("Start cohort ",years[i]))
  DEMO_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/DEMO",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  BMX_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/BMX",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  ALQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/ALQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  SMQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/SMQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  MCQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/MCQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  BPQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/BPQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  DIQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/DIQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  HUQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/HUQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  DUQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/DUQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  HIQ_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/HIQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  
  if(i <= 7){
    hearing_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/AUQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  } else{
    hearing_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/DLQ",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  }
  
  if(i <= 5){
    VIX_temp = read_xpt(paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/",years[i],"/DataFiles/VIX",ifelse(letters[i]=="","",paste0("_",letters[i])),".XPT"))
  }
  
  dat_temp = DEMO_temp %>% select(SEQN, # ID
                                  SDDSRVYR, # Data Release Number
                                  RIDSTATR, # Interview/Examination Status
                                  RIDEXMON, # Six month time period
                                  RIAGENDR, # Gender
                                  RIDAGEYR, # Age at Screening Adjudicated - Recode
                                  RIDAGEMN, # Age in Months - Recode
                                  starts_with("RIDRETH"), # Race/ethnicity
                                  starts_with("DMDEDUC"), # Education
                                  DMDMARTL, # Marital Status
                                  starts_with("INDHHIN"), # Annual Household Income (before 2007-2008: INDHHINC; after: INDHHIN2)
                                  starts_with("WTINT"),  # Survey weight
                                  starts_with("DMDBORN") # Country of birth
                                  ) %>%
    left_join(BMX_temp %>% select(SEQN,BMXBMI), by='SEQN') %>%
    left_join(ALQ_temp %>% select(SEQN,any_of(c("ALQ100","ALD100","ALQ101","ALQ111"))), by='SEQN') %>%
    left_join(SMQ_temp %>% select(SEQN,SMQ020), by='SEQN') %>%
    left_join(MCQ_temp %>% select(SEQN,MCQ160F,MCQ160C,MCQ160B,MCQ160E,MCQ010,MCQ220), by='SEQN') %>%
    left_join(BPQ_temp %>% select(SEQN,BPQ020), by='SEQN') %>%
    left_join(DIQ_temp %>% select(SEQN,DIQ010), by='SEQN') %>% 
    left_join(HUQ_temp %>% select(SEQN,HUQ010,starts_with("HUQ05")), by='SEQN') %>%
    left_join(DUQ_temp %>% select(any_of(c('SEQN','DUQ200'))), by='SEQN') %>%
    left_join(HIQ_temp %>% select(SEQN,any_of(c("HID010", "HIQ011"))), by='SEQN') %>%
    left_join(hearing_temp %>% select(SEQN,any_of(c("AUQ130", "AUQ131","AUQ054","DLQ010"))), by='SEQN') %>%
    left_join(VIX_temp %>% select(any_of(c("SEQN", "VIQ200"))), by='SEQN') 
    
  
  data[[i]] = dat_temp
}

# Data cleaning
data.clean = data
## 1999-2000
data.clean[[1]] = data.clean[[1]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '1999','2' = '2000')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH1, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','5' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHINC, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','11'= 'More than 75k','12'= 'NA','13' = 'NA','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT4YR*2/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ100, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN, '1'= 'US','2' = 'Outside US','3' = 'Outside US','7' = 'NA','9' = 'NA')) %>%
  mutate(marijuana = 'NA') %>%
  mutate(health.insurance = dplyr::recode(HID010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ130, '1'= 'No','2' = 'No','3' = 'Yes','4' = 'Yes','7' = 'NA','9' = 'NA')) %>%
  mutate(cataract.surgery = dplyr::recode(VIQ200, '1'= 'Yes','2' = 'No','9' = 'NA')) %>%
  select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery")) %>%
  mutate_if(is.character,function (x) na_if(x, 'NA'))
  
## 2001-2002
data.clean[[2]] = data.clean[[2]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2001','2' = '2002')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH1, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','5' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHINC, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','11'= 'More than 75k','12'= 'NA','13' = 'NA','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT4YR*2/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALD100, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN, '1'= 'US','2' = 'Outside US','3' = 'Outside US','7' = 'NA','9' = 'NA')) %>%
  mutate(marijuana = 'NA') %>%
  mutate(health.insurance = dplyr::recode(HID010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ130, '1'= 'No','2' = 'No','3' = 'Yes','4' = 'Yes','7' = 'NA','9' = 'NA')) %>%
  mutate(cataract.surgery = dplyr::recode(VIQ200, '1'= 'Yes','2' = 'No','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))

## 2003-2004
data.clean[[3]] = data.clean[[3]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2003','2' = '2004')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH1, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','5' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHINC, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','11'= 'More than 75k','12'= 'NA','13' = 'NA','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN, '1'= 'US','2' = 'Outside US','3' = 'Outside US','7' = 'NA','9' = 'NA')) %>%
  mutate(marijuana = 'NA') %>%
  mutate(health.insurance = dplyr::recode(HID010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ130, '1'= 'No','2' = 'No','3' = 'Yes','4' = 'Yes','7' = 'NA','9' = 'NA')) %>%
  mutate(cataract.surgery = dplyr::recode(VIQ200, '1'= 'Yes','2' = 'No','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))


## 2005-2006
data.clean[[4]] = data.clean[[4]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2005','2' = '2006')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH1, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','5' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHINC, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','11'= 'More than 75k','12'= 'NA','13' = 'NA','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN, '1'= 'US','2' = 'Outside US','3' = 'Outside US','7' = 'NA','9' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ131, '1'= 'No','2' = 'No','3' = 'No','4' = 'No','5' = 'Yes','6' = 'Yes','7' = 'NA','9' = 'NA')) %>%
  mutate(cataract.surgery = dplyr::recode(VIQ200, '1'= 'Yes','2' = 'No','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))


## 2007-2008
data.clean[[5]] = data.clean[[5]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2007','2' = '2008')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH1, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','5' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHIN2, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','12'= 'NA','13' = 'NA','14' = '75-100k','15' = 'More than 100k','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN2, '1'= 'US','2' = 'Outside US','4' = 'Outside US','5' = 'Outside US','7' = 'NA','9' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ131, '1'= 'No','2' = 'No','3' = 'No','4' = 'No','5' = 'Yes','6' = 'Yes','7' = 'NA','9' = 'NA')) %>%
  mutate(cataract.surgery = dplyr::recode(VIQ200, '1'= 'Yes','2' = 'No','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))

## 2009-2010
data.clean[[6]] = data.clean[[6]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2009','2' = '2010')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH1, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','5' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHIN2, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','12'= 'NA','13' = 'NA','14' = '75-100k','15' = 'More than 100k','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN2, '1'= 'US','2' = 'Outside US','4' = 'Outside US','5' = 'Outside US','7' = 'NA','9' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ131, '1'= 'No','2' = 'No','3' = 'No','4' = 'No','5' = 'Yes','6' = 'Yes','7' = 'NA','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))

## 2011-2012
data.clean[[7]] = data.clean[[7]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2011','2' = '2012')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH3, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','6' = 'Non-Hispanic Asian','7' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHIN2, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','12'= 'NA','13' = 'NA','14' = '75-100k','15' = 'More than 100k','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ050, '0'= '0','1' = '1','2' = '2.5','3'='6.5','4'='11','5' = '13','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN4, '1'= 'US','2' = 'Outside US','77' = 'NA','99' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(AUQ054, '1'= 'No','2' = 'No','3' = 'No','4' = 'No','5' = 'Yes','6' = 'Yes','77' = 'NA','99' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))

## 2013-2014
data.clean[[8]] = data.clean[[8]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2013','2' = '2014')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH3, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','6' = 'Non-Hispanic Asian','7' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHIN2, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','12'= 'NA','13' = 'NA','14' = '75-100k','15' = 'More than 100k','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ051, '0'= '0','1' = '1','2' = '2.5','3'='4.5','4'='6.5','5' = '8.5','6'='11','7'='14','8'='16','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN4, '1'= 'US','2' = 'Outside US','77' = 'NA','99' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(DLQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))

## 2015-2016
data.clean[[9]] = data.clean[[9]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2015','2' = '2016')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH3, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','6' = 'Non-Hispanic Asian','7' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHIN2, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','12'= 'NA','13' = 'NA','14' = '75-100k','15' = 'More than 100k','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ101, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ051, '0'= '0','1' = '1','2' = '2.5','3'='4.5','4'='6.5','5' = '8.5','6'='11','7'='14','8'='16','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN4, '1'= 'US','2' = 'Outside US','77' = 'NA','99' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(DLQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))


## 2017-2018
data.clean[[10]] = data.clean[[10]] %>% 
  filter(RIDAGEYR >= 18 | RIDAGEMN >= 216) %>% # 18+
  mutate(interview.yr = dplyr::recode(RIDEXMON, '1'= '2017','2' = '2018')) %>%
  mutate(sex = dplyr::recode(RIAGENDR, '1'= 'Male','2' = 'Female')) %>%
  mutate(baseline.age = ifelse(!is.na(RIDAGEMN),RIDAGEMN/12,RIDAGEYR)) %>%
  mutate(race.eth = dplyr::recode(RIDRETH3, '1'= 'Mexican American','2' = 'Other Hispanic','3' = 'Non-Hispanic White','4' = 'Non-Hispanic Black','6' = 'Non-Hispanic Asian','7' = 'Other')) %>%
  mutate(education = dplyr::recode(DMDEDUC2, '1'= 'Less than high school','2' = 'Less than high school','3' = 'High school','4' = 'Some college','5' = 'College or above','7' = 'NA','9' = 'NA')) %>%
  mutate(education = ifelse(is.na(education),dplyr::recode(DMDEDUC3, '0'= 'Less than high school','1'= 'Less than high school','2'= 'Less than high school','3'= 'Less than high school','4'= 'Less than high school','5'= 'Less than high school','6'= 'Less than high school','7'= 'Less than high school','8'= 'Less than high school','9'= 'Less than high school','10'= 'Less than high school','11'= 'Less than high school','12'= 'Less than high school','13' = 'High school','14' = 'High school','15' = 'High school','55' = 'Less than high school','66' = 'Less than high school','77' = 'NA','99' = 'NA'),education)) %>%
  mutate(marital.status = dplyr::recode(DMDMARTL, '1'= 'Married','2' = 'Widowed','3' = 'Divorced','4' = 'Separated','5' = 'Never married','6' = 'Living with partner','77' = 'NA','99' = 'NA')) %>%
  mutate(income = dplyr::recode(INDHHIN2, '1'= 'Less than 10k','2' = 'Less than 10k','3' = '10-25k','4' = '10-25k','5' = '10-25k','6'= '25-35k','7'= '35-75k','8'= '35-75k','9'= '35-75k','10'= '35-75k','12'= 'NA','13' = 'NA','14' = '75-100k','15' = 'More than 100k','77' = 'NA','99' = 'NA')) %>%
  mutate(survey.weight = WTINT2YR*1/10) %>%
  mutate(BMI = BMXBMI) %>%
  mutate(alcohol = dplyr::recode(ALQ111, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(smoking = dplyr::recode(SMQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(stroke = dplyr::recode(MCQ160F, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(chd = dplyr::recode(MCQ160C, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(congestive.heart.failure = dplyr::recode(MCQ160B, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(heart.attack = dplyr::recode(MCQ160E, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(asthma = dplyr::recode(MCQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(cancer = dplyr::recode(MCQ220, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(hypertension = dplyr::recode(BPQ020, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(diabetes = dplyr::recode(DIQ010, '1'= 'Yes','2' = 'No','3' = 'NA','7' = 'NA','9' = 'NA')) %>%
  mutate(general.health = dplyr::recode(HUQ010, '1'= 'Excellent','2' = 'Very good','3' = 'Good','4'='Fair','5'='Poor','7' = 'NA','9' = 'NA')) %>%
  mutate(HCU = dplyr::recode(HUQ051, '0'= '0','1' = '1','2' = '2.5','3'='4.5','4'='6.5','5' = '8.5','6'='11','7'='14','8'='16','77' = 'NA','99' = 'NA')) %>%
  mutate(birth.country = dplyr::recode(DMDBORN4, '1'= 'US','2' = 'Outside US','77' = 'NA','99' = 'NA')) %>%
  mutate(marijuana = dplyr::recode(DUQ200, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(health.insurance = dplyr::recode(HIQ011, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
  mutate(deaf = dplyr::recode(DLQ010, '1'= 'Yes','2' = 'No','7' = 'NA','9' = 'NA')) %>%
    select(SEQN,interview.yr,sex,baseline.age,race.eth,education,marital.status,income,survey.weight,BMI,alcohol,smoking,stroke,chd,congestive.heart.failure,heart.attack,asthma,cancer,hypertension,diabetes,general.health,HCU,birth.country,marijuana,health.insurance,deaf,any_of("cataract.surgery"))


saveRDS(data, '/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/data.RDS')
saveRDS(data.clean, '/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/data_clean.RDS')






################################################################################
# Read mortality data
# https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/
################################################################################
read_CDC_mortality = function(file){
  temp = read_fwf(file=file,
                  col_types = "iiiiiiii",
                  fwf_cols(seqn = c(1,6),
                           eligstat = c(15,15),
                           mortstat = c(16,16),
                           ucod_leading = c(17,19),
                           diabetes = c(20,20),
                           hyperten = c(21,21),
                           permth_int = c(43,45),
                           permth_exm = c(46,48)
                  ),
                  na = c("", ".")
  )
  return(temp)
} 

mort = list()

for (i in 1:length(years)) {
  print(paste0("Start cohort ",years[i]))
  # mort_temp = read_CDC_mortality(paste0("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_",sub("-", "_", years[i]),"_MORT_2019_PUBLIC.dat"))
  # mort_temp = read_CDC_mortality(paste0("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_",sub("-", "_", years[i]),"_",as.numeric(sub("-", "_", years[i]))+1,"_MORT_2019_PUBLIC.dat"))
  mort_temp = read_CDC_mortality(paste0('/Users/jwang30/Postdoc/Research/All of Us/Data/NHANES/Mortality/',"NHANES_",sub("-", "_", years[i]),"_",as.numeric(sub("-", "_", years[i]))+1,"_MORT_2019_PUBLIC.dat"))
  mort_temp = mort_temp %>% 
    filter(!is.na(mortstat)) %>%
    mutate(fu.time = permth_int/12)
  mort[[i]] = mort_temp
}

saveRDS(mort, '/Users/jwang30/Library/CloudStorage/Box-Box/PhD/Research/All of Us/Data/NHANES/mort.RDS')





