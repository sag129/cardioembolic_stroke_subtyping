#################################
#	 CARDIOEMBOLIC STROKE MANUSCRIPT FEEDBACK
#   
#   Answers questions posed by co-authors for the cardioembolic stroke paper
#     
#################################
library(tidyr)
library(dplyr)
library(lubridate)

### set working directory
# setwd("/data/arrhythmia_source/data/PRESTO/presto rpdr 20180719")



### set working directory
setwd("/data/arrhythmia_source/data/phsBio/phsBio_201812/phenotype/rpdr")

load(file="dia.RData")
load(file="mrn.RData")

dia <- dia %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Date = as.Date(Date,format="%m / %d / %Y"))
mrn <- mrn %>% mutate(EMPI = as.character(Enterprise_Master_Patient_Index),
                      MRN = as.character(MGH_MRN))
# get dx codes
ICD9_dx_codes <- c('433.01','433.11','433.21','433.31','433.81','433.91','434.01','434.11','434.91')
ICD10_dx_codes <- c('I63.00','I63.011','I63.012','I63.019','I63.02','I63.031'
                    ,'I63.032','I63.033','I63.039','I63.09','I63.10','I63.111','I63.112','I63.113','I63.119','I63.12'
                    ,'I63.131','I63.132','I63.133','I63.139','I63.19','I63.20','I63.211','I63.212','I63.219','I63.22'
                    ,'I63.231','I63.232','I63.233','I63.239','I63.29','I63.30','I63.311','I63.312','I63.319','I63.321'
                    ,'I63.322','I63.323','I63.329','I63.331','I63.332','I63.339','I63.341','I63.342','I63.343','I63.349'
                    ,'I63.39','I63.40','I63.411','I63.412','I63.413','I63.419','I63.421','I63.422','I63.423','I63.429'
                    ,'I63.431','I63.432','I63.433','I63.439','I63.441','I63.442','I63.443','I63.449','I63.49','I63.50'
                    ,'I63.511','I63.512','I63.513','I63.519','I63.521','I63.522','I63.523','I63.529','I63.531','I63.532'
                    ,'I63.539','I63.541','I63.542','I63.543','I63.549','I63.59','I63.6','I63.8','I63.9')

# get stroke data based on codes
ICD9_dx_data <- dia %>% filter(Code_Type == "ICD9" & Code %in% ICD9_dx_codes) %>% dplyr::select(EMPI, Date, Code_Type, Code)
ICD10_dx_data <- dia %>% filter(Code_Type == "ICD10" & Code %in% ICD10_dx_codes) %>% dplyr::select(EMPI, Date, Code_Type, Code)

# number of people with stroke dx (n=2360 patients)
stroke_dx_empi <- rbind(ICD9_dx_data, ICD10_dx_data) %>% select(EMPI) %>% distinct()

# 7.7% of patients in PHB have had at least 1 dx of stroke
nrow(stroke_dx_empi)/nrow(mrn)

# stroke diagnoses for PHB data
stroke_dx_data <- rbind(ICD9_dx_data, ICD10_dx_data) %>% select(EMPI, Date) %>% distinct()
names(stroke_dx_data)[names(stroke_dx_data) == "Date"] <- "stroke_date"

# Re-run top 100 lines in [00]0_make_7_cardioembolicStroke_20190823.R for PHB data 

load("dem.RData")
dem <- dem %>% mutate(EMPI = as.character(EMPI))

car$TEE <- as.numeric(car$Report_Description %in% TEE_list)
car$TTE <- as.numeric(car$Report_Description %in% TTE_list)
car$echo_both <- as.numeric(car$Report_Description %in% both_list)

echo_data <- car %>% filter(car$Report_Description %in% txt.files.list) %>% select(EMPI, Report_Description, Report_Date, TEE, TTE, echo_both)

echo_data_empi <- echo_data %>% arrange(EMPI, desc(Report_Date)) %>% group_by(EMPI) %>% filter(row_number() == 1) %>% ungroup(EMPI)

echo_data_2 <- echo_data_empi %>% 
  left_join(dem, by = c('EMPI')) %>%
  mutate(echo_age = difftime(Report_Date,dob, units="day")/365.25) %>%
  left_join(stroke_dx_data, by = c('EMPI')) %>%
  arrange(EMPI, desc(Report_Date), desc(stroke_date)) %>%
  mutate(has_stroke = !is.na(stroke_date)) %>% 
  arrange(EMPI, desc(Report_Date), desc(has_stroke)) %>%
  group_by(EMPI) %>%
  filter(row_number() == 1)

# age at echo
mean(echo_data_2$echo_age)
# Time difference of 61.45021 days
sd(echo_data_2$echo_age)
# [1] 15.69863

# gender
table(echo_data_2$Gender)
# Female   Male 
# 6391   6688
prop.table(table(echo_data_2$Gender))
# Female      Male 
# 0.4886459 0.5113541 

# with icd9, icd10 of stroke
table(echo_data_2$has_stroke)
prop.table(table(echo_data_2$has_stroke))

# cases
# stroke_echo_data <- stroke_dx_data %>% 
#   left_join(echo_data, by=c('EMPI')) %>%
#   mutate(recent_echo = case_when(
#     (feature_date >= stroke_date_index - 28 & feature_date <= stroke_date_index + 28) ~ 1,
#      ,
#     TRUE ~ 0
#     )

# recent myocardial infarction (<4 weeks prior to or after stroke)

# myocardial infarction (>4 weeks, <6 months after stroke)

setwd("/data/arrhythmia_source/data/PRESTO/presto rpdr 20180719")

car_CE$TEE <- as.numeric(car_CE$Report_Description %in% TEE_list)
car_CE$TTE <- as.numeric(car_CE$Report_Description %in% TTE_list)
car_CE$echo_both <- as.numeric(car_CE$Report_Description %in% both_list)
car_CE$none_report <- ifelse(sum(car_CE$TEE,car_CE$TTE,car_CE$echo_both)==0, 1, 0)

sum(sum(car_CE$mitral_stenosis, car_CE$none_report)==0 & car_CE$none_report==1)