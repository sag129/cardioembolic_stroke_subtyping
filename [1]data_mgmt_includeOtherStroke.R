# Author: Wyliena Guan
# date: 2019-06-27
# Project Description: Data Management for TOAST-CE Project
# 

### SUPPRESS WARNINGS TO RUN IN BATCH MODE - DISABLE THIS IF WORKING IN INTERACTIVE MODE
options(error = expression(NULL))
rm(list = ls())
# load libraries and data ----
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
library(reshape2)

setwd("/data/arrhythmia_source/data/PRESTO/presto rpdr 20180719")

# cardioembolic stroke code files
ce_codes0 <- read_excel("/data/arrhythmia/wguan/cardioembolic_stroke/algorithm/CE_source_codes_20190920.xlsx", sheet = 1)
# ce_codes_abbrev <- read_excel("/data/arrhythmia/lubitz/af_rpdr_scripts/af_scripts_20190419/CE_source_codes_20190627.xlsx", sheet = 2)
# get stroke from this list
# comorbidities <- read.delim("/data/arrhythmia/lubitz/af_rpdr_scripts/af_scripts_20190419/comorbidities.txt", sep="\t", colClasses="character")

# merge stroke codes together
stroke_codes <- ce_codes0 %>% filter(cov %in% c('mi_recent','mi_later')) %>% dplyr::select(-cov) %>% 
  distinct()

stroke_codes1 <- stroke_codes %>% mutate(cov = 'mi_recent')
stroke_codes2 <- stroke_codes %>% mutate(cov = 'mi_later')

ce_codes <- rbind(ce_codes0[!(ce_codes0$cov %in% c('mi_recent','mi_later')),], stroke_codes1, stroke_codes2)

# ce_codes <- ce_codes %>% 
#   add_row(cov = "af", cov.code.type = "ICD9", cov.code = "427.9", cov.code.description = "Cardiac dysrhythmia, unspecified") %>%
#   add_row(cov = "af", cov.code.type = "ICD10", cov.code = "I49.9", cov.code.description = "Cardiac dysrhythmia, unspecified")

# RPDR data files
load(file="enc.RData")
load(file="dem.RData")
load(file="prc.RData")
load(file="med.RData")
load(file="dia.RData")
load(file="mrn.RData")
load(file="car.RData")
load(file="lab.RData")
load(file="phy.RData")

# table(dem$Gender)
# summary(dem$Age)
# sd(dem$Age)
# dem_final <- dem %>% filter(EMPI %in% features_Toast_na$EMPI)
# table(dem_final$Gender)
# summary(dem_final$Age)
# sd(dem_final$Age)

#####

# preprocess EMPIs, MRNs, dates ----
enc <- enc %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Admit_Date = as.Date(Admit_Date,format="%m / %d / %Y"),
                      Discharge_Date = as.Date(Admit_Date,format="%m / %d / %Y"))
dem <- dem %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Date_of_Birth = as.Date(Date_of_Birth,format="%m / %d / %Y"),
                      Date_Of_Death = as.Date(Date_Of_Death,format="%m / %d / %Y"))
prc <- prc %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Date = as.Date(Date,format="%m / %d / %Y"))
med <- med %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Medication_Date = as.Date(Medication_Date,format="%m / %d / %Y"))
dia <- dia %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Date = as.Date(Date,format="%m / %d / %Y"))
mrn <- mrn %>% mutate(IncomingId = str_pad(as.character(IncomingId), 8, pad = "0"),
                      EMPI = as.character(Enterprise_Master_Patient_Index),
                      MRN = as.character(MGH_MRN))
car <- car %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Report_Date = as.Date(Report_Date_Time,format="%m / %d / %Y %I:%M:%S %p")) # added date variable instead of overwrite date_time var
lab <- lab %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Date = as.Date(Seq_Date_Time,format="%m / %d / %Y %H:%M")) # added date variable instead of overwrite date_time var
phy <- phy %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Date = as.Date(Date,format="%m / %d / %Y"))

# for getting index dates for pts in the PRESTO dataset for PREMISE AF 
presto_ais_data <- read_excel("/data/arrhythmia_source/data/PRESTO/databaseResults_20180613_clinicalOnly/180423 Anderson Lubitz query.xlsx", 
                              sheet = "AIS", col_types = c("text","text", "text", "numeric", "numeric", 
                                                           "numeric", "text", "text", "text", 
                                                           "date", "date", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "numeric", "numeric", "text", 
                                                           "numeric", "numeric", "numeric", 
                                                           "text", "text", "text", "text", "text", 
                                                           "text", "text", "text", "text", "numeric", 
                                                           "numeric", "numeric", "numeric","numeric"))
#### AIS Data Key
# Toast	
# 1	Definite Cardioembolic
# 2	Possible Cardioembolic
# 3	Large Artery
# 4	Small Vessel/Lacunar
# 5	Other
# 6	Undetermined
# 
# 
# CCS	
# 1	Large Artery Atherosclerosis Evident
# 2	Large Artery Atherosclerosis Probable
# 3	Large Artery Atherosclerosis Possible
# 4	Cardio-Aortic Embolism Evident
# 5	Cardio-Aortic Embolism Probable
# 6	Cardio-Aortic Embolism Possible
# 7	Small Artery Occlusion Evident
# 8	Small Artery Occlusion Probable
# 9	Small Artery Occlusion Possible
# 10	Other Uncommon Causes Evident
# 11	Other Uncommon Causes Probable
# 12	Other Uncommon Causes Possible
# 13	Undetermined Causes Unknown
# 14	Undetermined Causes Unclassified

presto <- presto_ais_data %>% mutate(IncomingId = str_pad(as.character(MRN), 8, pad = "0"), stroke_date_index = as.Date(`Date Index Event`)) %>% 
  dplyr::rename(sys_bp = `Systolic BP`, dia_bp = `Diastolic BP`) %>% dplyr::select(IncomingId, stroke_date_index, Height, Weight, sys_bp, dia_bp, Toast, CCS)

# table(presto$Toast,presto$CCS, useNA = c('always'))

presto <- presto %>% left_join(mrn[,c("IncomingId","EMPI")], by="IncomingId")

#####
# patients with missing stroke date index
missing_stroke_date_mrn_empi <- presto %>% filter(is.na(stroke_date_index)) %>% dplyr::select(EMPI)

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

dfr <- rbind(ICD9_dx_data, ICD10_dx_data) %>% filter(EMPI %in% unlist(missing_stroke_date_mrn_empi$EMPI)) %>% 
  arrange(EMPI, Date) %>% as_tibble()

# get earliest stroke per EMPI
dfr <- dfr %>% group_by(EMPI) %>% arrange(EMPI, Date) %>% filter(row_number()==1) %>% dplyr::select(EMPI, Date)
dfr <- left_join(missing_stroke_date_mrn_empi, dfr, by=c("EMPI"))

# substitute missing stroke_index_date in presto (4 dates) with earliest stroke date in dia
presto[presto$EMPI %in% unlist(dfr$EMPI), c("stroke_date_index")] <- dfr$Date

rm(missing_stroke_date_mrn_empi)

# add age, gender
presto <- presto %>% left_join(dem[,c("Gender","EMPI","Date_of_Birth")], by="EMPI") %>%
  mutate(age = (stroke_date_index - Date_of_Birth)/365)
# as.period(stroke_date_index - Date_of_Birth, unit = "years")
# status <-> Date_Of_Death ====

master <- left_join(presto, dem, by=c("EMPI")) %>% dplyr::select(-EPIC_PMRN, -MRN_Type)

# cleaning for height
master <- master %>% mutate(Height = case_when(
  (EMPI == "103541957" & Height == 571) ~ as.character(round(57.1)),
  TRUE ~ Height
))

# MANUSCRIPT FEEDBACK QUESTION - % of patients adjudicated
1-sum((is.na(master$Toast) & is.na(master$CCS)) == TRUE)/nrow(master)

# MANUSCRIPT FEEDBACK QUESTION - % of stroke patients in registry with TEEs/TTEs/other echos
# #############################
# # 		SET FILE VERSION
# vers <- "20190419"
# ##############################
# # # GET CURRENT WORKING DIRECTORY
# pwd <- getwd()
# setwd(paste("/data/arrhythmia_source/lubitz/af_rpdr_scripts/af_scripts_",vers,"/",sep=""))
# 
# # ####### LOAD MANUALLY CURATED CODE FILES
# report_summary_reviewed <- read.csv("report_summary_reviewed.csv", header=TRUE, stringsAsFactors = F)
# colnames(report_summary_reviewed)[2] <- 'Report_Description'
# TEE_list <- report_summary_reviewed[which(report_summary_reviewed$TEE==1),]$Report_Description
# TTE_list <- report_summary_reviewed[which(report_summary_reviewed$TTE==1),]$Report_Description
# both_list <- report_summary_reviewed[which(report_summary_reviewed$Both==1),]$Report_Description
# txt.files.list <- c(TEE_list, TTE_list, both_list)
# 
# setwd(pwd)

# n=1491
# echo <- car %>% filter(((Report_Description %in% TEE_list)|
#                  (Report_Description %in% TTE_list)|
#                  (Report_Description %in% both_list))) %>%
#   select(EMPI) %>% distinct()
# 
# # number of patients without any TEE/TTE/other echo
# # 1598-1491 = 107
# (nrow(mrn) - nrow(echo))/nrow(mrn)
# 
# # tee % in stroke registry
# tee <- car %>% filter(((Report_Description %in% TEE_list))) %>%
#   select(EMPI) %>% distinct()
# (nrow(tee))/nrow(mrn)
# 
# tee <- car %>% filter(((Report_Description %in% TEE_list))) %>% 
#   left_join(master, by=c('EMPI')) %>%
#   mutate(has_echo_28 = case_when(
#     (Report_Date >= stroke_date_index - 28 & Report_Date <= stroke_date_index + 28) ~ 1
#   ),
#   has_echo_90 = case_when(
#     (Report_Date >= stroke_date_index - 90 & Report_Date <= stroke_date_index + 90) ~ 1
#   ),
#   has_echo_28_183 = case_when(
#     (Report_Date >= stroke_date_index + 29 & Report_Date <= stroke_date_index + 183) ~ 1
#   ))
# sum(tee$has_echo_28, na.rm = T)/nrow(mrn)
# sum(tee$has_echo_90, na.rm = T)/nrow(mrn)
# sum(tee$has_echo_28_183, na.rm = T)/nrow(mrn)
# 
# # tte % in stroke registry
# tte <- car %>% filter(((Report_Description %in% TTE_list))) %>%
#   select(EMPI) %>% distinct()
# (nrow(tte))/nrow(mrn)
# 
# tte <- car %>% filter(((Report_Description %in% TTE_list))) %>% 
#   left_join(master, by=c('EMPI')) %>%
#   mutate(has_echo_28 = case_when(
#     (Report_Date >= stroke_date_index - 28 & Report_Date <= stroke_date_index + 28) ~ 1
#   ),
#   has_echo_90 = case_when(
#     (Report_Date >= stroke_date_index - 90 & Report_Date <= stroke_date_index + 90) ~ 1
#   ),
#   has_echo_28_183 = case_when(
#     (Report_Date >= stroke_date_index + 29 & Report_Date <= stroke_date_index + 183) ~ 1
#   ))
# sum(tte$has_echo_28, na.rm = T)/nrow(mrn)
# sum(tte$has_echo_90, na.rm = T)/nrow(mrn)
# sum(tte$has_echo_28_183, na.rm = T)/nrow(mrn)
# 
# other_echo <- car %>% filter(((Report_Description %in% both_list))) %>%
#   select(EMPI) %>% distinct()
# (nrow(other_echo))/nrow(mrn)
# 
# other_echo <- car %>% filter(((Report_Description %in% both_list))) %>% 
#   left_join(master, by=c('EMPI')) %>%
#   mutate(has_echo_28 = case_when(
#     (Report_Date >= stroke_date_index - 28 & Report_Date <= stroke_date_index + 28) ~ 1
#   ),
#   has_echo_90 = case_when(
#     (Report_Date >= stroke_date_index - 90 & Report_Date <= stroke_date_index + 90) ~ 1
#   ),
#   has_echo_28_183 = case_when(
#     (Report_Date >= stroke_date_index + 29 & Report_Date <= stroke_date_index + 183) ~ 1
#   ))
# sum(other_echo$has_echo_28, na.rm = T)/nrow(mrn)
# sum(other_echo$has_echo_90, na.rm = T)/nrow(mrn)
# sum(other_echo$has_echo_28_183, na.rm = T)/nrow(mrn)
# 
# rm(echo, tee, tte, other_echo)

# Table 1 = feature_table
# feature_table
feature_table <- NULL

# add features with more different code.type's first
# Mechanical prosthetic valve bioprosthetic or mechanical valve

# procedure codes
CPT_codes <- ce_codes %>% filter(cov == "mech_bio_valve" & cov.code.type == "CPT") %>% dplyr::select(cov.code)
ICD9_proc_codes <- ce_codes %>% filter(cov == "mech_bio_valve" & cov.code.type == "ICD9 Procedure Code") %>% dplyr::select(cov.code)
ICD10_proc_codes <- ce_codes %>% filter(cov == "mech_bio_valve" & cov.code.type == "ICD10 Procedure Code") %>% dplyr::select(cov.code)
# dx codes
ICD9_dx_codes <- ce_codes %>% filter(cov == "mech_bio_valve" & cov.code.type == "ICD9") %>% dplyr::select(cov.code)
ICD10_dx_codes <- ce_codes %>% filter(cov == "mech_bio_valve" & cov.code.type == "ICD10") %>% dplyr::select(cov.code)

# get data based on codes
CPT_data <- prc %>% filter(Code_Type == "CPT" & Code %in% unlist(CPT_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
ICD9_proc_data <- prc %>% filter(Code_Type == "ICD9" & Code %in% unlist(ICD9_proc_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
ICD10_proc_data <- prc %>% filter(Code_Type == "ICD10" & Code %in% unlist(ICD10_proc_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
ICD9_dx_data <- dia %>% filter(Code_Type == "ICD9" & Code %in% unlist(ICD9_dx_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
ICD10_dx_data <- dia %>% filter(Code_Type == "ICD10" & Code %in% unlist(ICD10_dx_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)

dfr <- bind_rows(CPT_data, ICD9_proc_data, ICD10_proc_data, ICD9_dx_data, ICD10_dx_data) %>% mutate(feature = "mech_bio_valve")

feature_table <- rbind(dfr)

# features based on ICD9, ICD10 codes
feature_list <- ce_codes %>% dplyr::select(cov) %>% distinct() %>% unlist()
feature_list <- feature_list[!(feature_list %in% c("mech_bio_valve"))]

make_dfr <- function(feature) {
  CPT_prc_codes <- ce_codes %>% filter(cov == feature & cov.code.type == "CPT") %>% dplyr::select(cov.code)
  ICD9_prc_codes <- ce_codes %>% filter(cov == feature & cov.code.type == "ICD9 Procedure Code") %>% dplyr::select(cov.code)
  ICD10_prc_codes <- ce_codes %>% filter(cov == feature & cov.code.type == "ICD10 Procedure Code") %>% dplyr::select(cov.code)
  ICD9_dx_codes <- ce_codes %>% filter(cov == feature & cov.code.type == "ICD9") %>% dplyr::select(cov.code)
  ICD10_dx_codes <- ce_codes %>% filter(cov == feature & cov.code.type == "ICD10") %>% dplyr::select(cov.code)
  
  # get data based on codes
  CPT_prc_data <- dia %>% filter(Code_Type == "CPT" & Code %in% unlist(CPT_prc_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
  ICD9_prc_data <- dia %>% filter(Code %in% unlist(ICD9_prc_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
  ICD10_prc_data <- dia %>% filter(Code %in% unlist(ICD9_prc_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
  ICD10_dx_data <- dia %>% filter(Code_Type == "ICD10" & Code %in% unlist(ICD10_dx_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
  ICD9_dx_data <- dia %>% filter(Code_Type == "ICD9" & Code %in% unlist(ICD9_dx_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
  ICD10_dx_data <- dia %>% filter(Code_Type == "ICD10" & Code %in% unlist(ICD10_dx_codes)) %>% dplyr::select(EMPI, Date, Code_Type, Code)
  
  dfr <- bind_rows(CPT_prc_data,ICD9_prc_data,ICD10_prc_data,ICD9_dx_data, ICD10_dx_data) %>% mutate(feature = feature) %>%
    filter(!is.na(Code))
  return(dfr)
}

for (feature in feature_list) {
  dfr <- make_dfr(feature)
  feature_table <- rbind(feature_table, dfr)
}

# add EMPI, MRN, stroke_date_index to long feature table
feature_table_long <- master %>% dplyr::select(EMPI, stroke_date_index) %>% 
  left_join(feature_table, by=c("EMPI")) %>% dplyr::rename(feature_date = Date) %>%
  filter(!is.na(feature_date)) %>%
  dplyr::select(-Code_Type,-Code)

# source("/data/arrhythmia/wguan/cardioembolic_stroke/src/[1]0_make_7_cardioembolicStroke_20190823.R")
load(file="car_NLP1.RData")

car_NLP2 <- presto %>% dplyr::select(EMPI, stroke_date_index) %>% 
  left_join(car_NLP1, by=c("EMPI")) %>% dplyr::rename(feature_date = car_d) %>% 
  filter(!is.na(feature_date)) %>% # some EMPIs do not have any NLP features at all, remove%>%
  dplyr::select(EMPI, stroke_date_index, feature_date, feature)
feature_table_long1 <- bind_rows(feature_table_long, car_NLP2)

feature_table_long2 <- feature_table_long1 %>%
  mutate(has_feature = case_when(
    feature == "mech_bio_valve" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "ms" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "af" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "laa" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    ,feature == "sss" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "mi_recent" & (feature_date >= stroke_date_index - 28 & feature_date <= stroke_date_index + 28) ~ 1
    ,feature == "lvt" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "dc" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "akine_lvs" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "amyxoma" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    ,feature == "infect_ec" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    # medium-risk sources
    ,feature == "mvp" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "mac" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "lat" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    ,feature == "asa" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "pfo" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "afl" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "nonbact_ec" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    ,feature == "chf" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "hypo_lvs" & feature_date <= stroke_date_index + 90 ~ 1
    ,feature == "mi_later" & (feature_date >= stroke_date_index + 29 & feature_date <= stroke_date_index + 183) ~ 1
    # not sure about this, delayed emptying velocity - 90 days before or after stroke
    ,feature == "dev" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    ,feature == "intracard_thromb" & (feature_date >= stroke_date_index - 90 & feature_date <= stroke_date_index + 90) ~ 1
    ,TRUE ~ 0
  )
  )

# get features for each stroke
# feature_table_long3 <- feature_table_long2 %>% filter(has_feature == 1) %>% dplyr::select(-Code, -Code_Type, -feature_date) %>%
#   arrange(EMPI, stroke_date_index, feature, desc(has_feature)) %>% group_by(EMPI, stroke_date_index, feature) %>%
#   filter(row_number()==1) %>% ungroup()

# get features for each stroke - even failed features
feature_table_long3 <- feature_table_long2 %>% dplyr::select(-feature_date) %>%
  arrange(EMPI, stroke_date_index, feature, desc(has_feature)) %>% group_by(EMPI, stroke_date_index, feature) %>%
  filter(row_number()==1) %>% ungroup()

feature_table_wide <- dcast(feature_table_long3, EMPI + stroke_date_index ~ feature, value.var="has_feature")

feature_table_wide[is.na(feature_table_wide)] <- 0

feature_table_wide <- presto %>% dplyr::select(Toast, CCS, EMPI, stroke_date_index, age, Gender) %>% 
  left_join(feature_table_wide[,-c(2)], by=c("EMPI"), suffix=c("",".y")) %>% dplyr::select(-ends_with(".y"))
feature_table_wide <- as.data.frame(feature_table_wide)
row.names(feature_table_wide) <- feature_table_wide$EMPI

sum(is.na(feature_table_wide$Toast)) # 130 missing TOAST values

#!!!!** for patients with af = NA, then this means that there are no qualifying diagnoses for those patients
# for that comorbidity
feature_table_wide[,-c(1:6)][is.na(feature_table_wide[,-c(1:6)])] <- 0

save(feature_table_wide, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide.RData")

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide.RData")
sum(feature_table_wide %>% filter(Toast != 1 & CCS %in% 1:3) %>% select(af))
# ce_codes %>% select(cov) %>% distinct() %>% unlist()

#####
# There are no detected cases of akine_lvs, lat, lvt, mech_valve, mvp, pfo.
#####

features_individ_freq <- data.frame(colSums(feature_table_wide[,-c(1:6)], na.rm = T))

dim(feature_table_wide[,-c(1:6)])

# frequency table of number of features
data.frame(table(rowSums(feature_table_wide[,-c(1:6)])))

# 6.	Stroke types
# a.	Inclusion and definitions
# i.	Cardioembolic = “definite cardioembolic”
# ii.	Noncardioembolic
# 1.	Large artery
# 2.	Small vessel
# 6. b.	Exclusion
# i.	Possible cardioembolic [include in cardioembolic definition in sensitivity analyses]
# ii.	Cryptogenic
# iii.	Unknown
# iv.	Other etiology

table(feature_table_wide$Toast, useNA = "always")

# replace TOAST with CCS if Toast is missing, then recode
#### AIS Data Key
# Toast	
# 1	Definite Cardioembolic
# 2	Possible Cardioembolic
# 3	Large Artery
# 4	Small Vessel/Lacunar
# 5	Other
# 6	Undetermined
# CCS	
# 1	Large Artery Atherosclerosis Evident
# 2	Large Artery Atherosclerosis Probable
# 3	Large Artery Atherosclerosis Possible
# 4	Cardio-Aortic Embolism Evident
# 5	Cardio-Aortic Embolism Probable
# 6	Cardio-Aortic Embolism Possible
# 7	Small Artery Occlusion Evident
# 8	Small Artery Occlusion Probable
# 9	Small Artery Occlusion Possible
# 10	Other Uncommon Causes Evident
# 11	Other Uncommon Causes Probable
# 12	Other Uncommon Causes Possible
# 13	Undetermined Causes Unknown
# 14	Undetermined Causes Unclassified

# Given previously demonstrated correlation of the TOAST and CCS schemes,
# we defined cardioembolic events as follows: definite cardioembolic stroke 
# (“Definite Cardioembolic” by TOAST criteria, “Cardio-Aortic Embolism Evident” or “Cardio-Aortic Embolism Probable” by CCS criteria), and 
# possible cardioembolic stroke (“Possible Cardioembolic” by TOAST criteria or “Cardio-Aortic Embolism Possible” by CCS criteria).

feature_table_wide3 <- feature_table_wide %>% 
  mutate(Toast_new = case_when(is.na(Toast) & CCS %in% 1:3 ~ 3,
                               is.na(Toast) & CCS %in% 7:9 ~ 4,
                               is.na(Toast) & CCS %in% 4:5 ~ 1,
                               is.na(Toast) & CCS %in% 6 ~ 2,
                               is.na(Toast) & CCS %in% 10:12 ~ 5,
                               is.na(Toast) & CCS %in% 13:14 ~ 6,
                               !is.na(Toast) ~ Toast))
table(feature_table_wide3$Toast_new,feature_table_wide3$CCS, useNA = "always")
nrow(feature_table_wide3)

save(feature_table_wide3, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide3.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide3.RData")


# remove
# i.	Possible cardioembolic [include in cardioembolic definition in sensitivity analyses]
feature_table_wide4 <- feature_table_wide3 %>% 
  mutate(Toast_final = ifelse(Toast_new == 1, 1, 0),
         gender = ifelse(Gender == "Male", 1, 0) #,
         # nonCE_pCE_CE = case_when(Toast_new %in% 3:6 ~ 0,
         #                     Toast_new %in% 2 ~ 1,
         #                     Toast_new %in% 1 ~ 2)
         )
names(feature_table_wide4)

save(feature_table_wide4, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")


# Table - Baseline characteristics of stroke sample stratified by stroke mechanism
# all group
data.frame(colSums(feature_table_wide4[,-c(1:5,6,30)]))
data.frame(colSums(feature_table_wide4[,-c(1:5,6,30)])/nrow(feature_table_wide4))
t(data.frame((aggregate(feature_table_wide4[,-c(1:4,6,30)], list(feature_table_wide4$Toast_final), sum))))
t(data.frame((aggregate(feature_table_wide4[,-c(1:4,6,30)], list(feature_table_wide4$nonCE_pCE_CE), sum))))
nrow(feature_table_wide4)

mean(feature_table_wide4$age)
sd(feature_table_wide4$age)
table(feature_table_wide4$Gender)

table(feature_table_wide4$Toast_final)

# mean(feature_table_wide4[feature_table_wide4$nonCE_pCE_CE==0,]$age)
# sd(feature_table_wide4[feature_table_wide4$nonCE_pCE_CE==0,]$age)
# 
# mean(feature_table_wide4[feature_table_wide4$nonCE_pCE_CE==1,]$age)
# sd(feature_table_wide4[feature_table_wide4$nonCE_pCE_CE==1,]$age)
# 
# mean(feature_table_wide4[feature_table_wide4$nonCE_pCE_CE==2,]$age)
# sd(feature_table_wide4[feature_table_wide4$nonCE_pCE_CE==2,]$age)


mean(feature_table_wide4[feature_table_wide4$Toast_final==0,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_final==0,]$age)

mean(feature_table_wide4[feature_table_wide4$Toast_final==1,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_final==1,]$age)

mean(feature_table_wide4[feature_table_wide4$Toast_new==2,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_new==2,]$age)

mean(feature_table_wide4[feature_table_wide4$Toast_new==3,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_new==3,]$age)

mean(feature_table_wide4[feature_table_wide4$Toast_new==4,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_new==4,]$age)

mean(feature_table_wide4[feature_table_wide4$Toast_new==5,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_new==5,]$age)

mean(feature_table_wide4[feature_table_wide4$Toast_new==6,]$age)
sd(feature_table_wide4[feature_table_wide4$Toast_new==6,]$age)


table(feature_table_wide4$Toast_new,feature_table_wide4$CCS, useNA = "always")
table(feature_table_wide4$Toast_final,feature_table_wide4$CCS, useNA = "always")

feature_table_wide4['RowSum'] <- rowSums(feature_table_wide4[,c(7:29)])
data.frame(table(feature_table_wide4$RowSum))
table(feature_table_wide4$RowSum, feature_table_wide4$Toast_final)

table(feature_table_wide4$Toast_new)
t(data.frame((aggregate(feature_table_wide4[,-c(1:4,6,30)], list(feature_table_wide4$Toast_new), sum))))
table(feature_table_wide4$RowSum, feature_table_wide4$Toast_new)
