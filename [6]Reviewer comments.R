library(dplyr)

#################################
# 6) It is stated that strokes that were classified as 
# "Possible Cardioembolic" by TOAST criteria or 
# "Cardio-Aortic Embolism Possible" by CCS criteria as well 
# as all other strokes not meeting the criteria for CE stroke 
# were considered as non-CE stroke. 

# It is stated in the article that is referenced by Adams et al, 
# 1993 that "A stroke in a patient with a medium-risk cardiac 
# source of embolism and no other cause of stroke is classified 
# as a possible cardioembolic stroke. Taking this into account 
# patients with patent foramen ovale or with atrial septal aneurysm 
# for example were not according to this definition to be included 
# as cardioembolic in this article. Please clarify. 
rm(list = ls())

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide.RData")

# pfo as only cardiac source of embolism
pfo <- feature_table_wide %>% 
  filter(feature_table_wide$pfo == 1)
pfo['RowSum'] <- rowSums(pfo[,7:29], na.rm = T)
pfo_source1 <- pfo[pfo$RowSum == 1, ]

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
table(pfo_source1$Toast, useNA = "always")
#   1    2    3    4    5    6 <NA> 
#   3   82   14   12   13   18   14 

table(pfo_source1$Toast, pfo_source1$CCS, useNA = "always")
#       1  4  6  7 10 13 14 <NA>
# 1     0  1  0  0  0  0  0    2
# 2     1  0 34  4  3  0  1   39
# 3     6  0  1  0  1  0  0    6
# 4     0  0  1  6  0  1  0    4
# 5     1  0  3  0  1  1  0    7
# 6     0  0  2  0  6  0  0   10
# <NA>  2  0  9  1  1  0  1    0

# basic stats for pfo in 1090 dataset
pfo <- feature_table_wide4 %>% 
  filter(feature_table_wide4$pfo == 1)
table(pfo$Toast, useNA = "always")
#    1    2    3    4    5    6 <NA> 
#   50  137   32   24   32   20   20 

table(pfo$Toast, pfo$CCS, useNA = "always")
#       1  2  4  6  7  8 10 11 13 14 <NA>
# 1     1  0 11  5  0  0  0  0  2  2   29
# 2     2  1  0 55 11  0  5  1  0  1   61
# 3     9  1  0  2  1  0  1  0  0  0   18
# 4     1  0  0  3  9  1  1  0  1  0    8
# 5     1  0  0  6  1  0  1  1  3  0   19
# 6     0  1  0  2  0  0  6  0  0  0   11
# <NA>  4  0  3 10  1  0  1  0  0  1    0

rm(list = ls())

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide4.RData")

pfo <- feature_table_wide4 %>% 
  filter(feature_table_wide4$pfo == 1)
pfo['RowSum'] <- rowSums(pfo[,7:29], na.rm = T)
# 137/261 (0.52) are possible CE by TOAST
table(pfo$Toast, useNA = "always")
# 147/261 (0.56) are possible CE by TOAST/CCS
table(pfo$Toast, pfo$CCS, useNA = "always")
# 82/137  (0.60) are possible CE by TOAST and have only 1 cardiac source of embolism
# 0.31 of patients with PFO are possible CE and have only 1 cardiac source of embolism
table(pfo$Toast, pfo$RowSum, useNA = "always")

#####
ce_pfo <- feature_table_wide4 %>% 
  filter(feature_table_wide4$Toast_new==TRUE & feature_table_wide4$pfo == 1)
ce_pfo['RowSum'] <- rowSums(ce_pfo[,7:29], na.rm = T)
sum(ce_pfo['RowSum'] > 1) #50
mean(unlist(ce_pfo['RowSum']))
summary(unlist(ce_pfo['RowSum']))

ce_pfo1 <- ce_pfo %>% filter(ce_pfo$RowSum==1)

# possible ce
possible_ce <- feature_table_wide4 %>% 
  filter(feature_table_wide4$Toast_new==2)

# add to table 1 for reviewer comments
t(data.frame((aggregate(possible_ce[,-c(1:4,6,30)], list(possible_ce$Toast_final), sum))))

table(possible_ce$Toast_new)

# add age to table 1 for reviewer comments
mean(possible_ce$age)
sd(possible_ce$age)

#################################
rm(list = ls())

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide4.RData")

nonce_nopossible <- feature_table_wide4 %>% 
  filter(feature_table_wide4$Toast_final == 0 & feature_table_wide4$Toast != 2)

mean(nonce_nopossible$age)
sd(nonce_nopossible$age)

#################################
# Task: understand the TOAST/CCS breakdown for non-CE & AF patients
# 10) Please comment this statement "Atrial fibrillation was more 
# common among individuals with CE than non-CE stroke (82.3% vs. 10.7%)". 
# If a patient has atrial fibrillation, even if other stroke cause is 
# discovered he qualifies according to the TOAST criteria as 
# "undetermined" due to more than one possible stroke cause. 
# If, like stated in the methods section, patients with undetermined 
# stroke etiology were excluded, how can 10.7% of patients with 
# non-CE stroke have atrial fibrillation? 
#################################

rm(list = ls())

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide4.RData")

table(feature_table_wide4$Toast_final, feature_table_wide4$af)

nonce_af <- feature_table_wide4 %>% 
  filter(feature_table_wide4$Toast_final == 0 & feature_table_wide4$af == 1)

table(nonce_af$Toast, nonce_af$CCS, useNA = "always")
table(nonce_af$Toast_new, useNA = "always")
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

#################################
# Task: find how many patients were adjudicated by TOAST vs. CCS in
# dataset used in primary analysis
#################################

rm(list = ls())

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide4.RData")

table(feature_table_wide4$Toast, useNA = "always")

# 101 rows have TOAST == NA, then these were adjudicated by CCS

#################################
# Task: find how many patients are in both
# MGH ischemic stroke registry and PhsBio repository
#################################
library(tidyr)
library(dplyr)

rm(list = ls())

### set working directory
setwd("/data/arrhythmia_source/data/PRESTO/presto rpdr 20180719")
load(file="mrn.RData")
presto_mrn <- mrn

### set working directory
setwd("/data/arrhythmia_source/data/phsBio/phsBio_201812/phenotype/rpdr")
load(file="mrn.RData")
phsBio_mrn <- mrn

rm(mrn)

both_mrn <- presto_mrn %>% 
  inner_join(phsBio_mrn, by = "Enterprise_Master_Patient_Index")

all.equal(both_mrn$MGH_MRN.x, both_mrn$MGH_MRN.y)
