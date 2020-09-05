#################################
#	 CARDIOEMBOLIC STROKE FEATURE EXTRACTION
#   
#   pulls cardioembolic stroke features based on
# Adams Jr, Harold P., et al. "Classification of subtype of acute ischemic stroke. Definitions for use in a multicenter clinical trial. TOAST. Trial of Org 10172 in Acute Stroke Treatment." Stroke 24.1 (1993): 35-41.
#     
#     
#################################
# https://cran.r-project.org/mirrors.html
# install.packages("stringr", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("stringi", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("tokenizers", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("foreach", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("doParallel", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("tm", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("corpus", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("officer", repos = "http://cran.cnr.berkeley.edu/")
# install.packages("klaR", repos = "http://cran.cnr.berkeley.edu/")


# set up library path and libraries
.libPaths( c( .libPaths(), "/PHShome/wg985/R/x86_64-pc-linux-gnu-library/3.5", "/opt/rstudio-r/R-3.5.0/lib/R/library"))

### SUPPRESS WARNINGS TO RUN IN BATCH MODE - DISABLE THIS IF WORKING IN INTERACTIVE MODE
options(error = expression(NULL))
rm(list = ls())

library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(tokenizers)
library(corpus)
library(tm)


### set working directory
setwd("/data/arrhythmia_source/data/phsBio/phsBio_201812/phenotype/rpdr")
# setwd("/data/arrhythmia_source/data/PRESTO/presto rpdr 20180719")

#############################
# 		SET FILE VERSION
vers <- "20190419"
##############################

### CREATE README FILE TO INDICATE VERSION
sink(file="out_make7_README_wg.txt",split=F,append=F)
print(paste("made using version af_scripts_",vers,sep=""))
sink()

# need the following named files:
# car

load(file="car.RData")
load(file="mrn.RData")

# setwd("/data/arrhythmia_source/data/PRESTO/presto rpdr 20180719")
# mrn <- mrn %>% mutate(IncomingId = str_pad(as.character(IncomingId), 8, pad = "0"),
#                       EMPI = as.character(Enterprise_Master_Patient_Index))
mrn <- mrn %>% mutate(EMPI = as.character(Enterprise_Master_Patient_Index))
car <- car %>% mutate(EMPI = as.character(EMPI),
                      MRN = as.character(MRN),
                      Report_Date = as.Date(Report_Date_Time,format="%m / %d / %Y %I:%M:%S %p")) # added date variable instead of overwrite date_time var

# # GET CURRENT WORKING DIRECTORY
pwd <- getwd()
setwd(paste("/data/arrhythmia_source/lubitz/af_rpdr_scripts/af_scripts_",vers,"/",sep=""))

# ####### LOAD MANUALLY CURATED CODE FILES
report_summary_reviewed <- read.csv("report_summary_reviewed.csv", header=TRUE, stringsAsFactors = F)
colnames(report_summary_reviewed)[2] <- 'Report_Description'
TEE_list <- report_summary_reviewed[which(report_summary_reviewed$TEE==1),]$Report_Description
TTE_list <- report_summary_reviewed[which(report_summary_reviewed$TTE==1),]$Report_Description
both_list <- report_summary_reviewed[which(report_summary_reviewed$Both==1),]$Report_Description
txt.files.list <- c(TEE_list, TTE_list, both_list)

setwd(pwd)

sum(car$Report_Description %in% TEE_list)
sum(car$Report_Description %in% TTE_list)
sum(car$Report_Description %in% both_list)
sum(car$Report_Description %in% txt.files.list)
# set number of cores
# numCores <- detectCores(all.tests = FALSE, logical = TRUE)
# registerDoParallel(numCores)

# add primary key column
car$primary_key <- 1:nrow(car)
# add year column (based on car_d)
car$Report_Year <- year(car$car_d)

##### TEXT PROCESSING CODE
corpus.txt <- car$strung

# docs.transf <- VCorpus(VectorSource(corpus.txt))
# docs.transf <- tm_map(docs.transf, content_transformer(tolower))
# 
# newWords <- stopwords("english")
# keep <- c("no", "not", "can't", "cannot", "isn't", "aren't", "wasn't", "weren't",
#           "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't",
#           "there", "and", "without") # include "there", "and" to show presence of new ideas, "without" to allow negation to remain present
# newWords <- newWords [!newWords %in% keep]
# 
# docs.transf <- tm_map(docs.transf, removeWords, newWords)
# rm(newWords, keep)
# 
# transform.wors <- content_transformer(function(x, from, to) gsub(from, to, x))
# docs.transf <- tm_map(docs.transf, transform.wors, "_", " ")
# docs.transf <- tm_map(docs.transf, transform.wors, "--", "")
# docs.transf <- tm_map(docs.transf, transform.wors, ":", ": ")
# docs.transf <- tm_map(docs.transf, transform.wors, ",", ", ")
# docs.transf <- tm_map(docs.transf, transform.wors, "\\?+", "? ")
# 
# docs.transf <- tm_map(docs.transf, transform.wors, "won't", "will not")
# docs.transf <- tm_map(docs.transf, transform.wors, "can't", "can not")
# docs.transf <- tm_map(docs.transf, transform.wors, "cannot", "can not")
# docs.transf <- tm_map(docs.transf, transform.wors, "n't", " not")
# docs.transf <- tm_map(docs.transf, transform.wors, "'ll", " will")
# docs.transf <- tm_map(docs.transf, transform.wors, "'re", " are")
# docs.transf <- tm_map(docs.transf, transform.wors, "'ve", " have")
# docs.transf <- tm_map(docs.transf, transform.wors, "'m", " am")
# docs.transf <- tm_map(docs.transf, transform.wors, "'d", " would")
# # 's could be 'is' or could be possessive: it has no expansion
# docs.transf <- tm_map(docs.transf, transform.wors, "'", "")
# 
# docs.transf <- tm_map(docs.transf, stripWhitespace)
# 
# docs.transf <- data.frame(text=unlist(sapply(docs.transf, `[`, "content")), stringsAsFactors=F)
# 
# docs.transf_tm <- as.data.frame(sapply(docs.transf, function(x) str_squish(x)))
# docs.transf_tm <- as.character(docs.transf_tm$text)
# save(docs.transf_tm, file = "car_docs.transf_tm.RData")
# #####
# 
docs.transf <- tolower(corpus.txt)
docs.transf <- str_replace_all(docs.transf, "_", replacement = " ")
docs.transf <- str_replace_all(docs.transf, "--", replacement = "") # want to keep neg sign
docs.transf <- str_replace_all(docs.transf, ":", replacement = ": ")
docs.transf <- str_replace_all(docs.transf, ";", replacement = "; ")
docs.transf <- str_replace_all(docs.transf, ",", replacement = ", ")
docs.transf <- str_replace_all(docs.transf, "\\?+", replacement = "? ")

# expand contractions in an English-language source
docs.transf <- str_replace_all(docs.transf, "won't", replacement = "will not")
docs.transf <- str_replace_all(docs.transf, "can't", replacement = "can not")
docs.transf <- str_replace_all(docs.transf, "cannot", replacement = "can not")
docs.transf <- str_replace_all(docs.transf, "n't", replacement = " not")
docs.transf <- str_replace_all(docs.transf, "'ll", replacement = " will")
docs.transf <- str_replace_all(docs.transf, "'re", replacement = " are")
docs.transf <- str_replace_all(docs.transf, "'ve", replacement = " have")
docs.transf <- str_replace_all(docs.transf, "'m", replacement = " am")
docs.transf <- str_replace_all(docs.transf, "'d", replacement = " would")
# 's could be 'is' or could be possessive: it has no expansion
docs.transf <- str_replace_all(docs.transf, "'s", replacement = "")
#
# docs.transf <- str_squish(docs.transf)
docs.transf <- as.data.frame(sapply(docs.transf, function(x) str_squish(x)))
docs.transf <- as.character(docs.transf$`sapply(docs.transf, function(x) str_squish(x))`)
save(docs.transf, file = "car_docs.transf.RData")
#####

#### load transformed text file
# load(file="car_docs.transf_tm.RData")
load(file="car_docs.transf.RData")

####===== source_vs_feature$mitral_stenosis #1=====
feature <- "mitral_stenosis"
car$mitral_stenosis <- 0
ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,100})" # all characters without period
ch_sm <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,10})" # all characters without period
# dummy_data <- str_remove_all(docs.transf, "(reason[:space:]*)([:print:])*mitral stenosis") # ignore "Reason CHF;Mitral Stenosis;"
dummy_data <- str_remove_all(docs.transf, "(reason[:space:]*)([:print:]){0,100}mitral\\s+stenosis") # ignore "Reason CHF;Mitral Stenosis;"
dummy_data <- str_remove_all(dummy_data, "(mitral\\s+stenosis)([:print:]){0,100}icd-") # ignore "Diagnosis: Rheumatic mitral stenosis [I05.0 (ICD-10-CM)]"
dummy_data <- str_remove_all(dummy_data, paste0("eval\\w*",ch,"mitral",ch,"stenosis")) # evaluate
dummy_data <- str_remove_all(dummy_data, paste0("exclude",ch,"mitral",ch,"stenosis")) # exclude
dummy_data <- str_remove_all(dummy_data, paste0("assess",ch,"mitral",ch,"stenosis")) # assess
dummy_data <- str_remove_all(dummy_data, paste0("aortic stenosis")) # aortic stenosis

b1 <- which(str_detect(dummy_data, "mitral\\s+stenosis")) # find phrases of "mitral stenosis", calcific mitral stenosis
c1 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"mitral\\s+stenosis")))
c2 <- which(str_detect(dummy_data, paste0("(without )",ch,"mitral\\+stenosis")))
c3 <- which(str_detect(dummy_data, paste0("mitral",ch,"without",ch_sm,"stenosis")))
b2 <- Reduce(union, list(c1,c2,c3))
b3 <- intersect(b1,b2) # find common set
ret <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)
car$mitral_stenosis[ret] <- 1

sum(car$mitral_stenosis)
rm(dummy_data)
rm(b1,b2,b3, c1,c2, ret)

####===== source_vs_feature$left_atrial_appendage_thrombus #2=====
feature <- "left_atrial_appendage_thrombus"
car$left_atrial_appendage_thrombus <- 0
ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:;?]){0,100})" # all characters without period
ch_sm <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,30})" # all characters without period

dummy_data <- str_remove_all(docs.transf, paste0("reason",ch,"(left\\s+atrial\\s+appendage|laa|la\\s+appendage) (clot|th(r)?ombus)")) # Reason...:...Left Atrial Appendage th(r)?ombus
dummy_data <- str_remove_all(dummy_data, paste0("diagnosis",ch,"(left\\s+atrial\\s+appendage|laa|la\\s+appendage) (clot|th(r)?ombus)")) # Diagnosis...:...Left Atrial Appendage th(r)?ombus
dummy_data <- str_remove_all(dummy_data, paste0("rule out",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage) (clot|th(r)?ombus)")) # rule out
dummy_data <- str_remove_all(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage) (clot|th(r)?ombus)",ch_sm,"rule out")) # rule out
dummy_data <- str_remove_all(dummy_data, paste0("(clot|th(r)?ombus)",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch_sm,"rule(d)? out")) # rule out
dummy_data <- str_remove_all(dummy_data, paste0("eval\\w*",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(clot|th(r)?ombus)")) # evaluate
dummy_data <- str_remove_all(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"eval\\w*",ch_sm,"(clot|th(r)?ombus)")) # evaluate
dummy_data <- str_remove_all(dummy_data, paste0("exclude",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(clot|th(r)?ombus)")) # exclude
dummy_data <- str_remove_all(dummy_data, paste0("exclude",ch,"(clot|th(r)?ombus)",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")) # exclude
dummy_data <- str_remove_all(dummy_data, paste0("assess",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(clot|th(r)?ombus)")) # assess
dummy_data <- str_remove_all(dummy_data, paste0("indications",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(clot|th(r)?ombus)")) # assess
dummy_data <- str_remove_all(dummy_data, paste0("clinical history",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(clot|th(r)?ombus)")) # clinical history
dummy_data <- str_remove_all(dummy_data, paste0("information requested",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(clot|th(r)?ombus)")) # info requested
dummy_data <- str_remove_all(dummy_data, paste0("low risk",ch_sm,"thromb(u|i)",ch_sm,"formation")) # info requested


## regex 1 (left atrial appendage < th(r)?ombus)
b1 <- which(str_detect(dummy_data, paste0("left atrial",ch,"(appendage )?",ch,"th(r)?ombus"))) # find phrases of "left atrial appendage th(r)?ombus"
b2 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"th(r)?ombus")))
bb <- Reduce(union, list(b1,b2))
c1 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(appendage)?",ch,"th(r)?ombus"))) # no
c2 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"left atrial",ch,"(appendage)?",ch,"th(r)?ombus"))) # find phrases of "left atrial appendage th(r)?ombus"
c3 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch_sm,"appendage",ch_sm,"th(r)?omb(u|i)")))
c4 <- which(str_detect(dummy_data, paste0("(without )",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"th(r)?ombus"))) # without
c5 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(no|not|normal)",ch,"th(r)?ombus"))) # left atrial appendage <(no|not) th(r)?ombus
c6 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(without)",ch,"th(r)?ombus"))) # left atrial appendage without th(r)?ombus
c7 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(free)",ch,"th(r)?ombus"))) # left atrial appendage free of th(r)?ombus
# Left atrial appendage th(r)?ombus cannot be excluded by this technique
# LA  appendage thrombus cannot be excluded by this technique.
c8 <- which(str_detect(dummy_data,paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage) th(r)?ombus",ch,"(can|could)\\s+(not)",ch,"excluded"))) # ignore "Left atrial appendage th(r)?ombus cannot be excluded" phrases in "CARDIAC ULTRASOUND.txt"
c9 <- which(str_detect(dummy_data, paste0("ruled out",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch_sm,"(th(r)?ombus)"))) # rule out
c10 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch_sm,"(th(r)?ombus)",ch_sm,"ruled out"))) # rule out
cc <- Reduce(union, list(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10))
b3 <- intersect(bb,cc) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)

dummy_data <- str_remove_all(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage) (clot|th(r)?ombus)",ch_sm,"rule? out")) # rule out

car$left_atrial_appendage_thrombus[b4] <- 1

# regex 2 (th(r)?ombus < left atrial appendage)
b1 <- which(str_detect(dummy_data, paste0("th(r)?omb(u|i)",ch,"left",ch,"(atrial|atrium)",ch_sm,"appendage"))) # find phrases of "th(r)?omb* in left atrial appendage"
b2 <- which(str_detect(dummy_data, paste0("th(r)?omb(u|i)",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")))
bb <- Reduce(union, list(b1,b2))
c1 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"th(r)?omb(u|i)",ch,"left ",ch,"(atrial|atrium)",ch_sm,"(appendage)?")))
c2 <- which(str_detect(dummy_data, paste0("th(r)?omb(u|i)",ch,"(no\\w* )",ch,"left ",ch,"(atrial|atrium)",ch_sm,"(appendage)?")))
c3 <- which(str_detect(dummy_data, paste0("th(r)?omb(u|i)",ch,"(without)",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")))
c4 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"th(r)?ombus",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")))
c5 <- which(str_detect(dummy_data, paste0("(without)",ch,"th(r)?ombus",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")))
c6 <- which(str_detect(dummy_data, paste0("th(r)?ombus",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"resolved")))
c7 <- which(str_detect(dummy_data, paste0("ruled out",ch_sm,"(th(r)?ombus)",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)"))) # rule out
cc <- Reduce(union, list(c1,c2,c3,c4,c5,c6,c7))
b3 <- intersect(bb,cc) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)

car$left_atrial_appendage_thrombus[b4] <- 1

## regex 3 (left atrial appendage < clot)
b1 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"clot"))) # find phrases of "left atrial appendage th(r)?ombus"
c1 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"clot")))
c2 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(no\\w* )",ch,"clot")))
c3 <- which(str_detect(dummy_data, paste0("(without\\w* )",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"clot")))
c4 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(without\\w* )",ch,"clot")))
c5 <- which(str_detect(dummy_data, paste0("ruled out",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch_sm,"(clot)"))) # rule out
cc <- Reduce(union, list(c1,c2,c3,c4,c5))
b3 <- intersect(b1,cc) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)

car$left_atrial_appendage_thrombus[b4] <- 1

# regex 4 (clot < left atrial appendage)
b1 <- which(str_detect(dummy_data, paste0("clot",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")))
c1 <- which(str_detect(dummy_data, paste0("(no\\w* )",ch,"clot",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)")))
c2 <- which(str_detect(dummy_data, paste0("(without\\w* )",ch,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"clot")))
c3 <- which(str_detect(dummy_data, paste0("((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)",ch,"(without\\w* )",ch,"clot")))
c4 <- which(str_detect(dummy_data, paste0("rule(d)? out",ch_sm,"(clot)",ch_sm,"((left\\s+atrial\\s+(appendage)?)|laa|la\\s+appendage)"))) # rule out
cc <- Reduce(union, list(c1,c2,c3,c4))
b3 <- intersect(b1,cc) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)

car$left_atrial_appendage_thrombus[b4] <- 1

sum(car$left_atrial_appendage_thrombus)
# car$left_atrial_appendage_thrombus[222325]
# car$left_atrial_appendage_thrombus[229420]
# car$left_atrial_appendage_thrombus[234280]
# car$left_atrial_appendage_thrombus[386741]
# car$left_atrial_appendage_thrombus[53694]
# car$left_atrial_appendage_thrombus[143188]
# car$left_atrial_appendage_thrombus[29961]
# car$left_atrial_appendage_thrombus[158400]
# car$left_atrial_appendage_thrombus[383346]
# car$left_atrial_appendage_thrombus[136660]
# car$left_atrial_appendage_thrombus[332252]
# car$left_atrial_appendage_thrombus[399176]
# 
# car$left_atrial_appendage_thrombus[346276]
# car$left_atrial_appendage_thrombus[66472]
# car$left_atrial_appendage_thrombus[233861]
# car$left_atrial_appendage_thrombus[29961] #<- 0
# car$left_atrial_appendage_thrombus[399658] #<- 0
# car$left_atrial_appendage_thrombus[271786]
# car$left_atrial_appendage_thrombus[431097] #<- 0 - hard to change 1
# car$left_atrial_appendage_thrombus[164454] #<- 0
# car$left_atrial_appendage_thrombus[412175] # 1
# car$left_atrial_appendage_thrombus[43807]
# car$left_atrial_appendage_thrombus[323517]
# car$left_atrial_appendage_thrombus[25658] #<- 0
# 
# dummy_data[143188]
# dummy_data[25658]

rm(b1,b2,b3,b4,c1,c2,c3,c4,c5,c6,bb,cc, dummy_data)

####===== source_vs_feature$akinetic_left_ventricular_segment #3=====
feature <- "akinetic_left_ventricular_segment"
car$akinetic_left_ventricular_segment <- 0

# Remove "Wall Motion Abbreviations: N=Normal H=Hypokinetic A=Akinetic D=Dyskinetic"
dummy_data <- str_remove_all(docs.transf, "a=akinetic")
dummy_data <- str_remove_all(dummy_data, "h=hypokinetic")
dummy_data <- str_remove_all(dummy_data, "d=dyskinetic")
dummy_data <- str_remove_all(dummy_data, "2\\s*-\\s*hypokinesis")
dummy_data <- str_remove_all(dummy_data, "3\\s*-\\s*akinesis")
dummy_data <- str_remove_all(dummy_data, "4\\s*-\\s*dyskinesis")


b1 <- which(str_detect(dummy_data, "akine")) # find phrases of "akinesia"
# b2 <- which(str_detect(dummy_data, "hypokine")) # find phrases of "hypokinesia" # without hypokine in this version
b3 <- which(str_detect(dummy_data, "dyskine")) # find phrases of "dyskinesia"
# b4 <- Reduce(union, list(b1,b2,b3))
b4 <- Reduce(union, list(b1,b3))

detect_within_paragr <- function(heading_pat, feature_pat, sentences) {
  paste0("heading_pat=",heading_pat)
  paste0("feature_pat=",feature_pat)
  # heading_pat <- "left ventr"
  # feature_pat <- "hypokine"

  c1 <- str_detect(tolower(sentences), heading_pat) # get sentences with "left ventr"
  if(sum(c1) == 0) return(0)
  ## !! min() logic here is for clearing through warnings(); arbitrary, guessing that left ventr is early on in the cardiac report; still need to validate a bit
  c2 <- which(c1 == T) # get location of "left ventr" from sentences
  c3 <- str_detect(tolower(sentences), feature_pat) # detect which sentence contains "akine"
  if(sum(c3) == 0) return(0)
  c4 <- which(c3 == T) # get location of akine from sentences
  
  # N=Normal H=Hypokinetic A=Akinetic D=Dyskinetic
  d1 <- str_detect(tolower(sentences), paste0("=",feature_pat))
  d2 <- which(d1 == T)
  d3 <- intersect(c4, d2)
  c4 <- setdiff(c4, d3)
  if (sum(c4) == 0) return(0)
  
  # negation
  ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],]){0,100})" # all characters without period
  d1 <- str_detect(tolower(sentences), paste0("(no(t)? )",ch,feature_pat))
  d2 <- which(d1 == T)
  d3 <- intersect(c4, d2)
  c4 <- setdiff(c4, d3)
  if (sum(c4) == 0) return(0)

    # get location of these headings
  # pericardial disease and extracardiac masses; "interatrial septum, interventricular septum"
  headings <- "venous|pulmonic valve|pericardium|interatrial septum|conclusions|dyssynchrony|pericardial disease|pulmonary valve|right ventr|interventricular septum|left atrium|right atrium|tricuspid valve|aortic valve|mitral valve|interatrial septum|report_end"
  c5 <- str_detect(tolower(sentences), headings) # get sentences with section headings or report_end
  if(sum(c5) == 0) return(0)
  c6 <- which(c5 == T)
  
  # find all possible cases of (c2 <= c4) && (c4 < c6)
  # c2 <= c4
  c7 <- c()
  for (c4i in c4) {
    # make sure left ventr heading < akine/hypokine/dyskine keyword
    c2_lteq_c4 <- sapply(c2, function(x) {x <= c4i})
    # check if left ventr heading < new heading < akine/hypokine/dyskine keyword
    c2_lteq_c6_lteq_c4 <- sapply(c6, function(x) {c2 <= x && x <= c4i})
    if (sum(c2_lteq_c6_lteq_c4) > 0) {
      c7 <- c(c7,0)
      next
    }
    # make sure akine/hypokine/dyskine keyword < other headings
    c4_lteq_c6 <- sapply(c6, function(x) {c4i < x})
    c7 <- c(c7, sum(c2_lteq_c4) && sum(c4_lteq_c6))
  }
  # not sure what this next line of code does or if possible
  # if(!(min(c2) < max(c6))) return(0)
  # c7 <- min(c6[c2 < c6]) # index of first heading after "left ventr"
  # c8 <- (c2 <= c4) && (c4 < c6) # left ventr <= akine && akine < [next heading]
  return(sum(c7) > 0)
}

for (k in b4) {
  # return to original corpus (e.g. "LEFT VENTRICLE:...", "RIGHT ATRIUM:...")
  sentences <- tokenize_sentences(corpus.txt[k], simplify = T)
  
  c6 <- detect_within_paragr("left ventr", "akine", sentences)
  #c66 <- detect_within_paragr("left ventr", "hypokine", sentences) # repeat for hypokinesia
  c666 <- detect_within_paragr("left ventr", "dyskine", sentences) # repeat for dyskinesia
  
  # ret <- sum(sum(c6, c66, c666) > 0)
  ret <- sum(sum(c6, c666) > 0)
  car$akinetic_left_ventricular_segment[k] <- ret
}
sum(car$akinetic_left_ventricular_segment)
rm(ret,c6,c66,c666, b1,b2,b3,b4)

# test <- foreach(i = b4, .combine = rbind, .errorhandling = 'pass') %dopar% {
#   # return to original corpus
#   sentences <- tokenize_sentences(corpus.txt[i], simplify = T)
#   
#   c6 <- detect_within_paragr("left ventr", "akine", sentences)
#   c66 <- detect_within_paragr("left ventr", "hypokine", sentences) # repeat for hypokinesia
#   c666 <- detect_within_paragr("left ventr", "dyskine", sentences) # repeat for dyskinesia
#   
#   ret <- sum(sum(c6, c66, c666) > 0)
#   # print(paste("i = ", i))
#   # print(paste("ret = ", ret))
#   car$akinetic_left_ventricular_segment[i] <- ret
#   c(i, ret)
# }
# 
# test1 <- as.data.frame(test)
# foreach(j = 1:nrow(test1)) %do% {
#   car$akinetic_left_ventricular_segment[test1$V1[j]] <- test1$V2[j]
# }
# 
# rm(test, test1, b1,b2,b3,b4)

####===== source_vs_feature$mitral_valve_prolapse #4=====
feature <- "mitral_valve_prolapse"
car$mitral_valve_prolapse <- 0

# remove neutral terms
dummy_data <- str_remove_all(docs.transf, "(evaluate )([\\w]*\\s){0,3}mitral valve prolapse") # ignore "evaluate for mitral valve prolapse" phrases

b1 <- which(str_detect(dummy_data, "(mitral )([\\w]*\\s){1,10}prolapse*")) # find phrases of "mitral valve prolapse"
b2a <- which(str_detect(dummy_data, "(no\\w* )([\\w]*\\s){1,6}prolapse*"))
b2b <- which(str_detect(dummy_data, "mitral valve ([\\w]*\\s){1,6}without ([\\w]*\\s){1,6}prolapse"))
b2c <- which(str_detect(dummy_data, "mitral valve.*minimal.*prolapse"))
b2d <- which(str_detect(dummy_data, "borderline mitral valve prolapse"))
b2e <- which(str_detect(dummy_data, "does not meet criteria for mitral valve prolapse"))
b2f <- which(str_detect(dummy_data, "normal mitral valve"))
b2 <- Reduce(union, list(b2a,b2b,b2c,b2d,b2e,b2f))
b3 <- intersect(b1,b2) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)

car$mitral_valve_prolapse[b4] <- 1
rm(b1,b2,b3,b4,b2a,b2b,b2c,b2d,b2e,b2f,dummy_data)


####===== source_vs_feature$mitral_annulus_calcification #5=====
feature <- "mitral_annulus_calcification"
car$mitral_annulus_calcification <- 0
ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],]){0,100})" # all characters without period
ch_sm <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],]){0,10})" # all characters without period

# mitral annul* < calcif*
b1 <- which(str_detect(docs.transf, paste0("(mitral )", ch, "annul*",ch,"calcif*")))
b2 <- which(str_detect(docs.transf, paste0("(no\\w* )",ch_sm,"(mitral )", ch, "annul*",ch,"calcif*")))
b3 <- intersect(b1,b2) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with no (we couldn't find with b1)

# car$mitral_annulus_calcification[b4] <- 1

# calcif* < mitral annul*
# ex: There is calcification of the anterior and posterior mitral annulus
c1 <- which(str_detect(docs.transf, paste0("calcif*",ch,"mitral ",ch,"annul*")))
c2 <- which(str_detect(docs.transf, paste0("(no\\w* )",ch_sm,"calcif*",ch,"mitral ",ch,"annul*")))
c3 <- intersect(c1,c2) # find common set
c4 <- setdiff(c1, c3) # find subset of phrases with no (we couldn't find with b1)

d4 <- Reduce(union, list(b4,c4))
car$mitral_annulus_calcification[d4] <- 1
rm(b1,b2,b3,b4, c1,c2,c3,c4, ch,ch_sm,d4)

####===== source_vs_feature$left_atrial_turbulence #6=====
feature <- "left_atrial_turbulence"
car$left_atrial_turbulence <- 0

# find smoke near laa text, since a person could be a smoke cigarettes
b0 <- which(car$Report_Description %in% c(TEE_list,both_list))
b1 <- which(str_detect(docs.transf, "smoke\\b")) # find phrases of "smoke" (n=35)
b2 <- intersect(b0, b1) # some echo's are actually TEEs; stop intersection to capture lg # of echo's

for (i in b1) {
  ret <- 0
  sentences <- unlist(str_extract_all(docs.transf[i], "([^[\\.!?]&&[:print:]]*[\\.!?])"))
  c1 <- which(str_detect(sentences, "smoke\\b")) # find which sentence contains "smoke"
  d1 <- which(str_detect(sentences, "(no\\w*\\b).*smoke")) # find phrases of "no...smoke"
  d2 <- which(str_detect(sentences, "(negative\\b).*smoke")) # find phrases of "negative...smoke"
  c2 <- union(d1,d2)
  c3 <- intersect(c1,c2) # find common set
  c4 <- setdiff(c1,c3) # find subset of phrases with no (we couldn't find with b1)
  if(sum(c4)==0) next
  
  c5 <- str_detect(sentences[c4], "left atr") # detect if there is "left atr" in all of the sentences w "smoke"
  c6 <- str_detect(sentences[c4], "laa")
  c7 <- str_detect(sentences[c4], "la appendage")
  c8 <- str_detect(sentences[c4], "left atrial appendage")
  c9 <- str_detect(sentences[c4], "appendage")
  ret <- sum((sum(c5,c6,c7,c8,c9) > 0))
  
  car$left_atrial_turbulence[i] <- ret
}
rm(c1,c2,c3,c4,c5,c6,c7,c8,c9,d1,d2,ret,sentences)
rm(b0,b1,b2)

# find spontaneous echo contrast, only "spontaneous echo contrast" is sufficient
# never really abbreviated as "SEC" since that could mean "second" as well
b0 <- which(car$Report_Description %in% c(TEE_list,both_list))
b1 <- which(str_detect(docs.transf, "left atrium.{0,300}spontaneous\\s+echo\\s+contrast")) # find phrases of "spontaneous echo contrast" under left atrium section
b2 <- which(str_detect(docs.transf, "spontaneous\\s+echo\\s+contrast[[:alpha:][:space:],]*(laa|la appendage|left atr|left atrial appendage|appendage)")) # find phrases of "spontaneous echo contrast" under left atrium section
bb <- Reduce(union, list(b1,b2))
c1 <- which(str_detect(docs.transf, "left atrium.{0,300}(right atrium|left ventr|right ventr|aorta|conclusion).{0,300}spontaneous\\s+echo\\s+contrast")) # find phrases of "spontaneous echo contrast"
c2 <- which(str_detect(docs.transf, "left atrium.{0,300}spontaneous\\s+echo\\s+contrast.{0,300}(right atrium|left ventr|right ventr|aorta).{0,300}spontaneous\\s+echo\\s+contrast"))
c3 <- setdiff(c1,c2)
c4 <- which(str_detect(docs.transf, "left atrium.{0,300}(no\\b)[[:alpha:][:space:]]*spontaneous\\s+echo\\s+contrast.*(right atrium|left ventr|right ventr|aorta|conclusion)?")) # find phrases of "spontaneous echo contrast" under left atrium section
cc <- Reduce(union, list(c3,c4))
d1 <- setdiff(bb,cc)
dd <- intersect(b0,d1)

car$left_atrial_turbulence[dd] <- 1
rm(b0,b1,b2,bb,c1,c2,c3,c4,cc,d1,dd)

# sub-feature breakdown quanities
# c4 <- which(str_detect(docs.transf, "left atrium.{0,300}(no\\b)[[:alpha:][:space:]]*spontaneous\\s+echo\\s+contrast.*(right atrium|left ventr|right ventr|aorta|conclusion)")) # find phrases of "spontaneous echo contrast" under left atrium section
# d1 <- which(str_detect(sentences, "left atrium.{0,300}(no\\w*\\b).*smoke"))
# length(d1)
# left_atr_sec <- setdiff(b1,cc)
# length(left_atr_sec)
# length(intersect(left_atr_sec, delayed_laa_emptying))
# left_atr_no_sec <- c4
# length(left_atr_no_sec)
# length(delayed_laa_emptying)
# length(intersect(left_atr_no_sec, delayed_laa_emptying))
# length(union(left_atr_sec,delayed_laa_emptying))

####===== source_vs_feature$left_atrial_turbulence ===== EXTRA FEATURE ===== DELAYED EMPTYING VELOCITY
## delayed emptying velocity
feature <- "delayed_emptying_velocity"
car$delayed_emptying_velocity <- 0

ch <- "(([^[\\.]&&[:print:]]){0,300})" # all characters without period

# find "left atrial appendage"/"laa"/"la appendage" + "velocit*" in same sentence
c1 <- which(str_detect(tolower(docs.transf), "velocit")) # find "velocity"
d1 <- which(str_detect(tolower(docs.transf), "left\\s+atrial\\s+appendage"))
d2 <- which(str_detect(tolower(docs.transf), "laa"))
d3 <- which(str_detect(tolower(docs.transf), "la\\s+appendage"))
d4 <- which(str_detect(tolower(docs.transf), "appendage"))

c2 <- Reduce(union, list(d1,d2,d3,d4))
b1 <- intersect(c1,c2) # find common set

delayed_laa_emptying <- c()
for (k in b1) {
  ret <- 0
  # check if apply quantitative or qualitative rule
  # apply quantitative rule, if it fails, try qualitative rule
  # apply qualitative rule
  b2 <- str_extract(docs.transf[k], paste0(ch,"velocit",ch,"\\.")) # extract sentence containing velocity
  # print(b2)
  # apply quantitative rule - check if sentence after velocity ends in m/s or cm/s
  b3 <- str_extract(docs.transf[k], paste0(ch,"velocit",ch,"([\\d]{0,2}\\.[\\d]{0,2})?",ch,"(\\/s|per\\b)",ch,"\\.","([\\d]{0,2}\\.[\\d]{0,2})?",ch,"\\.")) # extract context around "velocit", 2 sentences before and 3 sentences after (3 after since number has decimal point within it)
  
  
  # if quantitative rule does not apply, check if sentence satisfies qualitative rules
  if (is.na(b3)) {
    pq <- str_detect(b2, "left atrial appendage") | str_detect(b2, "la appendage") | str_detect(b2, "laa") | str_detect(b2, "appendage")
    x0 <- str_detect(b2, "reduce") | str_detect(b2, "low")
    
    if (is.na(pq && x0)) { ret <- 0 
    } else if (pq && x0) { ret <- 1 #0.5
    } else { ret <- 0 }
    rm(pq,x0)
    next
  }
  
  # use for 2 sentences rule - check for "left atrial appendage" in the 2 sentences before the target sentence, target sentence has "velocit"
  b4 <- str_extract(docs.transf[k], paste0(ch,"\\.",ch,"\\.",ch,"velocit",ch,"([\\d]{0,2}\\.[\\d]{0,2})?",ch,"((\\/s)|per)",ch,"\\.","([\\d]{0,2}\\.[\\d]{0,2})?",ch,"\\.")) # extract context around "velocit", 1 sentence before and target sentence containing velocity
  if(is.na(b4)) {
    # if 2 sentences give na, check for "left atrial appendage" in 1 sentence before the target sentence, target sentence has "velocit"
    b4 <- str_extract(docs.transf[k], paste0(ch,"\\.",ch,"velocit",ch,"((\\/s)|per)",ch,"\\.")) # extract context around "velocit", 1 sentence before and target sentence containing velocity
  }
  
  # for 0.57 m/s in last sentence, replace 0. with 0,, to delimit sentences
  if (str_detect(b4,"\\d\\.") == T) {
    b4 <- str_replace(b4, "(?<=\\d)\\.",",,")
  }
  # extract last sentence
  t0 <- str_extract_all(b4, paste0(ch, paste0("\\.")))
  p0 <- unlist(lapply(t0, function(y) {str_detect(y, "left\\s+atrial\\s+appendage") | str_detect(y, "la\\s+appendage") | str_detect(y, "laa") | str_detect(y, "la\\s+appendage")}))
  p0 <- (sum(p0) > 0)
  # extract target sentence with velocity
  x <- str_extract(b4, paste0(ch,"velocit",ch,"(\\.)?",ch,"(\\/\\s*s|per)",ch,"\\."))
  # replace . back into ,,
  if (!is.na(str_detect(x,",,"))) {
    if (str_detect(x,",,")) x <- str_replace_all(x,"(?<=\\d),,",".")
  }
  if (is.na(x)) {
    print(k)
    print(x)
    print(b4)
  }
  # extract and fix unit
  # negate
  n1 <- str_detect(x, "aort") | # aortic valve, aorta, transaortic
    str_detect(x, "mitral valve") |
    str_detect(x, "regurgitant") |
    str_detect(x, "tricuspid") |
    str_detect(x, "pv")|str_detect(x, "pulmonary")|str_detect(x, "vein")|str_detect(x, "venous") |
    str_detect(x, "lvad") |
    str_detect(x, "lvot") |
    str_detect(x, "transgastric") |
    str_detect(x, "transvalvular") |
    str_detect(x, "normal")
  if(n1 == TRUE) { ret <- 0; next }
  p1 <- str_detect(x, "left\\s+atrial\\s+appendage") | str_detect(x, "la\\s+appendage") | str_detect(x, "laa") | str_detect(x, "appendage")
  # check for sentence structure (ex below)
  # Filling and emptying velocit*
  # OR
  # Inflow and outflow velocit*
  # OR
  # left atrial appendage velocity systolic filling velocity 0.5 m/s and diastolic emptying velocity 0.6 m/s.
  s1 <- str_detect(x, "filling.*and.*emptying.*velocit") | str_detect(x, "inflow.*and.*outflow.*velocit")
  if (s1 == TRUE) {
    x <- str_extract(x, "and.*")
    # if there are 2 numbers, pick the second number
    # ex: "left atrial appendage inflow and outflow velocities 0.3 and 0.2 m/s respectively."
    # ->  "and outflow velocities 0.3 and 0.2 m/s respectively."
    s3 <- str_count(x, "\\d+\\.?\\d+")
    s4 <- str_count(x, "and")
    if (s3 == 2 && s4 == 2) {
      x <- str_extract(x, " and.*")
    }
    rm(s1,s3,s4)
  }
  s2 <- str_detect(x, "emptying.*and.*filling.*velocit") | str_detect(x, "outflow.*and.*inflow.*velocit") # reverse order for completeness although this order doesn't happen ever
  if (s2 == TRUE) {
    if(str_detect(x, "emptying.*\\d+.?\\d*.*and")) {
      x <- str_extract(x, ".*and")
    } else if(str_detect(x, "emptying.*and.*\\d+.?\\d*")) {
      x <- str_extract(x, "and.*")
    } else if(str_detect(x, "outflow.*\\d+.?\\d*.*and")) {
      x <- str_extract(x, ".*and")
    } else if(str_detect(x, "outflow.*and.*\\d+.?\\d*")) {
      x <- str_extract(x, "and.*")
    }
    rm(s2)
  }
  
  # replace long form of unit with abbreviation
  x <- gsub("centimeters","cm",x) # detect plural first
  x <- gsub("centimeter","cm",x)
  x <- gsub("meters","m",x)
  x <- gsub("meter","m",x)
  
  # gets first number in a range if a range is present
  x1 <- str_extract(x, paste0("[<>]?\\d*\\.?\\d*?[-\\s]?\\d+\\.?\\d*",ch,"[\\s]?[cm]{1,2}[s]?[\\s\\.]{0,2}(\\/|per)[\\s]?s")) # equivalent of regexpr, regmatches together; first number optional, second number mandatory
  # detect less than / greater than (ex: less 0.4 m/s)
  orig_sign <- ""
  if(str_detect(x, "less\\s+(\\w+\\s+)?\\d+.?\\d*")) {
    orig_sign <- "<"
  } else if(str_detect(x, "greater\\s+(\\w+\\s+)?\\d+.?\\d*")) {
    orig_sign <- ">"
  } else if(str_detect(x, "exceeds\\s+\\d+.?\\d*")) {
    orig_sign <- ">"
  } else if(str_detect(x, "[<>]")){
    orig_sign <- str_extract(x, "[<>]")
  } else {
  }
  
  orig_num <- trimws(str_extract(x1, "\\d+\\.?\\d*"))
  # detect range - get first number
  if(grepl("[-//s]", orig_num)) {
    final_num <- str_extract(orig_num,"\\d+.?\\d*[-\\s]")
    final_num <- gsub("[-\\s]","",final_num)
    final_num <- as.numeric(final_num)
  }else{
    final_num <- as.numeric(orig_num)
  }
  
  orig_unit <- str_extract(x1, "[cm]{1,2}[s]?[\\s\\.]{0,2}(\\/|per\\b)(\\s)?s")
  x2 <- trimws(gsub("[\\s\\.]{0,2}(\\/|per\\b)(\\s)?s","",orig_unit)) # m or cm
  # fix units
  if (x2 == "cms") x2 <- "cm"
  if (x2 == "cm") final_num <- final_num / 100
  if (x2 == "m") final_num <- final_num # if metric is in "m", do nothing
  # check if scale is correct after units in m (ex: 25 m/s -> 0.25 m/s)
  
  # http://imaging.onlinejacc.org/content/jimg/7/12/1251.full.pdf
  # In normal subjects, without known
  # cardiac abnormalities, LAA contraction is biphasic
  # with velocities ranging from 50  6 cm/s to 83 +- 25 (-> max = 108, for safety, use 1.2 as upper bd before
  # dividing by 100)
  # cm/s with filling velocities ranging from 46 +- 12 cm/s
  # to 60 +- 19 cm/s
  # ex: There is a trace flow into the LAA by color Doppler with a high velocity (> 120 cm/s) -> 1.3 = upper bd
  if (final_num > 1.3) final_num <- final_num / 100
  
  # check if laa present && number < 0.40 m/s
  if (p1 == TRUE && orig_sign == ">" && final_num < 0.4) {
    ret <- 0
  } else if (p1 == TRUE && orig_sign == "" && final_num < 0.4) {
    ret <- 1
  } else if (p1 == TRUE && orig_sign == "<" && final_num == 0.4) {
    ret <- 1
  } else if (p0 == TRUE && orig_sign == ">" && final_num < 0.4) {
    ret <- 0
  } else if (p0 == TRUE && orig_sign == "" && final_num < 0.4) { # if previous sentence has "laa"
    ret <- 1
  } else if (p0 == TRUE && orig_sign == "<" && final_num == 0.4) { # if previous sentence has "laa"
    ret <- 1
  } else {
    ret <- 0
  }
  # print(paste0("ret=",ret))
  car$delayed_emptying_velocity[k] <- ret
  if (ret == 1) {
    delayed_laa_emptying <- c(delayed_laa_emptying, k)
  }
  rm(b2,b3,b4,t0,p0,n1,p1,c1,c2,d1,d2,d3,p0,p1,pq,orig_sign,orig_num,final_num,orig_unit,x0,x,x1,x2)
}

# if delayed_emptying_velocity is a part of the left atrial turbulence algorithm,
# then you capture smoke, SEC, then delayed emptying velocity, cancel out SEC with "no SEC"
c4 <- which(str_detect(docs.transf, "left atrium.{0,300}(no\\b)[[:alpha:][:space:]]*spontaneous\\s+echo\\s+contrast.*(right atrium|left ventr|right ventr|aorta|conclusion)?")) # find phrases of "spontaneous echo contrast" under left atrium section
car$left_atrial_turbulence[c4] <- 0 # doesn't affect the set (which(car$left_atrial_turbulence == 1))
rm(b1,b2,b3,b4,bb,t0,p0,n1,p1,s1,s2,s3,s4,c1,c2,c4,cc,d1,d2,d3,d4,dd,p0,p1,pq,orig_sign,orig_num,final_num,orig_unit,x0,x,x1,x2,ret)
sum(car$left_atrial_turbulence)
sum(car$delayed_emptying_velocity)

####===== source_vs_feature$atrial_septal_aneurysm #7=====
# atrium septum aneurysm, interatrial septum aneurysm, aneurysm atrium septum
feature <- "atrial_septal_aneurysm"
car$atrial_septal_aneurysm <- 0

ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],]){0,100})" # all characters without period
ch_sm <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],]){0,60})" # all characters without period

## regex 1 (atr.*sept.*aneurysm) (atrial sept < aneurysm)
b1 <- which(str_detect(docs.transf, paste0("atr",ch_sm,"sept",ch_sm,"aneurysm"))) # find phrases of "atrial septal/septum aneurysm"
c1 <- which(str_detect(docs.transf, paste0("(without\\b)",ch,"atr",ch_sm,"sept",ch_sm,"aneurysm"))) # without
c11 <- which(str_detect(docs.transf, paste0("atr",ch_sm,"sept",ch_sm,"(without\\b)",ch,"aneurysm"))) # without
c2 <- which(str_detect(docs.transf, paste0("(no\\w*\\b)",ch,"atr",ch_sm,"sept",ch_sm,"aneurysm"))) # no
c22 <- which(str_detect(docs.transf, paste0("atr",ch_sm,"sept",ch_sm,"(no\\w*\\b)",ch,"aneurysm"))) # no

c3 <- which(str_detect(docs.transf, paste0("(reason\\b)",ch,"atr",ch_sm,"sept",ch_sm,"aneurysm"))) # exclude "reason atrial septal aneurysm"

b2 <- Reduce(union, list(c1,c11,c2,c22,c3))
b3 <- intersect(b1,b2) # find common set
b4 <- setdiff(b1, b3) # find subset of phrases with negation (we couldn't find with b1)

## regex 2 (aneurysm.*atr.*sept.*) (aneurysm < atrial septum)
d1 <- which(str_detect(docs.transf, paste0("aneurysm",ch_sm,"atr",ch_sm,"sept",ch_sm))) # find phrases of "aneurysm of atrial septum"

e1 <- which(str_detect(docs.transf, paste0("(without\\b)",ch,"aneurysm",ch_sm,"atr",ch_sm,"sept"))) # without
e2 <- which(str_detect(docs.transf, paste0("(no\\w*\\b)",ch,"aneurysm",ch_sm,"atr",ch_sm,"sept"))) # no

e3 <- which(str_detect(docs.transf, paste0("(reason\\b)",ch,"aneurysm",ch_sm,"atr",ch_sm,"sept"))) # exclude "reason aneurysm of atrial septum" - not really present

d2 <- Reduce(union, list(e1,e2,e3))
d3 <- intersect(d1,d2) # find common set
d4 <- setdiff(d1, d3) # find subset of phrases with negation (we couldn't find with d1)

## regex 3 (interatr.*sept.*aneurysm) (interatrial sept < aneurysm)
f1 <- which(str_detect(docs.transf, paste0("interatr",ch_sm,"sept",ch_sm,"aneurysm"))) # find phrases of "aneurysm of interatrial septum"

g1 <- which(str_detect(docs.transf, paste0("(without\\b)","interatr",ch_sm,"sept",ch,"aneurysm"))) # without -- not really a problem
g11 <- which(str_detect(docs.transf, paste0("interatr",ch_sm,"sept",ch_sm,"(without\\b)",ch,"aneurysm"))) # A mobile interatrial septum is present without atrial septal aneurysm formation
g2 <- which(str_detect(docs.transf, paste0("(no\\w*\\b)",ch,"interatr",ch_sm,"sept",ch_sm,"aneurysm"))) # no
g22 <- which(str_detect(docs.transf, paste0("interatr",ch_sm,"sept",ch_sm,"(no\\w*\\b)",ch,"aneurysm"))) # no

g3 <- which(str_detect(docs.transf, paste0("(reason\\b)",ch,"interatr",ch_sm,"sept",ch_sm,"aneurysm"))) # exclude "reason aneurysm of interatrial septum" - not really present
f2 <- Reduce(union, list(g1,g11,g2,g22,g3))
f3 <- intersect(f1,f2) # find common set
f4 <- setdiff(f1, f3) # find subset of phrases with negation (we couldn't find with f1)

## regex 4 (aneurysm.*interatr.*sept.*) (aneurysm < interatrial septum)
h1 <- which(str_detect(docs.transf, paste0("aneurysm",ch_sm,"interatr",ch_sm,"sept",ch_sm))) # find phrases of "aneurysm of atrial septum"

i1 <- which(str_detect(docs.transf, paste0("(without\\b)",ch,"aneurysm",ch_sm,"interatr",ch_sm,"sept"))) # without -- not really a problem
i2 <- which(str_detect(docs.transf, paste0("(no\\w*\\b)",ch,"aneurysm",ch_sm,"interatr",ch_sm,"sept"))) # no

i3 <- which(str_detect(docs.transf, paste0("(reason\\b)",ch,"aneurysm",ch_sm,"interatr",ch_sm,"sept"))) # exclude "reason aneurysm of atrial septum" - not really present

h2 <- Reduce(union, list(i1,i2,i3))
h3 <- intersect(h1,h2) # find common set
h4 <- setdiff(h1, h3) # find subset of phrases with negation (we couldn't find with d1)

ret <- Reduce(union, list(b4,d4,f4, h4))

car$atrial_septal_aneurysm[ret] <- 1
sum(car$atrial_septal_aneurysm)
rm(b1,b2,b3,b4, c1,c11,c2,c22,c3, d1,d2,d3,d4, e1,e2,e3, f1,f2,f3,f4, g1,g11,g2,g22,g3, h1,h2,h3,h4, i1,i2,i3, ret)

####===== source_vs_feature$patent_foramen_ovale #8=====
feature <- "patent_foramen_ovale"
car$patent_foramen_ovale <- 0
# str_detect(":","\\p{Terminal_Punctuation}")
ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,50})" # all characters without period
ch_sm <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,10})" # all characters without period
dummy_data <- str_remove_all(docs.transf, "(reason[:space:]*)([:print:]){0,100}(pfo|patent forament ovale)") # ignore "Reason CHF;Mitral Stenosis;"
dummy_data <- str_remove_all(dummy_data, "(patent forament ovale)([:print:]){0,100}icd-") # ignore "Diagnosis: patent forament ovale [I05.0 (ICD-10-CM)]"
dummy_data <- str_remove_all(dummy_data, "diagnosis[:print:]{0,100}(pfo|patent forament ovale)")
dummy_data <- str_remove_all(dummy_data, "(atrial septal defect)([:print:]){0,100}icd-") # ignore "Diagnosis: patent forament ovale [I05.0 (ICD-10-CM)]"
dummy_data <- str_remove_all(dummy_data, "diagnosis[:print:]{0,100}(atrial septal defect)")
dummy_data <- str_remove_all(dummy_data, paste0("reason",ch,"(pfo|patent foramen ovale)")) # reason
dummy_data <- str_remove_all(dummy_data, paste0("eval\\w*",ch,"(pfo|patent foramen ovale)")) # evaluate, eval
dummy_data <- str_remove_all(dummy_data, paste0("exclude",ch,"(pfo|patent foramen ovale)")) # exclude
dummy_data <- str_remove_all(dummy_data, paste0("assess",ch,"(pfo|patent foramen ovale)")) # assess
dummy_data <- str_remove_all(dummy_data, paste0("rule out",ch,"(pfo|patent foramen ovale)")) # rule out

b1 <- which(str_detect(dummy_data, "patent\\s+foramen\\s+ovale")) # find phrases of "patent forament ovale"
c1 <- which(str_detect(dummy_data, paste0("(no\\w*\\b)",ch,"patent foramen ovale")))
c2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"patent foramen ovale")))
c3 <- which(str_detect(dummy_data, paste0("patent foramen ovale",ch,"(no\\w*\\b)")))
c4 <- which(str_detect(dummy_data, paste0("patent foramen ovale",ch,"clos\\w*")))
c5 <- which(str_detect(dummy_data, paste0("patent foramen ovale",ch,"repair")))
cc <- Reduce(union, list(c1,c2,c3,c4,c5))
b2 <- intersect(b1,cc) # find common set
b3 <- setdiff(b1,b2) # find subset of phrases with no (we couldn't find with b1)

# atrial septal defect (housed under patent foramen ovale)
d1 <- which(str_detect(dummy_data, "atrial\\s+septal\\s+defect")) # find phrases of "left atrial appendage thrombus"
e1 <- which(str_detect(dummy_data, paste0("(no\\w*\\b)",ch,"atrial\\s+septal\\s+defect")))
e2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"atrial\\s+septal\\s+defect")))
e3 <- which(str_detect(dummy_data, paste0("(clos\\w*)",ch,"atrial\\s+septal\\s+defect")))
e4 <- which(str_detect(dummy_data, paste0("post\\s+atrial\\s+septal\\s+defect",ch,"repair")))
e5 <- which(str_detect(dummy_data, paste0("post\\s+atrial\\s+septal\\s+defect",ch,"clos\\w*")))
ee <- Reduce(union, list(e1,e2,e3,e4,e5))
d2 <- intersect(d1,ee) # find common set
d3 <- setdiff(d1,d2) # find subset of phrases with no (we couldn't find with d1)

# PFO
f1 <- which(str_detect(dummy_data, "pfo")) # find phrases of "PFO"
g1 <- which(str_detect(dummy_data, paste0("(no\\w*\\b)",ch,"pfo")))
g2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"pfo")))
g3 <- which(str_detect(dummy_data, paste0("pfo",ch,"(no\\w*\\b)")))
g4 <- which(str_detect(dummy_data, paste0("pfo",ch,"clos\\w*")))
g5 <- which(str_detect(dummy_data, paste0("clos\\w*",ch,"pfo")))
g6 <- which(str_detect(dummy_data, paste0("pfo",ch,"repair")))
gg <- Reduce(union, list(g1,g2,g3,g4,g5,g6))
f2 <- intersect(f1,gg) # find common set
f3 <- setdiff(f1,f2) # find subset of phrases with no (we couldn't find with d1)

# right-to-left interatrial shunting
# no evidence of an interatrial shunt, # no evidence of interatrial communication
h1 <- which(str_detect(dummy_data, "interarterial (shunt|communication)")) # find phrases of "interatrial shunt"
k1 <- which(str_detect(dummy_data, paste0("(no\\w*\\b)",ch,"interatrial (shunt|communication)")))
k2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"interatrial (shunt|communication)")))
kk <- Reduce(union, list(k1,k2))
h2 <- intersect(h1,kk) # find common set
h3 <- setdiff(h1,h2) # find subset of phrases with no (we couldn't find with d1)

# intracardiac shunting
m1 <- which(str_detect(dummy_data, "intracard\\w* shunt")) # find phrases of "PFO"
n1 <- which(str_detect(dummy_data, paste0("(no\\w*\\b)",ch,"intracard\\w* shunt")))
n2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"intracard\\w* shunt")))
nn <- Reduce(union, list(n1,n2))
m2 <- intersect(m1,nn) # find common set
m3 <- setdiff(m1,m2) # find subset of phrases with no (we couldn't find with d1)

# residual shunting
p1 <- which(str_detect(dummy_data, "residual shunt")) # find phrases of "PFO"
q1 <- which(str_detect(dummy_data, paste0("(no\\w*\\b)",ch,"residual shunt")))
q2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"residual shunt")))
qq <- Reduce(union, list(q1,q2))
p2 <- intersect(p1,qq) # find common set
p3 <- setdiff(p1,p2) # find subset of phrases with no (we couldn't find with d1)

ret <- Reduce(union, list(b3,d3,f3,h3,m3,p3))
car$patent_foramen_ovale[ret] <- 1
sum(car$patent_foramen_ovale)
rm(b1,b2,b3, c1,c2,c3,cc, d1,d2,d3, e1,e2,e3,e4,e5,ee, f1,f2,f3, g1,g2,g3,g4,g5,g6,gg, 
   h1,h2,h3,k1,k2,kk, m1,m2,m3,n1,n2,nn, p1,p2,p3,q1,q2,qq, ret)

# left to right shunting, bidirectional shunting
# right to left shunting



####===== source_vs_feature$hypokinetic_left_ventricular_segment #9=====
feature <- "hypokinetic_left_ventricular_segment"
car$hypokinetic_left_ventricular_segment <- 0

pat <- "LV\\s?(\\w+\\s){0,4}EF\\s(\\w+\\s){0,4}\\d+|LV\\s?EF (\\w+\\s){0,4}is (\\w+\\s){0,4}\\d+\\s?(-|to)\\s?\\d+|LV\\s?EF at \\d+|((L?V?EF:*|Ejection\\s+fraction\\s+is|ejection\\s+fraction\\s+of|ejection\\s+fraction\\s+is|ejection\\s+fraction\\s+is \\D* at|The calculated LVEF \\D*is\\D*|LVEF is|LVEF estimated at|left\\s+ventricular function is|ejection\\s+fraction\\s+is \\D* of)\\s+(\\S+)\\s*\\%)"
b <- str_extract(corpus.txt, pat) # find phrases of "LVEF"
p <- gsub("'", "", b, fixed=F)
d <- gsub(":", "", p, fixed=F)
e <- gsub(",", "", d, fixed=F)
f <- gsub("[a-z]+", "", e, fixed=F, ignore.case=T)	
g <- gsub("%", "", f, fixed=F)
n = 5
h = substr(g,(nchar(g)+1)-n,nchar(g))
h <- trimws(g, "both")
i <- strsplit(h, "-")
j <- lapply(i, as.numeric)
k <- lapply(j, mean, na.rm=T) #take mean when \\1 group measurement or range (i.e., "45-50%")
z <- as.numeric(unlist(k))
car$LVEF <- z
car$LVEF[which(car$LVEF == 0)] <- NA
sum(is.na(car$LVEF))

# if LVEF <= 40%, then hypokinetic_left_ventricular_segment present
ret <- as.numeric(z <= 40)
sum(car$Report_Description=='CARDIAC ULTRASOUND' & car$hypokinetic_left_ventricular_segment==1, na.rm=T)
car$hypokinetic_left_ventricular_segment <- ret
rm(pat,b,p,d,e,f,g,n,h,i,j,k,z,ret)

####===== source_vs_feature$left_ventricular_thrombus #10=====
feature <- "left_ventricular_thrombus"
car$left_ventricular_thrombus <- 0
ch <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,80})" # all characters without period
ch_sm <- "(([[^\\p{Terminal_Punctuation}&&[:print:]],:]){0,30})" # all characters without period
term <- "(lv\\s+\\w{0,15}\\s*(thromb|clot)|left\\s+ventr\\w+[[:alpha:]\\s,]*(thromb|clot)|(thromb|clot)[[:alpha:]\\s]*left\\s+ventr|(thromb|clot)\\w{0,40}lv)"
dummy_data <- str_remove_all(docs.transf, paste0("(reason[:space:]*)([[:print:]\\/]){0,100}",term)) # ignore "reason chf; sle cardiomyopathy; ? lv thrombus;"
dummy_data <- str_remove_all(dummy_data, paste0("eval",ch,term)) # evaluate, eval
dummy_data <- str_remove_all(dummy_data, paste0("exclu(de|sion)\\b",ch,term)) # exclude
dummy_data <- str_remove_all(dummy_data, paste0(term,ch,"excluded\\b")) # excluded, "left ventricular thrombus can not be excluded"
dummy_data <- str_remove_all(dummy_data, paste0("assess",ch,term)) # assess
dummy_data <- str_remove_all(dummy_data, paste0("rule\\s+out",ch,term)) # rule out
dummy_data <- str_remove_all(dummy_data, paste0("r\\/o",ch,term)) # rule out
dummy_data <- str_remove_all(dummy_data, paste0(term,"([:print:]){0,70}icd-")) # ignore "diagnosis: left ventricular thrombosis without mi [i51.3 (icd-10-cm)]"
dummy_data <- str_remove_all(dummy_data, paste0("diagnosis[:print:]{0,70}",term))
dummy_data <- str_remove_all(dummy_data, paste0("indications[:print:]{0,70}",term)) # Indications:   LV thrombus

# search for lv thrombus
term <- "(\\blv\\s+\\w{0,15}\\s*(thromb|clot))"
b1 <- which(str_detect(dummy_data, paste0(term))) # find phrases of "lv thrombus"
c1 <- which(str_detect(dummy_data, paste0("(\\bno\\w{0,10}\\b|negative)",ch,term))) # no obvious lv thrombus
c2 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,term)))
c3 <- which(str_detect(dummy_data, paste0(term,ch,"(\\bno\\w{0,10}\\b|negative)")))
c4 <- which(str_detect(dummy_data, paste0(term,ch,"(unlikely|not\\s+likely)"))) # lv thrombus is unlikely
cc <- Reduce(union, list(c1,c2,c3,c4))
b2 <- intersect(b1,cc) # find common set
b3 <- setdiff(b1,b2) # find subset of phrases with no (we couldn't find with b1)

# left ventricular thrombus (left ventr < thrombus)
# left ventricular apex consistent with thrombus
d1 <- which(str_detect(dummy_data, paste0("(left\\s+ventr)",ch,"((thromb|clot))"))) # find phrases of "left ventricular thrombus"
e1 <- which(str_detect(dummy_data, paste0("(\\bno(t)?\\w{0,5}\\b|negative)",ch_sm,"(left\\s+ventr)",ch,"((thromb|clot))"))) # no obvious evidence of left ventricular thrombus
e2 <- which(str_detect(dummy_data, paste0("(left\\s+ventr)",ch_sm,"(no(t)?\\w{0,5}\\b|negative)",ch_sm,"((thromb|clot))")))
e3 <- which(str_detect(dummy_data, paste0("(left\\s+ventr)",ch,"((thromb|clot))",ch,"(\\bno(t)?\\w{0,10}\\b)")))
e4 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"(left\\s+ventr)",ch,"((thromb|clot))")))
e5 <- which(str_detect(dummy_data, paste0("(left\\s+ventr)",ch,"(without\\b)",ch,"((thromb|clot))")))
ee <- Reduce(union, list(e1,e2,e3,e4,e5))
d2 <- intersect(d1,ee) # find common set
d3 <- setdiff(d1,d2) # find subset of phrases with no (we couldn't find with b1)

# thrombus in the left ventricle (thrombus < left ventr)
# thrombus is present within the left ventricular apex
f1 <- which(str_detect(dummy_data, paste0("((thromb|clot))",ch,"(left\\s+ventr)"))) # find phrases of "left ventricular thrombus"
g1 <- which(str_detect(dummy_data, paste0("(no(t)?\\b|negative)",ch,"((thromb|clot))",ch,"(left\\s+ventr)")))
g2 <- which(str_detect(dummy_data, paste0("((thromb|clot))",ch,"(no(t)?|negative)",ch,"(left\\s+ventr)")))
g3 <- which(str_detect(dummy_data, paste0("(without\\b)",ch,"((thromb|clot))",ch,"(left\\s+ventr)")))
g4 <- which(str_detect(dummy_data, paste0("(left\\s+ventr)",ch,"(without\\b)",ch,"((thromb|clot))")))
g5 <- which(str_detect(dummy_data, paste0("((thromb|clot))",ch,"(left\\s+ventr)",ch,"(\\bno(t|rmal)?\\b)")))
gg <- Reduce(union, list(g1,g2,g3,g4,g5))
f2 <- intersect(f1,gg) # find common set
f3 <- setdiff(f1,f2) # find subset of phrases with no (we couldn't find with b1)

# left ventricular apex c / w thrombus
h1 <- which(str_detect(dummy_data, paste0("(left)",ch,"(ventr)",ch,"(apex|apical)",ch,"((thromb|clot))")))
i1 <- which(str_detect(dummy_data, paste0("(left)",ch,"(ventr)",ch,"(apex|apical)",ch,"(\\bno(t)?\\b|negative)",ch,"((thromb|clot))")))
i2 <- which(str_detect(dummy_data, paste0("(\\bno(t)?\\b|negative)",ch,"(left)",ch,"(ventr)",ch,"(apex|apical)",ch,"((thromb|clot))")))
i3 <- which(str_detect(dummy_data, paste0("(left)",ch,"(ventr)",ch,"(apex|apical)",ch,"((thromb|clot))",ch,"(\\bno(t)?\\b|negative)")))
i4 <- which(str_detect(dummy_data, paste0("(left)",ch,"(ventr)",ch,"(apex|apical)",ch,"without\\b",ch,"((thromb|clot))")))
i5 <- which(str_detect(dummy_data, paste0("without\\b",ch,"(left)",ch,"(ventr)",ch,"(apex|apical)",ch,"((thromb|clot))")))
ii <- Reduce(union, list(i1,i2,i3,i4,i5))
h2 <- intersect(h1,ii)
h3 <- setdiff(h1,h2)

jj <- Reduce(union, list(b1,d1,f1,h1))
kk <- Reduce(union, list(cc,ee,gg,ii)) # find common set
ret <- setdiff(jj,kk)
car$left_ventricular_thrombus[ret] <- 1

# left ventricular thrombus -> negation, neutralization -> verify regular expression logic manually
# see that thrombus is synonomous with clot -> find left ventricular apex -> apical
# -> additional negation, neutralization

features_list <- c('mitral_stenosis',
                   'left_atrial_appendage_thrombus',
                   'akinetic_left_ventricular_segment',
                   'mitral_valve_prolapse',
                   'mitral_annulus_calcification',
                   'left_atrial_turbulence',
                   'delayed_emptying_velocity',
                   'atrial_septal_aneurysm',
                   'patent_foramen_ovale',
                   'hypokinetic_left_ventricular_segment')

for (i in (1:length(features_list))) {
  print(features_list[i])
  print(table(car[,c(features_list[i])], useNA = "always"))
}

colSums(car[,features_list], na.rm = T)

car_CE <- car
save(car_CE, file = "car_CE.RData")

# this code for Cardioembolic stroke algorithm paper

# the following builds a long data file containing all of the instances of the CE NLP features
car_NLP <- mrn %>% dplyr::select(IncomingId, EMPI) %>% left_join(car_CE, by=c("EMPI"))

car1 <- car_NLP %>% filter(mitral_stenosis == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "ms")
car2 <- car_NLP %>% filter(left_atrial_appendage_thrombus == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "laa")
car3 <- car_NLP %>% filter(akinetic_left_ventricular_segment == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "akine_lvs")
car4 <- car_NLP %>% filter(mitral_valve_prolapse == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "mvp")
car5 <- car_NLP %>% filter(mitral_annulus_calcification == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "mac")
car6 <- car_NLP %>% filter(left_atrial_turbulence == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "lat")
car7 <- car_NLP %>% filter(atrial_septal_aneurysm == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "asa")
car8 <- car_NLP %>% filter(patent_foramen_ovale == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "pfo")
car9 <- car_NLP %>% filter(hypokinetic_left_ventricular_segment == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "hypo_lvs")
car10 <- car_NLP %>% filter(left_ventricular_thrombus == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "lvt")
car11 <- car_NLP %>% filter(delayed_emptying_velocity == 1) %>% dplyr::select(EMPI, car_d, primary_key) %>% mutate(feature = "dev")

car_NLP1 <- c()
for(i in 1:11) {
  if (exists(paste0("car",i)) == T) {
    car_NLP1 <- rbind(car_NLP1, get(paste0("car",i)))
  }
}

save(car_NLP1, file = "car_NLP1.RData")

# sink(file="out_docs.transf_1.txt",split=F,append=F)
# print(docs.transf[1])
# sink()

