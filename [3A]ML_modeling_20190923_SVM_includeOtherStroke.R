## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# myvars <- names(feature_table_wide4) %in% c("EMPI", "stroke_date_index", "Toast", "CCS", "Toast_new", "Toast_final", "age", "Gender", "gender") #remove missing cardiac sources, response variables (TOAST, CCS)
# features_only <- feature_table_wide4[!myvars]

## ----SVM linear----------------------------------------------------------
# SVM
# 2) Using machine learning package "caret" ====
library(caret)
library(e1071)
library(dplyr)
library(pROC)

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")

Accuracy.SVM <- c()
Sensitivity.SVM <- c()
Specificity.SVM <- c()
PPV.SVM <- c()
NPV.SVM <- c()
F1.SVM <- c()
AUC.SVM <- c()
# C.SVM <- c()

for (i in 1:50) {
set.seed(i)

df0 = feature_table_wide4

df2 = df0[sample(1:nrow(df0), replace=T),]
  # split dataset 70-30
  DS.Size <- dim(df2)[1]
  Test.Train.Percent <- 0.7 # Split Data into 70% for Training and 30% for Testing
  ix.Range <- round(DS.Size*Test.Train.Percent)
  train.Range <- seq(from = 1, to = ix.Range, by = 1)
  test.Range <- seq(from = (ix.Range+1), to = DS.Size, by = 1)
  
  # exclude variables
  myvars <- names(df2) %in% c("Toast","CCS","EMPI","stroke_date_index","Gender",
                                           "Toast_new","intracard_thromb") #remove missing cardiac sources, response variables (TOAST, CCS)
  train.doc <- df2[train.Range,!myvars]
  test.doc  <- df2[test.Range,!myvars]
  
  Tags.Train <- c(df2$Toast_final[train.Range])
  Tags.Test <- c(df2$Toast_final[test.Range])

  table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 5 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
SVM.Train <- svm(formula = as.factor(Toast_final) ~ . -doc.class,
                  data = train.doc1,
                  type = "C-classification",
                  kernel = "linear",
                  tunecontrol = tune.control(cross=5))
summary(SVM.Train)

SVM.Predict <- predict(SVM.Train, newdata = test.doc1, tunecontrol = tune.control(cross=5)) # Give prediction on the test data

CM.SVM <- confusionMatrix(SVM.Predict, as.factor(test.doc1$doc.class));
Accuracy.SVM <- c(Accuracy.SVM, CM.SVM$overall[c('Accuracy')])
Sensitivity.SVM <- c(Sensitivity.SVM,CM.SVM$byClass['Sensitivity'])
Specificity.SVM <- c(Specificity.SVM,CM.SVM$byClass['Specificity'])
PPV.SVM <- c(PPV.SVM,CM.SVM$byClass['Pos Pred Value'])
NPV.SVM <- c(NPV.SVM,CM.SVM$byClass['Neg Pred Value'])
F1.SVM <- c(F1.SVM,CM.SVM$byClass['F1'])
# C.SVM <- c(C.SVM, SVM.Train$bestTune$C) # Best C parameter Chosen

# pred <- prediction(as.numeric(SVM.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)
# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

# Syntax (response, predictor):
AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(SVM.Predict))
AUC.SVM <- c(AUC.SVM, AUC.perf)

}

Accuracy.SVM_boot <- mean(Accuracy.SVM)
PPV.SVM_boot <- mean(PPV.SVM)
NPV.SVM_boot <- mean(NPV.SVM)
Sensitivity.SVM_boot <- mean(Sensitivity.SVM)
Specificity.SVM_boot <- mean(Specificity.SVM)
F1.SVM_boot <- mean(F1.SVM)
AUC.SVM_boot <- mean(AUC.SVM)
# C.SVM_boot <- mean(C.SVM)

SVM.statistics <- list(Accuracy.SVM=Accuracy.SVM,
                       PPV.SVM=PPV.SVM,
                       NPV.SVM=NPV.SVM,
                       Sensitivity.SVM=Sensitivity.SVM,
                       Specificity.SVM=Specificity.SVM,
                       F1.SVM=F1.SVM,
                       AUC.SVM=AUC.SVM)
save(SVM.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/SVM.statistics.RData")

