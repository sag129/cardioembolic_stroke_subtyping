## ----setup, include=FALSE------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE)
options(error = expression(NULL))

.libPaths( c( .libPaths(), "/PHShome/wg985/R/x86_64-pc-linux-gnu-library/3.4", "/opt/rstudio-r/R-3.4.0/lib/R/library"))

# myvars <- names(feature_table_wide4) %in% c("EMPI", "stroke_date_index", "Toast", "CCS", "Toast_new", "Toast_final", "age", "Gender", "gender") #remove missing cardiac sources, response variables (TOAST, CCS)
# features_only <- feature_table_wide4[!myvars]

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")

## ----RF------------------------------------------------------------------
# RF
# 2) Using machine learning package "caret" ====
library(caret)
library(dplyr)
library(pROC)
library(randomForest)

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide4.RData")

Accuracy.RF <- c()
Sensitivity.RF <- c()
Specificity.RF <- c()
PPV.RF <- c()
NPV.RF <- c()
F1.RF <- c()
AUC.RF <- c()

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
                                           "Toast_new") #remove missing cardiac sources, response variables (TOAST, CCS)
  train.doc <- df2[train.Range,!myvars]
  test.doc  <- df2[test.Range,!myvars]
  
  Tags.Train <- c(df2$Toast_final[train.Range])
  Tags.Test <- c(df2$Toast_final[test.Range])

  # table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  # table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 5)

# train
RF.Train <- train(as.factor(Toast_final) ~. -doc.class, data = train.doc1, method = "rf",
 trControl=ctrl,
 scale=F)

RF.Predict <- predict(RF.Train, newdata = test.doc1) # Give prediction on the test data

CM.RF <- confusionMatrix(RF.Predict, as.factor(test.doc1$doc.class));
Accuracy.RF <- c(Accuracy.RF, CM.RF$overall[c('Accuracy')])
Sensitivity.RF <- c(Sensitivity.RF,CM.RF$byClass['Sensitivity'])
Specificity.RF <- c(Specificity.RF,CM.RF$byClass['Specificity'])
PPV.RF <- c(PPV.RF,CM.RF$byClass['Pos Pred Value'])
NPV.RF <- c(NPV.RF,CM.RF$byClass['Neg Pred Value'])
F1.RF <- c(F1.RF,CM.RF$byClass['F1'])

# pred <- prediction(as.numeric(RF.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)

# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(RF.Predict))

AUC.RF <- c(AUC.RF, AUC.perf)

}

Accuracy.RF_boot <- mean(Accuracy.RF)
PPV.RF_boot <- mean(PPV.RF)
NPV.RF_boot <- mean(NPV.RF)
Sensitivity.RF_boot <- mean(Sensitivity.RF)
Specificity.RF_boot <- mean(Specificity.RF)
F1.RF_boot <- mean(F1.RF)
AUC.RF_boot <- mean(AUC.RF)

RF.statistics <- list(Accuracy.RF=Accuracy.RF,
                      PPV.RF=PPV.RF,
                      NPV.RF=NPV.RF,
                      Sensitivity.RF=Sensitivity.RF,
                      Specificity.RF=Specificity.RF,
                      F1.RF=F1.RF,
                      AUC.RF=AUC.RF)
save(RF.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/RF.statistics.RData")
