## ----setup, include=FALSE------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE)
options(error = expression(NULL))

.libPaths( c( .libPaths(), "/PHShome/wg985/R/x86_64-pc-linux-gnu-library/3.4", "/opt/rstudio-r/R-3.4.0/lib/R/library"))

## ----KNN-----------------------------------------------------------------
# KNN
# 2) Using machine learning package "caret" ====

library(caret)
library(dplyr)
library(pROC)

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")

Accuracy.KNN <- c()
Sensitivity.KNN <- c()
Specificity.KNN <- c()
PPV.KNN <- c()
NPV.KNN <- c()
F1.KNN <- c()
AUC.KNN <- c()
K.KNN <- c()

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
  myvars <- names(df2) %in% c("EMPI","stroke_date_index","Toast","CCS",
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
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
KNN.Train <- train(as.factor(Toast_final) ~. -doc.class, data = train.doc1, method = "knn",
 trControl=ctrl,
 tuneLength = 10)

KNN.Predict <- predict(KNN.Train, newdata = test.doc1, decision.value=T) # Give prediction on the test data
CM.KNN <- confusionMatrix(KNN.Predict, as.factor(test.doc1$doc.class));

Accuracy.KNN <- c(Accuracy.KNN, CM.KNN$overall[c('Accuracy')])
Sensitivity.KNN <- c(Sensitivity.KNN,CM.KNN$byClass['Sensitivity'])
Specificity.KNN <- c(Specificity.KNN,CM.KNN$byClass['Specificity'])
PPV.KNN <- c(PPV.KNN,CM.KNN$byClass['Pos Pred Value'])
NPV.KNN <- c(NPV.KNN,CM.KNN$byClass['Neg Pred Value'])
F1.KNN <- c(F1.KNN,CM.KNN$byClass['F1'])
K.KNN <- c(K.KNN, KNN.Train$finalModel$k)
# print(Accuracy.KNN)
# Fscore.KNN <- CM.KNN$byClass[7]
# print(Fscore.KNN) # 0.7811159 

# pred <- prediction(as.numeric(KNN.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)
# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(KNN.Predict))

AUC.KNN <- c(AUC.KNN, AUC.perf)

}

Accuracy.KNN_boot <- mean(Accuracy.KNN)
PPV.KNN_boot <- mean(PPV.KNN)
NPV.KNN_boot <- mean(NPV.KNN)
Sensitivity.KNN_boot <- mean(Sensitivity.KNN)
Specificity.KNN_boot <- mean(Specificity.KNN)
F1.KNN_boot <- mean(F1.KNN)
AUC.KNN_boot <- mean(AUC.KNN)
K.KNN_boot <- mean(K.KNN)

KNN.statistics <- list(Accuracy.KNN=Accuracy.KNN,
                       PPV.KNN=PPV.KNN,
                       NPV.KNN=NPV.KNN,
                       Sensitivity.KNN=Sensitivity.KNN,
                       Specificity.KNN=Specificity.KNN,
                       F1.KNN=F1.KNN,
                       AUC.KNN=AUC.KNN,
                       K.KNN=K.KNN)
save(KNN.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/KNN.statistics.RData")
