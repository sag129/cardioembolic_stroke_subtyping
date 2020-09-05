## ----setup, include=FALSE------------------------------------------------
# knitr::opts_chunk$set(echo = TRUE)
options(error = expression(NULL))

.libPaths( c( .libPaths(), "/PHShome/wg985/R/x86_64-pc-linux-gnu-library/3.4", "/opt/rstudio-r/R-3.4.0/lib/R/library"))

# myvars <- names(feature_table_wide4) %in% c("EMPI", "stroke_date_index", "Toast", "CCS", "Toast_new", "Toast_final", "age", "Gender", "gender") #remove missing cardiac sources, response variables (TOAST, CCS)
# features_only <- feature_table_wide4[!myvars]

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")

## ----CART----------------------------------------------------------------
# CART
# 2) Using machine learning package "caret" ====
library(caret)
library(dplyr)
library(pROC)
library(rpart)

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/feature_table_wide4.RData")

Accuracy.CART <- c()
Sensitivity.CART <- c()
Specificity.CART <- c()
PPV.CART <- c()
NPV.CART <- c()
F1.CART <- c()
AUC.CART <- c()

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

  # table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  # table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)
# grid <- expand.grid(cp = seq(0, 0.05, 0.005))

# train
CART.Train <- train(as.factor(Toast_final) ~. -doc.class, data = train.doc1, method = "rpart",
 trControl=ctrl,
 metric = 'Accuracy',
 tuneLength = 10)

CART.Predict <- predict(CART.Train, newdata = test.doc1) # Give prediction on the test data

CM.CART <- confusionMatrix(CART.Predict, as.factor(test.doc1$doc.class));
Accuracy.CART <- c(Accuracy.CART, CM.CART$overall[c('Accuracy')])
Sensitivity.CART <- c(Sensitivity.CART,CM.CART$byClass['Sensitivity'])
Specificity.CART <- c(Specificity.CART,CM.CART$byClass['Specificity'])
PPV.CART <- c(PPV.CART,CM.CART$byClass['Pos Pred Value'])
NPV.CART <- c(NPV.CART,CM.CART$byClass['Neg Pred Value'])
F1.CART <- c(F1.CART,CM.CART$byClass['F1'])


# pred <- prediction(as.numeric(CART.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)

# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

# AUC.CART <- c(AUC.CART, unlist(AUC.perf@y.values))

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(CART.Predict))

AUC.CART <- c(AUC.CART, AUC.perf)

}

Accuracy.CART_boot <- mean(Accuracy.CART)
PPV.CART_boot <- mean(PPV.CART)
NPV.CART_boot <- mean(NPV.CART)
Sensitivity.CART_boot <- mean(Sensitivity.CART)
Specificity.CART_boot <- mean(Specificity.CART)
F1.CART_boot <- mean(F1.CART)
AUC.CART_boot <- mean(AUC.CART)

CART.statistics <- list(Accuracy.CART=Accuracy.CART,
                        PPV.CART=PPV.CART, 
                        NPV.CART=NPV.CART,
                        Sensitivity.CART=Sensitivity.CART,
         Specificity.CART=Specificity.CART, 
         F1.CART=F1.CART, 
         AUC.CART=AUC.CART)
save(CART.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/CART.statistics.RData")
