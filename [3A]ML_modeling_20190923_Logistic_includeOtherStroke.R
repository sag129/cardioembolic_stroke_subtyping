## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----classifier 1--------------------------------------------------------
# logisitic regression
# 2) Using machine learning package "caret" ====
library(caret)
library(dplyr)
library(pROC)

load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/feature_table_wide4.RData")

features_only <- feature_table_wide4
myvars <- names(feature_table_wide4) %in% c("EMPI", "MRN", "stroke_date_index", 
                                            "Toast", "CCS","Toast_new","Toast_final",
                                            "age","Gender","gender") #remove missing cardiac sources, response variables (TOAST, CCS)
features_only <- feature_table_wide4[!myvars]

# for supplemental materials
glm.fits0=glm(Toast_final ~ .-EMPI-stroke_date_index-Toast-CCS-Toast_new-gender,
              data = feature_table_wide4, family = binomial)
coef(summary(glm.fits0))
summary(glm.fits0)
options("scipen"=100, "digits"=3)
# (summary(glm.fits0)$coefficients[,1]-1.96*summary(glm.fits0)$coefficients[,2])
# (summary(glm.fits0)$coefficients[,1]+1.96*summary(glm.fits0)$coefficients[,2])
as.data.frame(exp(summary(glm.fits0)$coefficients[,1]))
exp(confint(glm.fits0))

library(car)
# Calculate the asymptotic P-value for anything <2e-16 in R using a wald chisq for each variable
model_af <- glm(Toast_final ~ af, 
                data = feature_table_wide4, 
                family = binomial(link="logit"))
Anova(model_af, type="II", test="Wald")



# varimp_mars <- varImp(model_mars)
# plot(varimp_mars, main="Variable Importance with MARS")
# testData4 <- predict(preProcess_range_model, testData3)

# for Dichotomized classifier
cnt <- 1

Accuracy.Logistic1 <- c()
Sensitivity.Logistic1 <- c()
Specificity.Logistic1 <- c()
PPV.Logistic1 <- c()
NPV.Logistic1 <- c()
F1.Logistic1 <- c()
AUC.Logistic1 <- c()

for (i in 1:50) {
set.seed(i)

df0 = feature_table_wide4
df0$feature_cnt <- rowSums(df0[,names(features_only)])
# df0$feature_cnt1 <- (df0$feature_cnt)
# for Dichotomized classifier
df0$feature_cnt1 <- (df0$feature_cnt >= cnt)*1
table(df0$feature_cnt)

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

  table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
Logistic.Train <- train(as.factor(Toast_final) ~ feature_cnt1 + age + Gender -doc.class, data = train.doc1,method="glm",family=binomial, trControl = ctrl)
Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Logistic.Train$finalModel$coefficients

Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Accuracy.Logistic1 <- c(Accuracy.Logistic1, CM.Logistic$overall[c('Accuracy')])
Sensitivity.Logistic1 <- c(Sensitivity.Logistic1,CM.Logistic$byClass['Sensitivity'])
Specificity.Logistic1 <- c(Specificity.Logistic1,CM.Logistic$byClass['Specificity'])
PPV.Logistic1 <- c(PPV.Logistic1,CM.Logistic$byClass['Pos Pred Value'])
NPV.Logistic1 <- c(NPV.Logistic1,CM.Logistic$byClass['Neg Pred Value'])
F1.Logistic1 <- c(F1.Logistic1,CM.Logistic$byClass['F1'])

# pred <- prediction(as.numeric(Logistic.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)

# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(Logistic.Predict))

AUC.Logistic1 <- c(AUC.Logistic1, AUC.perf)
}

# Logistic.Train$finalModel

# results
# colMeans(results)

# results
# vecprops
# AUC

Logistic1.statistics <- list(Accuracy.Logistic1=Accuracy.Logistic1,
                             PPV.Logistic1=PPV.Logistic1,
                             NPV.Logistic1=NPV.Logistic1,
                             Sensitivity.Logistic1=Sensitivity.Logistic1,
                             Specificity.Logistic1=Specificity.Logistic1,
                             F1.Logistic1=F1.Logistic1,
                             AUC.Logistic1=AUC.Logistic1)
save(Logistic1.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic1.statistics.RData")

## ----classifier 2--------------------------------------------------------
# logisitic regression
# 2) Using machine learning package "caret" ====
library(caret)

# varimp_mars <- varImp(model_mars)
# plot(varimp_mars, main="Variable Importance with MARS")
# testData4 <- predict(preProcess_range_model, testData3)

# for Dichotomized classifier
cnt <- 2

Accuracy.Logistic2 <- c()
Sensitivity.Logistic2 <- c()
Specificity.Logistic2 <- c()
PPV.Logistic2 <- c()
NPV.Logistic2 <- c()
F1.Logistic2 <- c()
AUC.Logistic2 <- c()

for (i in 1:50) {
set.seed(i)

df0 = feature_table_wide4
df0$feature_cnt <- rowSums(df0[,names(features_only)])
# df0$feature_cnt1 <- (df0$feature_cnt)
# for Dichotomized classifier
df0$feature_cnt1 <- (df0$feature_cnt >= cnt)*1
table(df0$feature_cnt)

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

  table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
Logistic.Train <- train(as.factor(Toast_final) ~ feature_cnt1 + age + Gender -doc.class, data = train.doc1,method="glm",family=binomial, trControl = ctrl)
Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Logistic.Train$finalModel$coefficients

Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Accuracy.Logistic2 <- c(Accuracy.Logistic2, CM.Logistic$overall[c('Accuracy')])
Sensitivity.Logistic2 <- c(Sensitivity.Logistic2,CM.Logistic$byClass['Sensitivity'])
Specificity.Logistic2 <- c(Specificity.Logistic2,CM.Logistic$byClass['Specificity'])
PPV.Logistic2 <- c(PPV.Logistic2,CM.Logistic$byClass['Pos Pred Value'])
NPV.Logistic2 <- c(NPV.Logistic2,CM.Logistic$byClass['Neg Pred Value'])
F1.Logistic2 <- c(F1.Logistic2,CM.Logistic$byClass['F1'])

# Fscore.Logistic <- CM.Logistic$byClass[7]
# print(Fscore.Logistic) # 0.7811159 

# pred <- prediction(as.numeric(Logistic.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)

# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(Logistic.Predict))

AUC.Logistic2 <- c(AUC.Logistic2, AUC.perf)

}

# Logistic.Train$finalModel

Logistic2.statistics <- list(Accuracy.Logistic2=Accuracy.Logistic2, Sensitivity.Logistic2=Sensitivity.Logistic2, Specificity.Logistic2=Specificity.Logistic2, PPV.Logistic2=PPV.Logistic2,
                             NPV.Logistic2=NPV.Logistic2, F1.Logistic2=F1.Logistic2,
                             AUC.Logistic2=AUC.Logistic2)
save(Logistic2.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic2.statistics.RData")

## ----classifier 3--------------------------------------------------------
# logisitic regression
# 2) Using machine learning package "caret" ====
library(caret)

# varimp_mars <- varImp(model_mars)
# plot(varimp_mars, main="Variable Importance with MARS")
# testData4 <- predict(preProcess_range_model, testData3)

# for Dichotomized classifier
cnt <- 3

Accuracy.Logistic3 <- c()
Sensitivity.Logistic3 <- c()
Specificity.Logistic3 <- c()
PPV.Logistic3 <- c()
NPV.Logistic3 <- c()
F1.Logistic3 <- c()
AUC.Logistic3 <- c()

for (i in 1:50) {
set.seed(i)

df0 = feature_table_wide4
df0$feature_cnt <- rowSums(df0[,names(features_only)])
# df0$feature_cnt1 <- (df0$feature_cnt)
# for Dichotomized classifier
df0$feature_cnt1 <- (df0$feature_cnt >= cnt)*1
table(df0$feature_cnt)

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

  table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
Logistic.Train <- train(as.factor(Toast_final) ~ feature_cnt1 + age + Gender -doc.class, data = train.doc1,method="glm",family=binomial, trControl = ctrl)
Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Logistic.Train$finalModel$coefficients

Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Accuracy.Logistic3 <- c(Accuracy.Logistic3, CM.Logistic$overall[c('Accuracy')])
Sensitivity.Logistic3 <- c(Sensitivity.Logistic3,CM.Logistic$byClass['Sensitivity'])
Specificity.Logistic3 <- c(Specificity.Logistic3,CM.Logistic$byClass['Specificity'])
PPV.Logistic3 <- c(PPV.Logistic3,CM.Logistic$byClass['Pos Pred Value'])
NPV.Logistic3 <- c(NPV.Logistic3,CM.Logistic$byClass['Neg Pred Value'])
F1.Logistic3 <- c(F1.Logistic3,CM.Logistic$byClass['F1'])

# Fscore.Logistic <- CM.Logistic$byClass[7]
# print(Fscore.Logistic) # 0.7811159 

# pred <- prediction(as.numeric(Logistic.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)
# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(Logistic.Predict))

AUC.Logistic3 <- c(AUC.Logistic3, AUC.perf)

}

# Logistic.Train$finalModel

# results
# colMeans(results)

# results
# vecprops
# AUC

Logistic3.statistics <- list(Accuracy.Logistic3=Accuracy.Logistic3,
                             Sensitivity.Logistic3=Sensitivity.Logistic3,
                             Specificity.Logistic3=Specificity.Logistic3,
                             PPV.Logistic3=PPV.Logistic3,
                             NPV.Logistic3=NPV.Logistic3,
                             F1.Logistic3=F1.Logistic3,
                             AUC.Logistic3=AUC.Logistic3)
save(Logistic3.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic3.statistics.RData")

## ----score---------------------------------------------------------------
# logisitic regression
# 2) Using machine learning package "caret" ====
library(caret)

# varimp_mars <- varImp(model_mars)
# plot(varimp_mars, main="Variable Importance with MARS")
# testData4 <- predict(preProcess_range_model, testData3)

Accuracy.Logistic.score <- c()
Sensitivity.Logistic.score <- c()
Specificity.Logistic.score <- c()
PPV.Logistic.score <- c()
NPV.Logistic.score <- c()
F1.Logistic.score <- c()
AUC.Logistic.score <- c()

for (i in 1:50) {
set.seed(i)

df0 = feature_table_wide4
df0$feature_cnt <- rowSums(df0[,names(features_only)])
df0$feature_cnt1 <- (df0$feature_cnt)
# for Dichotomized classifier
# df0$feature_cnt1 <- (df0$feature_cnt >= cnt)*1
table(df0$feature_cnt)

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

  table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
Logistic.Train <- train(as.factor(Toast_final) ~ feature_cnt1 + age + Gender -doc.class, data = train.doc1,method="glm",family=binomial, trControl = ctrl)
Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Logistic.Train$finalModel$coefficients

Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Accuracy.Logistic.score <- c(Accuracy.Logistic.score, CM.Logistic$overall[c('Accuracy')])
Sensitivity.Logistic.score <- c(Sensitivity.Logistic.score,CM.Logistic$byClass['Sensitivity'])
Specificity.Logistic.score <- c(Specificity.Logistic.score,CM.Logistic$byClass['Specificity'])
PPV.Logistic.score <- c(PPV.Logistic.score,CM.Logistic$byClass['Pos Pred Value'])
NPV.Logistic.score <- c(NPV.Logistic.score,CM.Logistic$byClass['Neg Pred Value'])
F1.Logistic.score <- c(F1.Logistic.score,CM.Logistic$byClass['F1'])

# Fscore.Logistic <- CM.Logistic$byClass[7]
# print(Fscore.Logistic) # 0.7811159 

# pred <- prediction(as.numeric(Logistic.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)

# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values
AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(Logistic.Predict))

AUC.Logistic.score <- c(AUC.Logistic.score, AUC.perf)

}

# Logistic.Train$finalModel

Logistic.scores.statistics <- list(Accuracy.Logistic.score=Accuracy.Logistic.score,
                                   PPV.Logistic.score=PPV.Logistic.score,
                                   NPV.Logistic.score=NPV.Logistic.score, 
                              Sensitivity.Logistic.score=Sensitivity.Logistic.score,
                                   Specificity.Logistic.score=Specificity.Logistic.score,
                              F1.Logistic.score=F1.Logistic.score, AUC.Logistic.score=AUC.Logistic.score)
save(Logistic.scores.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic.score.statistics.RData")


## ----logistic regression-------------------------------------------------
# logisitic regression
# 2) Using machine learning package "caret" ====
library(caret)

Accuracy.Logistic <- c()
Sensitivity.Logistic <- c()
Specificity.Logistic <- c()
PPV.Logistic <- c()
NPV.Logistic <- c()
F1.Logistic <- c()
AUC.Logistic <- c()

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

  table(df2[train.Range,c('Toast_final')]) %>% prop.table()
  table(df2[test.Range,c('Toast_final')]) %>% prop.table()
  
# Prepare data by adding to DTM a column of document's class
train.doc1 <- as.data.frame(train.doc, stringsAsFactors = FALSE); train.doc1$doc.class <- as.character(Tags.Train)
test.doc1 <- as.data.frame(test.doc, stringsAsFactors = FALSE); test.doc1$doc.class <- as.character(Tags.Test)

# To reduce overfitting in the model, repeated 5-fold cross-validation is used.  That is, the training data is split into 10 subsets (folds).  Then, algorithms are run 10 times each time using a different fold as the training date.  This process is repeated 100 separate times and the average error across all trials is computed to help reduce overfitting in the models.
# set resampling scheme
ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 10)

# train
Logistic.Train <- train(as.factor(Toast_final) ~ Gender*hypo_lvs  -doc.class, data = train.doc1,method="glm",family=binomial, trControl = ctrl)
Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Logistic.Train$finalModel$coefficients
Logistic.Predict <- predict(Logistic.Train, newdata = test.doc1) # Give prediction on the test data

CM.Logistic <- confusionMatrix(Logistic.Predict, as.factor(test.doc1$doc.class));
Accuracy.Logistic <- c(Accuracy.Logistic, CM.Logistic$overall[c('Accuracy')])
Sensitivity.Logistic <- c(Sensitivity.Logistic,CM.Logistic$byClass['Sensitivity'])
Specificity.Logistic <- c(Specificity.Logistic,CM.Logistic$byClass['Specificity'])
PPV.Logistic <- c(PPV.Logistic,CM.Logistic$byClass['Pos Pred Value'])
NPV.Logistic <- c(NPV.Logistic,CM.Logistic$byClass['Neg Pred Value'])
F1.Logistic <- c(F1.Logistic,CM.Logistic$byClass['Neg Pred Value'])

# pred <- prediction(as.numeric(Logistic.Predict), test.doc1$Toast_final)
# perf <- performance(pred,"tpr","fpr")
# plot(perf,colorize=TRUE)
# abline(a=0, b= 1)

# AUC.perf = performance(pred, measure = "auc")
# AUC.perf@y.values

AUC.perf <- auc(response=test.doc1$Toast_final, predictor=as.numeric(Logistic.Predict))

AUC.Logistic <- c(AUC.Logistic, AUC.perf)

}

# results
# colMeans(results)

# results
# vecprops
# AUC


Logistic.statistics <- list(Accuracy.Logistic=Accuracy.Logistic,
                        Sensitivity.Logistic=Sensitivity.Logistic,
                        Specificity.Logistic=Specificity.Logistic,
                        PPV.Logistic=PPV.Logistic,
                        NPV.Logistic=NPV.Logistic,
                        F1.Logistic=F1.Logistic,
                        AUC.Logistic=AUC.Logistic)
save(Logistic.statistics, file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic.statistics.RData")

