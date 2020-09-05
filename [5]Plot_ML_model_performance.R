load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/CART.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/KNN.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic1.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic2.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic3.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/Logistic.score.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/RF.statistics.RData")
load(file="/data/arrhythmia/wguan/cardioembolic_stroke/data/includeOtherStroke/SVM.statistics.RData")

models <- c("RF","SVM","Logistic","CART","KNN","Logistic - Feature count",
            "Logistic1", "Logistic2", "Logistic3")

# some performance values are NaN -> remove them

# RF
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("RF", 7),
                 mean = c(mean(RF.statistics$Accuracy.RF),
                          mean(RF.statistics$Sensitivity.RF),
                          mean(RF.statistics$Specificity.RF),
                          mean(RF.statistics$PPV.RF),
                          mean(RF.statistics$NPV.RF),
                          mean(RF.statistics$F1.RF),
                          mean(RF.statistics$AUC.RF)
                          ),
                 q025 = c(quantile(RF.statistics$Accuracy.RF, probs = c(0.025)),
                          quantile(RF.statistics$Sensitivity.RF, probs = c(0.025)),
                          quantile(RF.statistics$Specificity.RF, probs = c(0.025)),
                          quantile(RF.statistics$PPV.RF, probs = c(0.025)),
                          quantile(RF.statistics$NPV.RF, probs = c(0.025)),
                          quantile(RF.statistics$F1.RF, probs = c(0.025)),
                          quantile(RF.statistics$AUC.RF, probs = c(0.025))
                          ),
                 q975 = c(quantile(RF.statistics$Accuracy.RF, probs = c(0.975)),
                          quantile(RF.statistics$Sensitivity.RF, probs = c(0.975)),
                          quantile(RF.statistics$Specificity.RF, probs = c(0.975)),
                          quantile(RF.statistics$PPV.RF, probs = c(0.975)),
                          quantile(RF.statistics$NPV.RF, probs = c(0.975)),
                          quantile(RF.statistics$F1.RF, probs = c(0.975)),
                          quantile(RF.statistics$AUC.RF, probs = c(0.975))
                          )
                 )
df_name <- paste("df",which(models == "RF"),sep = "_")
assign(df_name, df)

# SVM
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("SVM", 7),
                 mean = c(mean(SVM.statistics$Accuracy.SVM),
                          mean(SVM.statistics$Sensitivity.SVM),
                          mean(SVM.statistics$Specificity.SVM),
                          mean(SVM.statistics$PPV.SVM),
                          mean(SVM.statistics$NPV.SVM),
                          mean(SVM.statistics$F1.SVM),
                          mean(SVM.statistics$AUC.SVM)
                 ),
                 q025 = c(quantile(SVM.statistics$Accuracy.SVM, probs = c(0.025)),
                          quantile(SVM.statistics$Sensitivity.SVM, probs = c(0.025)),
                          quantile(SVM.statistics$Specificity.SVM, probs = c(0.025)),
                          quantile(SVM.statistics$PPV.SVM, probs = c(0.025)),
                          quantile(SVM.statistics$NPV.SVM, probs = c(0.025)),
                          quantile(SVM.statistics$F1.SVM, probs = c(0.025)),
                          quantile(SVM.statistics$AUC.SVM, probs = c(0.025))
                 ),
                 q975 = c(quantile(SVM.statistics$Accuracy.SVM, probs = c(0.975)),
                          quantile(SVM.statistics$Sensitivity.SVM, probs = c(0.975)),
                          quantile(SVM.statistics$Specificity.SVM, probs = c(0.975)),
                          quantile(SVM.statistics$PPV.SVM, probs = c(0.975)),
                          quantile(SVM.statistics$NPV.SVM, probs = c(0.975)),
                          quantile(SVM.statistics$F1.SVM, probs = c(0.975)),
                          quantile(SVM.statistics$AUC.SVM, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "SVM"),sep = "_")
assign(df_name, df)

# Logistic
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("Logistic", 7),
                 mean = c(mean(Logistic.statistics$Accuracy.Logistic),
                          mean(Logistic.statistics$Sensitivity.Logistic),
                          mean(Logistic.statistics$Specificity.Logistic),
                          mean(Logistic.statistics$PPV.Logistic),
                          mean(Logistic.statistics$NPV.Logistic, na.rm = T),
                          mean(Logistic.statistics$F1.Logistic, na.rm = T),
                          mean(Logistic.statistics$AUC.Logistic)
                 ),
                 q025 = c(quantile(Logistic.statistics$Accuracy.Logistic, probs = c(0.025)),
                          quantile(Logistic.statistics$Sensitivity.Logistic, probs = c(0.025)),
                          quantile(Logistic.statistics$Specificity.Logistic, probs = c(0.025)),
                          quantile(Logistic.statistics$PPV.Logistic, probs = c(0.025)),
                          quantile(Logistic.statistics$NPV.Logistic, probs = c(0.025), na.rm = T),
                          quantile(Logistic.statistics$F1.Logistic, probs = c(0.025), na.rm = T),
                          quantile(Logistic.statistics$AUC.Logistic, probs = c(0.025))
                 ),
                 q975 = c(quantile(Logistic.statistics$Accuracy.Logistic, probs = c(0.975)),
                          quantile(Logistic.statistics$Sensitivity.Logistic, probs = c(0.975)),
                          quantile(Logistic.statistics$Specificity.Logistic, probs = c(0.975)),
                          quantile(Logistic.statistics$PPV.Logistic, probs = c(0.975)),
                          quantile(Logistic.statistics$NPV.Logistic, probs = c(0.975), na.rm = T),
                          quantile(Logistic.statistics$F1.Logistic, probs = c(0.975), na.rm = T),
                          quantile(Logistic.statistics$AUC.Logistic, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "Logistic"),sep = "_")
assign(df_name, df)

# CART
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("CART", 7),
                 mean = c(mean(CART.statistics$Accuracy.CART),
                          mean(CART.statistics$Sensitivity.CART),
                          mean(CART.statistics$Specificity.CART),
                          mean(CART.statistics$PPV.CART),
                          mean(CART.statistics$NPV.CART),
                          mean(CART.statistics$F1.CART),
                          mean(CART.statistics$AUC.CART)
                 ),
                 q025 = c(quantile(CART.statistics$Accuracy.CART, probs = c(0.025)),
                          quantile(CART.statistics$Sensitivity.CART, probs = c(0.025)),
                          quantile(CART.statistics$Specificity.CART, probs = c(0.025)),
                          quantile(CART.statistics$PPV.CART, probs = c(0.025)),
                          quantile(CART.statistics$NPV.CART, probs = c(0.025)),
                          quantile(CART.statistics$F1.CART, probs = c(0.025)),
                          quantile(CART.statistics$AUC.CART, probs = c(0.025))
                 ),
                 q975 = c(quantile(CART.statistics$Accuracy.CART, probs = c(0.975)),
                          quantile(CART.statistics$Sensitivity.CART, probs = c(0.975)),
                          quantile(CART.statistics$Specificity.CART, probs = c(0.975)),
                          quantile(CART.statistics$PPV.CART, probs = c(0.975)),
                          quantile(CART.statistics$NPV.CART, probs = c(0.975)),
                          quantile(CART.statistics$F1.CART, probs = c(0.975)),
                          quantile(CART.statistics$AUC.CART, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "CART"),sep = "_")
assign(df_name, df)

# KNN
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("KNN", 7),
                 mean = c(mean(KNN.statistics$Accuracy.KNN),
                          mean(KNN.statistics$Sensitivity.KNN),
                          mean(KNN.statistics$Specificity.KNN),
                          mean(KNN.statistics$PPV.KNN),
                          mean(KNN.statistics$NPV.KNN),
                          mean(KNN.statistics$F1.KNN),
                          mean(KNN.statistics$AUC.KNN)
                 ),
                 q025 = c(quantile(KNN.statistics$Accuracy.KNN, probs = c(0.025)),
                          quantile(KNN.statistics$Sensitivity.KNN, probs = c(0.025)),
                          quantile(KNN.statistics$Specificity.KNN, probs = c(0.025)),
                          quantile(KNN.statistics$PPV.KNN, probs = c(0.025)),
                          quantile(KNN.statistics$NPV.KNN, probs = c(0.025)),
                          quantile(KNN.statistics$F1.KNN, probs = c(0.025)),
                          quantile(KNN.statistics$AUC.KNN, probs = c(0.025))
                 ),
                 q975 = c(quantile(KNN.statistics$Accuracy.KNN, probs = c(0.975)),
                          quantile(KNN.statistics$Sensitivity.KNN, probs = c(0.975)),
                          quantile(KNN.statistics$Specificity.KNN, probs = c(0.975)),
                          quantile(KNN.statistics$PPV.KNN, probs = c(0.975)),
                          quantile(KNN.statistics$NPV.KNN, probs = c(0.975)),
                          quantile(KNN.statistics$F1.KNN, probs = c(0.975)),
                          quantile(KNN.statistics$AUC.KNN, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "KNN"),sep = "_")
assign(df_name, df)

# Logistic - Feature count
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("Logistic - Feature count", 7),
                 mean = c(mean(Logistic.scores.statistics$Accuracy.Logistic.score),
                          mean(Logistic.scores.statistics$Sensitivity.Logistic.score),
                          mean(Logistic.scores.statistics$Specificity.Logistic.score),
                          mean(Logistic.scores.statistics$PPV.Logistic.score),
                          mean(Logistic.scores.statistics$NPV.Logistic.score),
                          mean(Logistic.scores.statistics$F1.Logistic.score),
                          mean(Logistic.scores.statistics$AUC.Logistic.score)
                 ),
                 q025 = c(quantile(Logistic.scores.statistics$Accuracy.Logistic.score, probs = c(0.025)),
                          quantile(Logistic.scores.statistics$Sensitivity.Logistic.score, probs = c(0.025)),
                          quantile(Logistic.scores.statistics$Specificity.Logistic.score, probs = c(0.025)),
                          quantile(Logistic.scores.statistics$PPV.Logistic.score, probs = c(0.025)),
                          quantile(Logistic.scores.statistics$NPV.Logistic.score, probs = c(0.025)),
                          quantile(Logistic.scores.statistics$F1.Logistic.score, probs = c(0.025)),
                          quantile(Logistic.scores.statistics$AUC.Logistic.score, probs = c(0.025))
                 ),
                 q975 = c(quantile(Logistic.scores.statistics$Accuracy.Logistic.score, probs = c(0.975)),
                          quantile(Logistic.scores.statistics$Sensitivity.Logistic.score, probs = c(0.975)),
                          quantile(Logistic.scores.statistics$Specificity.Logistic.score, probs = c(0.975)),
                          quantile(Logistic.scores.statistics$PPV.Logistic.score, probs = c(0.975)),
                          quantile(Logistic.scores.statistics$NPV.Logistic.score, probs = c(0.975)),
                          quantile(Logistic.scores.statistics$F1.Logistic.score, probs = c(0.975)),
                          quantile(Logistic.scores.statistics$AUC.Logistic.score, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "Logistic - Feature count"),sep = "_")
assign(df_name, df)

# Logistic1
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("Logistic1", 7),
                 mean = c(mean(Logistic1.statistics$Accuracy.Logistic1),
                          mean(Logistic1.statistics$Sensitivity.Logistic1),
                          mean(Logistic1.statistics$Specificity.Logistic1),
                          mean(Logistic1.statistics$PPV.Logistic1),
                          mean(Logistic1.statistics$NPV.Logistic1, na.rm = T),
                          mean(Logistic1.statistics$F1.Logistic1),
                          mean(Logistic1.statistics$AUC.Logistic1)
                 ),
                 q025 = c(quantile(Logistic1.statistics$Accuracy.Logistic1, probs = c(0.025)),
                          quantile(Logistic1.statistics$Sensitivity.Logistic1, probs = c(0.025)),
                          quantile(Logistic1.statistics$Specificity.Logistic1, probs = c(0.025)),
                          quantile(Logistic1.statistics$PPV.Logistic1, probs = c(0.025)),
                          quantile(Logistic1.statistics$NPV.Logistic1, probs = c(0.025), na.rm = T),
                          quantile(Logistic1.statistics$F1.Logistic1, probs = c(0.025)),
                          quantile(Logistic1.statistics$AUC.Logistic1, probs = c(0.025))
                 ),
                 q975 = c(quantile(Logistic1.statistics$Accuracy.Logistic1, probs = c(0.975)),
                          quantile(Logistic1.statistics$Sensitivity.Logistic1, probs = c(0.975)),
                          quantile(Logistic1.statistics$Specificity.Logistic1, probs = c(0.975)),
                          quantile(Logistic1.statistics$PPV.Logistic1, probs = c(0.975)),
                          quantile(Logistic1.statistics$NPV.Logistic1, probs = c(0.975), na.rm = T),
                          quantile(Logistic1.statistics$F1.Logistic1, probs = c(0.975)),
                          quantile(Logistic1.statistics$AUC.Logistic1, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "Logistic1"),sep = "_")
assign(df_name, df)

# Logistic2
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("Logistic2", 7),
                 mean = c(mean(Logistic2.statistics$Accuracy.Logistic2),
                          mean(Logistic2.statistics$Sensitivity.Logistic2),
                          mean(Logistic2.statistics$Specificity.Logistic2),
                          mean(Logistic2.statistics$PPV.Logistic2),
                          mean(Logistic2.statistics$NPV.Logistic2),
                          mean(Logistic2.statistics$F1.Logistic2),
                          mean(Logistic2.statistics$AUC.Logistic2)
                 ),
                 q025 = c(quantile(Logistic2.statistics$Accuracy.Logistic2, probs = c(0.025)),
                          quantile(Logistic2.statistics$Sensitivity.Logistic2, probs = c(0.025)),
                          quantile(Logistic2.statistics$Specificity.Logistic2, probs = c(0.025)),
                          quantile(Logistic2.statistics$PPV.Logistic2, probs = c(0.025)),
                          quantile(Logistic2.statistics$NPV.Logistic2, probs = c(0.025)),
                          quantile(Logistic2.statistics$F1.Logistic2, probs = c(0.025)),
                          quantile(Logistic2.statistics$AUC.Logistic2, probs = c(0.025))
                 ),
                 q975 = c(quantile(Logistic2.statistics$Accuracy.Logistic2, probs = c(0.975)),
                          quantile(Logistic2.statistics$Sensitivity.Logistic2, probs = c(0.975)),
                          quantile(Logistic2.statistics$Specificity.Logistic2, probs = c(0.975)),
                          quantile(Logistic2.statistics$PPV.Logistic2, probs = c(0.975)),
                          quantile(Logistic2.statistics$NPV.Logistic2, probs = c(0.975)),
                          quantile(Logistic2.statistics$F1.Logistic2, probs = c(0.975)),
                          quantile(Logistic2.statistics$AUC.Logistic2, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "Logistic2"),sep = "_")
assign(df_name, df)

# Logistic3
df <- data.frame(measure = c("Accuracy", "Sensitivity", "Specificity", 
                             "PPV", "NPV",
                             "F1 Score", "AUC"),
                 model = rep("Logistic3", 7),
                 mean = c(mean(Logistic3.statistics$Accuracy.Logistic3),
                          mean(Logistic3.statistics$Sensitivity.Logistic3),
                          mean(Logistic3.statistics$Specificity.Logistic3),
                          mean(Logistic3.statistics$PPV.Logistic3),
                          mean(Logistic3.statistics$NPV.Logistic3),
                          mean(Logistic3.statistics$F1.Logistic3),
                          mean(Logistic3.statistics$AUC.Logistic3)
                 ),
                 q025 = c(quantile(Logistic3.statistics$Accuracy.Logistic3, probs = c(0.025)),
                          quantile(Logistic3.statistics$Sensitivity.Logistic3, probs = c(0.025)),
                          quantile(Logistic3.statistics$Specificity.Logistic3, probs = c(0.025)),
                          quantile(Logistic3.statistics$PPV.Logistic3, probs = c(0.025)),
                          quantile(Logistic3.statistics$NPV.Logistic3, probs = c(0.025)),
                          quantile(Logistic3.statistics$F1.Logistic3, probs = c(0.025)),
                          quantile(Logistic3.statistics$AUC.Logistic3, probs = c(0.025))
                 ),
                 q975 = c(quantile(Logistic3.statistics$Accuracy.Logistic3, probs = c(0.975)),
                          quantile(Logistic3.statistics$Sensitivity.Logistic3, probs = c(0.975)),
                          quantile(Logistic3.statistics$Specificity.Logistic3, probs = c(0.975)),
                          quantile(Logistic3.statistics$PPV.Logistic3, probs = c(0.975)),
                          quantile(Logistic3.statistics$NPV.Logistic3, probs = c(0.975)),
                          quantile(Logistic3.statistics$F1.Logistic3, probs = c(0.975)),
                          quantile(Logistic3.statistics$AUC.Logistic3, probs = c(0.975))
                 )
)
df_name <- paste("df",which(models == "Logistic3"),sep = "_")
assign(df_name, df)

master <- rbind(df_1,df_2,df_3,df_4,
                df_5,df_6,df_7,df_8,df_9)

library(ggplot2)
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#FF0000", "#0072B2", "#D55E00", "black","#CC79A7")

k <- ggplot(master, aes(measure, mean, ymin = q025, ymax = q975, color=model)) + 
  scale_colour_manual(name="Model",values=cbp1) + 
  ggtitle("Model Performance Across Metrics") +
  xlab("Metric") +
  ylab("Performance") +
  theme(text=element_text(family="Times New Roman", size=12), 
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom")

k + geom_pointrange(position = position_dodge(width = 0.55), size = 0.75, fatten = 2)

