# install.packages("UpSetR")
library(UpSetR)
library(dplyr)

# load("//cifs2/secondaryaf$/Wyliena/Cardioembolic Stroke/analysis/feature_table_wide4.RData")

load(file="/Volumes/secondaryaf$/Wyliena/Cardioembolic Stroke/analysis/includeOtherStroke/feature_table_wide4.RData")

df <- feature_table_wide4

names(df) <- c("Toast","CCS","EMPI","stroke_date_index","age","Gender",
               "Atrial fibrillation",
               "Atrial flutter",
               "Akinetic left ventricular segment",
               "Atrial myxoma",
               "Atrial septal aneurysm",
               "Congestive heart failture",
               "Dilated cardiomyopathy",
               "Delayed emptying velocity",
               "Hypokinetic left ventricular segment",
               "Infective endocarditis",
               "Intracardiac thrombus",
               "Left atrial appendage thrombus",
               "Left atrial turbulence",
               "Left ventricular thrombus",
               "Mitral annulus calcification",
               "Mechanical and bioprosthetic valve",
               "MI (older)",
               "MI (recent)",
               "Mitral stenosis",
               "Mitral valve prolapse",
               "Nonbacterial endocarditis",
               "Patent foramen ovale",
               "Sick sinus syndrome",
               "Toast_new",
               "Toast_final",
               "Sex")

table(feature_table_wide4$pfo, feature_table_wide4$Toast)

df_noCE <- df %>% filter(Toast_final == 0) %>% select(-Toast_final, -Toast_new, -Sex)

df_CE <- df %>% filter(Toast_final == 1) %>% select(-Toast_final, -Toast_new, -Sex, -Toast)

# 'Frequency of Non-CE Strokes per Combination of TOAST Features'
upset(df_noCE, nsets = 30, number.angles = 30, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "# of Non-CE Stroke Patients per\nCombination of TOAST Features", sets.x.label = "Patients per TOAST Feature", 
      text.scale = c(1.5, 1.5, 1.25, 1.5, 1.5, 2), 
      main.bar.color = 'light green', sets.bar.color = 'light green',
      order.by = 'freq', nintersects = 10, mb.ratio = c(0.4, 0.6))

# 'Frequency of CE Strokes per Combination of TOAST Features'
upset(df_CE, nsets = 30, number.angles = 30, point.size = 3.5, line.size = 2, 
      mainbar.y.label = "# of CE Stroke Patients per\nCombination of TOAST Features", sets.x.label = "Patients per TOAST Feature", 
      text.scale = c(1.5, 1.5, 1.25, 1.5, 1.5, 2), 
      main.bar.color = 'red', sets.bar.color = 'red',
      order.by = 'freq', nintersects = 10, mb.ratio = c(0.4, 0.6))

