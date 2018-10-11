#Title: Juvenile Repeat Exposure Experiment 2018
#Project: FFAR
#Author: HM Putnam 
#Edited by: HM Putnam
#Date Last Modified: 20181011
#See Readme file for details

rm(list=ls()) #clears workspace 

##Install and load packages
library(ggplot2) 
library(lme4)

# Set Working Directory:
setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working

#Load Size Data
size<-read.csv("Data/All_growth_data.csv", header=T, sep=",", na.string="NA", as.is=T) 

size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals

# DIVIDE THE DATASET BETWEEN EXPERIMENTS 1 AND 2
size_EXP1 <- subset(size, Exp.Num=="Exp1")
size_EXP2 <- subset(size, Exp.Num=="Exp2")


# Visualize experiment 1

library("ggpubr")
ggboxplot(size_EXP1, x = "Day", y = "shell_size", color = "treatment",
          palette = c("#00AFBB", "#E7B800"))

Fig.Exp1.size <- ggplot(size_EXP1, aes(x=Day, y=shell_size, group=treatment)) +
  geom_point(aes(shape=treatment), size = 2, position = position_dodge(width = 0.6))
Fig.Exp1.size

Fig.Exp1.size_2 <- ggplot(size_EXP1, aes(x = factor(Day), y = shell_size, fill = tank)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = tank), size = 1, shape = 21, position = position_jitterdodge(0.3)) +
    theme(text = element_text(size = 18),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none")

print(Fig.Exp1.size_2 + labs(y="Shell size (mm)", x = "Day") + 
  ggtitle("Juvenile geoduck shell size \nin OA treatments EXP1"))

# Test for size differences in experiment 1
Init.lme <- lmer(shell_size ~ treatment + (1|Day/treatment/tank), data = size_EXP1) 

par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(Init.lme)) #plot histogram of residuals
boxplot(residuals(Init.lme)) #plot boxplot of residuals
plot( fitted(Init.lme),residuals(Init.lme)) #display residuals versus fitter, normal QQ plot, leverage plot


summary(Init.lme) # summary
anova(Init.lme) # anova



Fig.Exp2.size_2 <- ggplot(size_EXP2, aes(x = factor(Day), y = shell_size, fill = tank)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = tank), size = 1, shape = 21, position = position_jitterdodge(0.3)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

print(Fig.Exp2.size_2 + labs(y="Shell size (mm)", 
                             x = "Day") + 
        ggtitle("Juvenile geoduck shell size \nin OA treatments EXP2"))

