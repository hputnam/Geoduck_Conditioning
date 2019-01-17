#Title: 
#Project: FFAR
#Author: Sam Gurr
#Edited by: Sam Gurr
#Date Last Modified: 20190115
#See Readme file for details

rm(list=ls()) # clears workspace 

##Install and load packages
library(ggplot2) 
library(lme4)
library(ggpubr)
library(nlme)
library(plotrix)
library(lsmeans)
library(gridExtra)
library(tidyverse)
library(Rmisc)

# Set Working Directory:
#setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working
setwd("C:/Users/samjg/Documents/Notebook/data/photos_size_and_misc/geoduck_pics/") #set working

#Load Size Data
size<-read.csv("20190115_heathstack_commongarden/20190115_shell_length.csv", header=T, sep=",", na.string="NA", as.is=T) 

size # look at the data
names(size) # look at names of data

# both treatments
size_table_Treat1Treat2 <- do.call(data.frame,aggregate(Length_mm ~ Treat1_Treat2,
                                           data = size, function(x) c(mean = mean(x), se = std.error(x))))
size_table_Treat1Treat2 # view the table

# analysis
size.aov.mod <- aov(Length_mm ~ Init_treat*Sec_treat, data = size) # run anova on treatment and time
anova(size.aov.mod) # significant effect fromboth inital and secondary treatment
# Levene's test for homogeneity 
# levene.test(size.aov.mod) 
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(size.aov.mod)) #plot histogram of residuals
boxplot(residuals(size.aov.mod)) #plot boxplot of residuals
plot(fitted(size.aov.mod),residuals(size.aov.mod))

sumLENGTH_means <- summarySE(size, 
                                measurevar="Length_mm", 
                                groupvars=c("Init_treat")) # summarize previous table for overall treatment 
sumLENGTH_means # view the table
percentdiff <- ((sumLENGTH_means[2,3] - sumLENGTH_means[1,3])/sumLENGTH_means[2,3])*100 # calculate percent difference
percentdiff # 5.8% greater shell length from animals initally exposed to low pH in initial exp trial

# significant effect graph
size_graph <- ggplot(size, aes(x = factor(size$Init_treat), y = size$Length_mm, fill = size$Init_treat)) +
              geom_boxplot(alpha = 0.1, outlier.shape = 19,
                           outlier.fill = "black", outlier.size = 1, lwd=0.2) + 
              geom_point(pch = 19, position = position_jitterdodge(0.05), size=1) +
              scale_color_grey() + scale_fill_grey() + theme_classic() +
              #geom_point(aes(fill = resp_EXP1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
              theme(legend.position = c(.9, .9), legend.text=element_text(size=8)) +
              stat_summary(fun.y=mean, geom="point", pch="+", size=3, position = position_jitterdodge(0.01)) +
              labs(y="Shell length (mm)",  x = "Initial Treatment", fill= "") 
size_graph # view the graph
