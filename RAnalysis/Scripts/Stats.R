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
library(ggpubr)
library(nlme)
library(plotrix)
library(lsmeans)

# Set Working Directory:
setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working

#Load Size Data
size<-read.csv("Data/All_growth_data.csv", header=T, sep=",", na.string="NA", as.is=T) 

size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals

# DIVIDE THE DATASET BETWEEN EXPERIMENTS 1 AND 2
size_EXP1 <- subset(size, Exp.Num=="Exp1")
size_EXP2 <- subset(size, Exp.Num=="Exp2")
size_EXP2.0 <- subset(size, Exp.Num=="Exp2")
size_EXP2.0 <-subset(size_EXP2.0, Day!=0)

# Visualize experiment 1


ggboxplot(size_EXP1, x = "Day", y = "shell_size", color = "treatment", ylab= "Size (mm)",
          palette = c("#00AFBB", "#E7B800"))

means.exp1 <- aggregate(shell_size ~ Day*treatment, data=size_EXP1, FUN=mean)

# Test for size differences in experiment 1
Init.lme <- lmer(shell_size ~ treatment + (1|Day/treatment/tank), data = size_EXP1) 

m1 <- lme(shell_size~treatment,random=~1|Day/treatment/tank,data=size_EXP1)
anova(m1)


par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m1)) #plot histogram of residuals
boxplot(residuals(m1)) #plot boxplot of residuals
plot( fitted(m1),residuals(m1)) #display residuals versus fitter, normal QQ plot, leverage plot

# NO STATISTICAL DIFFERENCE IN SIZE AFTER THE INITIAL EXPOSURE
ggboxplot(size_EXP2, x = "Day", y = "shell_size", color = "treatment", ylab= "Size (mm)",
          palette = c("light blue", "darkblue", "pink", "red"))

x2 <- do.call(data.frame,aggregate(shell_size ~ Day*Init.Trt*Sec.Trt, data = size_EXP2, function(x) c(mean = mean(x), se = std.error(x))))


m2 <- lme(shell_size~Init.Trt*Sec.Trt,random=~1|Day/Init.Trt/Sec.Trt,data=size_EXP2.0)
anova(m2)
summary(m2)

#Post hoc
m2.lsm <- lsmeans(m2,specs=c("Init.Trt", "Sec.Trt"), adjust="tukey") #compute least-squares means for Treatment*Day from ANOVA model
m2.lsm

par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2.1)) #plot histogram of residuals
boxplot(residuals(m2.1)) #plot boxplot of residuals
plot( fitted(m2.1),residuals(m2.1)) #display residuals versus fitter, normal QQ plot, leverage plot



#Exposure2 Plotting
#Day 0
Day0 <- subset(x2, Day==0)

Fig.Exp2.D0.size <- ggplot(Day0, aes(x=Sec.Trt, y=shell_size.mean, group=Init.Trt)) + 
  geom_errorbar(aes(ymin=Day0$shell_size.mean-Day0$shell_size.se, ymax=Day0$shell_size.mean+Day0$shell_size.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.Trt), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.Trt), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Shell size (mm)") +
  ylim(5,7) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 0") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D0.size

#Day 2
Day2 <- subset(x2, Day==2)

Fig.Exp2.D2.size <- ggplot(Day2, aes(x=Sec.Trt, y=shell_size.mean, group=Init.Trt)) + 
  geom_errorbar(aes(ymin=Day2$shell_size.mean-Day2$shell_size.se, ymax=Day2$shell_size.mean+Day2$shell_size.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.Trt), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.Trt), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Shell size (mm)") +
  ylim(5,7) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 2") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D2.size

#Day 4
Day4 <- subset(x2, Day==4)

Fig.Exp2.D4.size <- ggplot(Day4, aes(x=Sec.Trt, y=shell_size.mean, group=Init.Trt)) + 
  geom_errorbar(aes(ymin=Day4$shell_size.mean-Day4$shell_size.se, ymax=Day4$shell_size.mean+Day4$shell_size.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.Trt), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.Trt), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Shell size (mm)") +
  ylim(5,7) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 4") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D4.size

#Day 6
Day6 <- subset(x2, Day==6)

Fig.Exp2.D6.size <- ggplot(Day6, aes(x=Sec.Trt, y=shell_size.mean, group=Init.Trt)) + 
  geom_errorbar(aes(ymin=Day6$shell_size.mean-Day6$shell_size.se, ymax=Day6$shell_size.mean+Day6$shell_size.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.Trt), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.Trt), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Shell size (mm)") +
  ylim(5,7) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 6") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D6.size
