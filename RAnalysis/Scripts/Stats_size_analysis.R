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
library(gridExtra)

# Set Working Directory:
#setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/") #set working

#Load Size Data
size<-read.csv("Data/All_growth_data.csv", header=T, sep=",", na.string="NA", as.is=T) 

size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals

# DIVIDE THE DATASET BETWEEN EXPERIMENTS 1 AND 2
size_EXP1 <- subset(size, Exp.Num=="Exp1")
size_EXP2 <- subset(size, Exp.Num=="Exp2")
size_EXP2.0 <- subset(size, Exp.Num=="Exp2")
size_EXP2.0 <-subset(size_EXP2.0, Day!=0)

# Test for difference in shell size between tanks to decide whether to standardize
size_EXP1_d2 <-subset(size_EXP1, !Day %in% c(5,8,10)) # only Day 2 exp1
size_EXP1_d10 <-subset(size_EXP1, !Day %in% c(2,5,8)) # only Day 10 exp1
size_EXP2_d0 <-subset(size_EXP2, !Day %in% c(2,4,6)) # only Day 0 exp2

# Model and visualize EXP1
tank.exp1 <- aov(shell_size~tank,data=size_EXP1_d2)
anova(tank.exp1) # no difference in shell size with tank
summary(tank.exp1)
#Post hoc
#exp2.tank <- lsmeans(tank.exp1, pairwise ~ tank)
Fig.d2.EXP1.tank <- ggboxplot(size_EXP1_d2, x = "Day", y = "shell_size", color = "tank", ylab= "Shell Size (mm)",
                              palette = c(), main= "Day 2 Exp 1")

# Model and visualize EXP1
tank.exp1.d10 <- aov(shell_size~tank,data=size_EXP1_d10)
anova(tank.exp1.d10) # no difference in shell size with tank
summary(tank.exp1.d10)
#Post hoc
#exp1.tank.d10 <- lsmeans(tank.exp1.d10, pairwise ~ tank)
Fig.d10.EXP1.tank <- ggboxplot(size_EXP1_d10, x = "Day", y = "shell_size", color = "tank", ylab= "Shell Size (mm)",
                              palette = c(), main= "Day 10 Exp 1")

# Model and visualize EXP2
tank.exp2 <- aov(shell_size~tank,data=size_EXP2_d0)
anova(tank.exp2) # difference in shell size with tank
summary(tank.exp2)
#Post hoc
exp2.tank <- lsmeans(tank.exp2, pairwise ~ tank) # significant difference in starting sizes
Fig.d0.EXP2.tank <- ggboxplot(size_EXP2_d0, x = "Day", y = "shell_size", color = "tank", ylab= "Shell Size (mm)",
                              palette = c(), main= "Day 0 Exp 2")



# Visualize experiment 1

Exp1.Fig <- ggboxplot(size_EXP1, x = "Day", y = "shell_size", color = "treatment", ylab= "Shell Size (mm)",
          palette = c("blue", "red"), main= "Initial Exposure")

Exp1.Fig

x1 <- do.call(data.frame,aggregate(shell_size ~ Day*treatment, data = size_EXP1, function(x) c(mean = mean(x), se = std.error(x))))

# Test for size differences in experiment 1
Init.lme <- lmer(shell_size ~ treatment*Day + (1|Day), data = size_EXP1) 

anova(Init.lme)
summary(Init.lme)

m1 <- lme(shell_size~treatment*Day,random=~1|Day,data=size_EXP1)
anova(m1)
EXP1.lme.size.anovatable <- anova(m1)

par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m1)) #plot histogram of residuals
boxplot(residuals(m1)) #plot boxplot of residuals
plot( fitted(m1),residuals(m1)) #display residuals versus fitter, normal QQ plot, leverage plot

# NO STATISTICAL DIFFERENCE IN SIZE AFTER THE INITIAL EXPOSURE


#Exposure 2
Exp2.Fig <- ggboxplot(size_EXP2, x = "Day", y = "shell_size", color = "treatment", ylab= "Size (mm)",
          palette = c("light blue", "darkblue", "pink", "red"))
Exp2.Fig


x2 <- do.call(data.frame,aggregate(shell_size ~ Day*Init.Trt*Sec.Trt, data = size_EXP2, function(x) c(mean = mean(x), se = std.error(x))))
x2.1 <- do.call(data.frame,aggregate(shell_size ~ Init.Trt*Sec.Trt, data = size_EXP2.0, function(x) c(mean = mean(x), se = std.error(x))))


m2 <- lme(shell_size~Init.Trt*Sec.Trt,random=~1|Day/Init.Trt/Sec.Trt,data=size_EXP2.0)
m2 <- lme(shell_size~Init.Trt*Sec.Trt,random=~1|Day/Init.Trt/Sec.Trt,data=size_EXP2.0)

anova(m2)
EXP2.lme.size.anovatable <- anova(m2)
summary(m2)


#Post hoc
exp2.ph <- lsmeans(m2, pairwise ~ Init.Trt*Sec.Trt)

par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2)) #plot histogram of residuals
boxplot(residuals(m2)) #plot boxplot of residuals
plot( fitted(m2),residuals(m2)) #display residuals versus fitter, normal QQ plot, leverage plot



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

#Averaged across Days

Fig.Exp2.All.size <- ggplot(x2.1, aes(x=Sec.Trt, y=shell_size.mean, group=Init.Trt)) + 
  geom_errorbar(aes(ymin=x2.1$shell_size.mean-x2.1$shell_size.se, ymax=x2.1$shell_size.mean+x2.1$shell_size.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
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
  ggtitle(" Secondary Exposure All Days") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.All.size

# Saving output plots

Figure1.Exp1.Size <- arrangeGrob(Exp1.Fig, ncol=1)
ggsave(file="Output/Geoduck_Size_Exp1.pdf", Figure1.Exp1.Size, width = 6.5, height = 8, units = c("in"))


Figure1.Exp2.Size <- arrangeGrob(Exp2.Fig,Fig.Exp2.D0.size, Fig.Exp2.D2.size,Fig.Exp2.D4.size,Fig.Exp2.D6.size, ncol=1)
ggsave(file="Output/Geoduck_Size_Exp2.byDay.pdf", Figure1.Exp2.Size, width = 6.5, height = 15, units = c("in"))

Figure1.Exp2.AllSize <- arrangeGrob(Fig.Exp2.All.size, ncol=1)
ggsave(file="Output/Geoduck_Size_Exp2.All.pdf", Figure1.Exp2.AllSize, width = 6.5, height = 8, units = c("in"))

# Saving anova tables 

Table1.Exp1.Size <- data.frame(EXP1.lme.size.anovatable)
write.csv(file="Output/Geoduck_Size.table_Exp1.csv", Table1.Exp1.Size)

Table12.Exp2.Size <- data.frame(EXP2.lme.size.anovatable)
write.csv(file="Output/Geoduck_Size.table_Exp2.csv", Table12.Exp2.Size)

