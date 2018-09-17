# Geoduck Conditioning

# Script for statistical analysis of Pacific geoduck (Panopea genoerosa) respiration data from Summer 2018
# written by Samuel J. Gurr in September 2018
# Supported by: FFAR
install.packages("tidyverse")
library(tidyverse)
library(nlme) 
?tidyverse
#set working directory---------------------------------------------------------------------------------------------

setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv/Cumulative_output")
main<-getwd()

#path and file set-up---------------------------------------------------------------------------------------------
path<-"R_script_&_datasheets" #the location of all your titration files
respfile_trial1<-"Cumulative_resp_trial_1.csv" # name of your file with resp data
trial1<-read.csv(file.path(path,respfile_trial1), header=T, sep=",", na.string="NA", as.is=T) 

#check file---------------------------------------------------------------------------------------------
trial1
names(trial1)

#name columns---------------------------------------------------------------------------------------------
Treatment <- trial1$Treat1_Treat2
conical <- trial1$conical
heathtray <- trial1$tank
resp_nmolL<- trial1$rate_calc_nmol.L
resp_ugL <- trial1$rate_calc_ug.L
day <- trial1$Day_Trial

# Divide dataset by each day to test for concial and heathtray effects-----------------------------------------------------
# Day 2 2080716
day2 <- trial1[1:24,]
day2
LOWday2 <- day2[(c(1:3,7:9,16:18,22:24)),]
AMBday2 <- day2[(c(4:6,10:15,19:21)),]

# Day 5 20180719
day5 <- trial1[25:48,]
day5
LOWday5 <- day5[(c(1:3,7:9,16:18,22:24)),]
AMBday5 <- day5[(c(4:6,10:15,19:21)),]

# Day 8 20180722
day8<- trial1[49:72,]
day8
LOWday8 <- day8[(c(1:3,7:9,16:18,22:24)),]
AMBday8 <- day8[(c(4:6,10:15,19:21)),]

# Day 10 20180724
day10 <- trial1[73:96,]
day10
LOWday10 <- day10[(c(1:3,7:9,16:18,22:24)),]
AMBday10 <- day10[(c(4:6,10:15,19:21)),]

# anova and post hoc Tukey HSD for effects 
# RESULTS OF BELOW:
# Day 2 CONICALS - LOW (no diff), AMB (no diff), HEATHTRAY - LOW (no diff), AMB (TUKEY - diff between H2_T-H1_T; p = 0.03)
# Day 5 CONICALS - LOW (no diff), AMB (no diff), HEATHTRAY - LOW (no diff), AMB (no diff)
# Day 8 CONICALS - LOW (no diff), AMB (sig diff anova conical 1 and 2 p=0.0185 *), HEATHTRAY - LOW (no diff), AMB (no diff)
# Day 10 CONICALS - LOW (no diff), AMB (no diff), HEATHTRAY - LOW (no diff), AMB (no diff)

# Day 2 2080716_____________________________________________________________________
# CONICAL EFFECT
par(mfrow=c(2,2))
# LOW treat (LOWday2)
d2LOW<-aov(LOWday2$rate_calc_ug.L~LOWday2$conical)
summary(d2LOW)
boxplot(LOWday2$rate_calc_ug.L~LOWday2$conical, main ="LOW-Day2", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
# AMB treat (AMBday2)
d2AMB<-aov(AMBday2$rate_calc_ug.L~AMBday2$conical)
summary(d2AMB)
boxplot(AMBday2$rate_calc_ug.L~AMBday2$conical, main ="AMB-Day2", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
#HEATHTRAY EFFECT
# LOW treat (LOWday2)
d2LOW_ht <-aov(LOWday2$rate_calc_ug.L~LOWday2$tank)
summary(d2LOW_ht)
boxplot(LOWday2$rate_calc_ug.L~LOWday2$tank, main ="Low-Day2", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")
# AMB treat (AMBday2)                             SIGNIFICANT DIFFERENCE BETWEEN HEATHTRAYS  H2_T-H1_T HERE!!!!!!!!!!
d2AMB_ht <-aov(AMBday2$rate_calc_ug.L~AMBday2$tank)
summary(d2AMB_ht)
boxplot(AMBday2$rate_calc_ug.L~AMBday2$tank, main ="AMB-Day2", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")
TukeyHSD(d2AMB_ht) # diff between H2_T-H1_T


# Day 5 2080719_____________________________________________________________________
# CONICAL EFFECT
par(mfrow=c(2,2))
# LOW treat (LOWday5)
d5LOW<-aov(LOWday5$rate_calc_ug.L~LOWday5$conical)
summary(d5LOW)
boxplot(LOWday5$rate_calc_ug.L~LOWday5$conical, main ="LOW-Day5", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
# AMB treat (AMBday5)
d5AMB<-aov(AMBday5$rate_calc_ug.L~AMBday5$conical)
summary(d5AMB)
boxplot(AMBday5$rate_calc_ug.L~AMBday5$conical, main ="AMB-Day5", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
#HEATHTRAY EFFECT
# LOW treat (LOWday5)
d5LOW_ht <-aov(LOWday5$rate_calc_ug.L~LOWday5$tank)
summary(d5LOW_ht)
boxplot(LOWday5$rate_calc_ug.L~LOWday5$tank, main ="LOW-Day5", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")
# AMB treat (AMBday5) 
d5AMB_ht <-aov(AMBday5$rate_calc_ug.L~AMBday5$tank)
summary(d5AMB_ht)
boxplot(AMBday5$rate_calc_ug.L~AMBday5$tank, main ="AMB-Day5", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")
 

# Day 8 2080722_____________________________________________________________________
# CONICAL EFFECT
par(mfrow=c(2,2))
# LOW treat (LOWday8)
d8LOW<-aov(LOWday8$rate_calc_ug.L~LOWday8$conical)
summary(d8LOW)
boxplot(LOWday8$rate_calc_ug.L~LOWday8$conical, main ="LOW-Day8", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
# AMB treat (AMBday8)                                  SIGNIFICANT DIFFERENCE BETWEEN CONICALS HERE!!!!!!!!!!
d8AMB<-aov(AMBday8$rate_calc_ug.L~AMBday8$conical)
summary(d8AMB)
boxplot(AMBday8$rate_calc_ug.L~AMBday8$conical, main ="AMB-Day8", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
#HEATHTRAY EFFECT
# LOW treat (LOWday8)
d8LOW_ht <-aov(LOWday8$rate_calc_ug.L~LOWday8$tank)
summary(d8LOW_ht)
boxplot(LOWday8$rate_calc_ug.L~LOWday8$tank, main ="LOW-Day8", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")
# AMB treat (AMBday8) 
d8AMB_ht <-aov(AMBday8$rate_calc_ug.L~AMBday8$tank)
summary(d8AMB_ht)
boxplot(AMBday8$rate_calc_ug.L~AMBday8$tank, main ="AMB-Day8", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")

# Day 10 20180724_____________________________________________________________________
# CONICAL EFFECT
par(mfrow=c(2,2))
# LOW treat (LOWday10)
d10LOW<-aov(LOWday10$rate_calc_ug.L~LOWday10$conical)
summary(d10LOW)
boxplot(LOWday10$rate_calc_ug.L~LOWday10$conical, main ="LOW-Day10", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
# AMB treat (AMBday10)
d10AMB<-aov(AMBday10$rate_calc_ug.L~AMBday10$conical)
summary(d10AMB)
boxplot(AMBday10$rate_calc_ug.L~AMBday10$conical, main ="AMB-Day10", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="conical")
#HEATHTRAY EFFECT
# LOW treat (LOWday10)
d10LOW_ht <-aov(LOWday10$rate_calc_ug.L~LOWday10$tank)
summary(d10LOW_ht)
boxplot(LOWday10$rate_calc_ug.L~LOWday10$tank, main ="LOW-Day10", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")
# AMB treat (AMBday10) 
d10AMB_ht <-aov(AMBday10$rate_calc_ug.L~AMBday10$tank)
summary(d10AMB_ht)
boxplot(AMBday10$rate_calc_ug.L~AMBday10$tank, main ="AMB-Day10", ylab= "resp rate ug L-1 h-1 indiv-1", xlab="heath tray")



# REPEATED MEASURES------------------------------------------------------------------------------------------------------
# convert variables to factor

RMdata <- within(trial1, {
  treatment <- factor(trial1$Treat1_Treat2)
  time <- factor(trial1$Day_Trial)
  ID <- factor(trial1$tank)
})

with(RMdata, interaction.plot(time, treatment, trial1$rate_calc_ug.L,
                              ylim = c(0,0.4), lty = c(1,12), lwd = 2,
                              ylab = "respiration rate ug O2 L-1 h-1 indiv-1", xlab = "day", trace.label = "treatment"))

RMdata.aov <- aov(trial1$rate_calc_ug.L ~ treatment * time + Error(ID), data = RMdata)
summary(RMdata.aov)

# Boxplot with all datapoints
par(mfrow=c(1,1))
#combine day and treatment
trial1$Day_Treatment <- paste(trial1$Day_Trial, trial1$Treat1_Treat2)

boxplot(trial1$rate_calc_ug.L ~ trial1$Day_Treatment, lwd = 2, ylab = 'respiration rate ug O2 L-1 h-1 indiv-1', data=RMdata)
stripchart(trial1$rate_calc_ug.L ~ trial1$Day_Treatment, vertical = TRUE, data=RMdata, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue')
