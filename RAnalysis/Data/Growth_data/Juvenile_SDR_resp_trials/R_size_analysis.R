###################################################################################################
rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(devtools)
library(easyGgplot2)
#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/Growth_data/")
main<-getwd()
# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Juvenile_SDR_resp_trials" #the locationv of all your respiration files

########################
# make the dataframe
########################
size<-read.csv(file.path(path, ("All_size_Trials_1_and_2.csv")), header=T, sep=",", na.string="NA", as.is=T) 
########################
names(size)


# divide size into trials 1 and 2
size_Trial1 <- size[1:955,]
size_Trial2 <- size[956:1917,]

# test for normlity
shapiro.test(size_Trial1$shell_size) # Trial 1
hist(size_Trial1$shell_size) # Trial 1

shapiro.test(size_Trial2$shell_size) # Trial 1
hist(size_Trial2$shell_size) # Trial 

# make new columns to view size across tanks and treatment through time
# by tank as "Date_tank"
size$Date_tank <- paste(size$Date, size$tank, size$timeline_days, sep="_")
# by treatment as "Date_treat"
size$Date_treat <- paste(size$Date, size$treatment, sep="_")

########################
# summarize tables from dplyr as ddply(dataframe, "group name", summarise, mean = mean (Dep variable))
########################
# by tank
sizeMEANS_tank <- ddply(size, "Date_tank", summarise, mean = mean(shell_size))
sizeMEANS_tank
# by treatment
sizeMEANS_treat <- ddply(size, "Date_treat", summarise, mean = mean(shell_size))
sizeMEANS_treat

par(mfrow=c(4,2))
boxplot(sizeMEANS_tank[c(1,9,17,25,33,41,49,57),2]~
     sizeMEANS_tank[c(1,9,17,25,33,41,49,57),1], main ="H0_B_Low_LOW")

boxplot(sizeMEANS_tank[c(2,10,18,26,34,42,50,58),2]~
          sizeMEANS_tank[c(2,10,18,26,34,42,50,58),1], main ="H0_T_Low_Ambient")

boxplot(sizeMEANS_tank[c(3,11,19,27,35,43,51,59),2]~
          sizeMEANS_tank[c(3,11,19,27,35,43,51,59),1], main ="H1_B_Ambient_Ambient")

boxplot(sizeMEANS_tank[c(4,12,20,28,36,44,52,60),2]~
          sizeMEANS_tank[c(4,12,20,28,36,44,52,60),1], main ="H1_B_Ambient_Low")

boxplot(sizeMEANS_tank[c(5,13,21,29,37,45,53,61),2]~
          sizeMEANS_tank[c(5,13,21,29,37,45,53,61),1], main ="H2_B_Ambient_Ambient")

boxplot(sizeMEANS_tank[c(6,14,22,30,38,46,54,62),2]~
          sizeMEANS_tank[c(6,14,22,30,38,46,54,62),1], main ="H2_T_Ambient_Low")

boxplot(sizeMEANS_tank[c(7,15,23,31,39,47,55,63),2]~
          sizeMEANS_tank[c(7,15,23,31,39,47,55,63),1], main ="H3_B_Low_Low")

boxplot(sizeMEANS_tank[c(8,16,24,32,40,48,56,64),2]~
          sizeMEANS_tank[c(8,16,24,32,40,48,56,64),1], main ="H3_T_Low_Ambient")

sizeMEANS_tank
sizeMEANS_tank$timeline <- (substr(sizeMEANS_tank$Date_tank, 15, 16))
sizeMEANS_tank$tank  <- (substr(sizeMEANS_tank$Date_tank, 10, 13))



# get the x axis yileded correct order
sizeMEANS_tank$timeline <- gsub(" ", "", sizeMEANS_tank$timeline, fixed = TRUE)
sizeMEANS_tank$timeline <- factor(sizeMEANS_tank$timeline, ordered = TRUE,
                   levels = c("2" ,"5" , "8" ,"10" ,"24", "26", "28" ,"30"))

# plot it
ggplot2.scatterplot(data=sizeMEANS_tank, xName='timeline',yName='mean', 
                    groupName='tank', size=3, backgroundColor="white",
                    #groupColors=c('#999999','#E69F00', '#56B4E9'),
                    addRegLine=TRUE, fullrange=TRUE)  



growthrate <- ((sizeMEANS_tank[c(53,54,55,56,57,58,59,60),2]) - (sizeMEANS_tank[c(1,2,3,4,5,6,7,8),2]))/28
sizeMEANS_tank[c(53,54,55,56,57,58,59,60),1]
growthrate
