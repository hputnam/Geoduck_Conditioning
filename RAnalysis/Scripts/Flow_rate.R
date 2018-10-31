#Title: Juvenile Repeat Exposure Experiment 2018
#Project: FFAR
#Author: Sam Gurr
#Edited by: Sam Gurr
#Date Last Modified: 20181031
#See Readme file for details

rm(list=ls())

##Install and load packages
library(Rmisc)

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/") #set working

#---------------------------------------------------------------------------------------------------
# Flow rate data 
########################################################################################################################
# NOTE: heath tray pairs were gravity-fed SW from conical overflow (1 conical to every heath tray pair)
# conicals were set to 1 LPM and teed with PVC to target 500 LPM in each heath tray

flow<-read.csv("Data/Flow_rates.csv", header=T, sep=",", na.string="NA", as.is=T) #upload file
flow # view the data

EXP1 <- subset(flow, Exp.num=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
flow_EXP1 <- subset(EXP1, Day!=0) # ommit day 0
EXP2 <- subset(flow, Exp.num=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
flow_EXP2 <- subset(EXP2, Day!=0) # ommit day 0
flow_EXP1_2 <- rbind(flow_EXP1, flow_EXP2) # bind exp1 and 2, day 0 already ommited


# EXP1 summary -----------------------
flow_EXP1.TRMT<- do.call(data.frame,aggregate(LPM ~ Treatment*Day, 
                                                   data = flow_EXP1, 
                                                   function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by treatment and Day (4 tanks ber treatment)
flow_EXP1.TRMT.summary <- summarySE(flow_EXP1.TRMT, measurevar="LPM.mean", groupvars=c("Treatment")) # summary by treatment


# EXP2 summary -----------------------
# grouped as 2 treatments (just Sec.treat) during reciprocal exposure
# NOTE: this shows flow difference influenced by conical overflow to heath tray pairs
flow_EXP2.TRMT.2<- do.call(data.frame,aggregate(LPM ~ Sec.treat *Day, 
                                                data = flow_EXP2, 
                                                function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by treatment and Day (4 tanks ber treatment)
flow_EXP2.TRMT.2.summary <- summarySE(flow_EXP2.TRMT.2, measurevar="LPM.mean", groupvars=c("Sec.treat")) # summary by treatment
# grouped as 4 treatments during reciprocal exposure
# NOTE: this shows flow difference between treaments, but not influenced by the same conical overflow
flow_EXP2.TRMT.4<- do.call(data.frame,aggregate(LPM ~ Treatment*Day, 
                                              data = flow_EXP2, 
                                              function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by treatment and Day (4 tanks ber treatment)
flow_EXP2.TRMT.4.summary <- summarySE(flow_EXP2.TRMT.4, measurevar="LPM.mean", groupvars=c("Treatment")) # summary by treatment

# EXP1 AND EXP2 summary -----------------------
# group by just day for daily flow rates
flow_ALL.Date<- do.call(data.frame,aggregate(LPM ~ Date, 
                                                data = flow_EXP1_2, 
                                                function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by Date
flow_ALL.Date.summary <- summarySE(flow_ALL.Date, measurevar="LPM.mean") # summary by treatment
# na omited when notes read "adjusted"
flow_EXP1_2.OMIT <- subset(flow_EXP1_2, Notes!="adjusted") # ommit rows when flow rate was adjusted
flow_ALL.Date.OMIT<- do.call(data.frame,aggregate(LPM ~ Date, 
                                             data = flow_EXP1_2.OMIT, 
                                             function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by Date, adjusted ommited
flow_ALL.Date.summary.OMIT <- summarySE(flow_ALL.Date.OMIT, measurevar="LPM.mean") # summary by treatment
