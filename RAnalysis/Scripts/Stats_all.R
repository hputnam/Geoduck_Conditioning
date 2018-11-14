#Title: Juvenile Repeat Exposure Experiment 2018
#Project: FFAR
#Author: HM Putnam & Sam Gurr
#Edited by: Sam Gurr
#Date Last Modified: 20181026
#See Readme file for details


rm(list=ls())
##Install and load packages
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(nlme)
library(lme4)
library(ggplot2) 
library(lme4)
library(ggpubr)
library(nlme)
library(plotrix)
library(lsmeans)
library(gridExtra)
library(reshape) 
library(plotrix) 
library(ggplot2) 
library(plyr)
library(gridExtra)
library(multcompView)
library(lsmeans)

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/") #set working

#---------------------------------------------------------------------------------------------------
# CONICAL Seawater chemistry Data - Analysis, Graphs, Tables (APEX DATA)
###################################################################################################

##### CONTINUOUS EXPERIMENTAL APEX DATA #####
#Load Apex Data 
APEX_1<-read.csv("Data/Apex_data/20180724_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_2<-read.csv("Data/Apex_data/20180801_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_3<-read.csv("Data/Apex_data/20180805_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_4<-read.csv("Data/Apex_data/20180814_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 

APEX_data<- do.call("rbind", list(APEX_1, APEX_2, APEX_3, APEX_4))

#APEX_pH_data.csv
#pH <- read.csv("Avtech_pH_data.csv", header=TRUE, sep=",", na.strings="NA") #load data with a header, separated by commas, with NA as NA
pH <- APEX_data
pH$Date.Time <-as.POSIXct(pH$Date.Time, format="%Y-%m-%d %H:%M:%OS") #convert date format
pH$Date <- as.Date(pH$Date.Time) #convert Date only
pH$Time <- format(as.POSIXct(pH$Date.Time) ,format = "%H:%M:%S") #convert Time only
names(pH)
pH.low_t0 <- do.call(data.frame,aggregate(pH_T0 ~ Date, data = pH, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day
pH.low_t3 <- do.call(data.frame,aggregate(pH_T3 ~ Date, data = pH, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day
pH.amb_t1 <- do.call(data.frame,aggregate(pH_T1 ~ Date, data = pH, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day
pH.amb_t2 <- do.call(data.frame,aggregate(pH_T2 ~ Date, data = pH, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day

pH.low_t0$Treatment <- "Low" #Add treatment Information
colnames(pH.low_t0) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
pH.low_t3$Treatment <- "Low" #Add treatment Information
colnames(pH.low_t3) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
pH.amb_t1$Treatment <- "Ambient" #Add treatment Information
colnames(pH.amb_t1) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
pH.amb_t2$Treatment <- "Ambient" #Add treatment Information
colnames(pH.amb_t2) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
daily.pH <- rbind(pH.low_t0, pH.low_t3, pH.amb_t1, pH.amb_t2) #bind treatment data 
daily.pH #view data

# Plot daily averages of pH data for the complete experiment (continuous APEX data)
All.pH <- ggplot(daily.pH, aes(x=Date, y=mean, group=Treatment)) + #set up plot information
  geom_errorbar(aes(ymin=daily.pH$mean-daily.pH$se, ymax=daily.pH$mean+daily.pH$se), colour="black", width=.1, position = position_dodge(width = 0.05)) + #add standard error bars about the mean
  geom_point(aes(shape=Treatment), size = 2, position = position_dodge(width = 0.05)) + #include points in the shape of the treatments
  annotate("text", x=as.Date("2018-07-15"), y=7.95, label = "No Data") + #add text to the graphic where data are missing
  annotate("text", x=as.Date("2018-08-15"), y=7.95, label = "No Data") + #add text to the graphic where data are missing
  xlab("Time") + #label x axis
  ylab("pH Total Scale") + # label y axis
  ylim(6.8,8.1) + # set y axis scale
  geom_vline(xintercept = 16899, linetype="dotted", color = "gray", size=1) + #add vertical line
  geom_vline(xintercept = 16928, linetype="dashed", color = "gray", size=1) + #add vertical line
  geom_vline(xintercept = 17013, linetype="dotted", color = "gray", size=1) + #add vertical line
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position=c(.65, .25), #set legend location
        legend.text = element_text(size = 8), #set the legend text size
        legend.key = element_blank(), #remove the legend background
        legend.title = element_text(size=8, face="bold")) + #set legend title attributes
  ggtitle("A) Experimental pH") + #add a main title
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0)) #set title attributes
All.pH #view plot

##### CONTINUOUS EXPERIMENTAL TEMPERATURE DATA #####
#APEX_temp_data.csv
temp <- APEX_data
temp$Date.Time <-as.POSIXct(temp$Date.Time, format="%Y-%m-%d %H:%M:%OS") #convert date format
temp$Date <- as.Date(temp$Date.Time) #convert Date only
temp$Time <- format(as.POSIXct(temp$Date.Time) ,format = "%H:%M:%S") #convert time only

names(temp)
temp.low_t0 <- do.call(data.frame,aggregate(TMP_T0 ~ Date, data = temp, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day
temp.low_t3 <- do.call(data.frame,aggregate(Temp_T3 ~ Date, data = temp, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day
temp.amb_t1 <- do.call(data.frame,aggregate(TMP_T1 ~ Date, data = temp, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day
temp.amb_t2 <- do.call(data.frame,aggregate(TMP_T2 ~ Date, data = temp, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Day

temp.low_t0$Treatment <- "Low" #Add treatment Information
colnames(temp.low_t0) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
temp.low_t3$Treatment <- "Low" #Add treatment Information
colnames(temp.low_t3) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
temp.amb_t1$Treatment <- "Ambient" #Add treatment Information
colnames(temp.amb_t1) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
temp.amb_t2$Treatment <- "Ambient" #Add treatment Information
colnames(temp.amb_t2) <- c("Date", "mean", "se", "Treatment") #rename columns to generic format
daily.temp <- rbind(temp.low_t0, temp.low_t3, temp.amb_t1, temp.amb_t2) #bind treatment data 
daily.temp #view data

# Plot daily averages of temp data for the complete experiment (continuous APEX data)
All.temp <- ggplot(daily.temp, aes(x=Date, y=mean, group=Treatment)) + #set up plot information
  geom_errorbar(aes(ymin=daily.temp$mean-daily.temp$se, ymax=daily.temp$mean+daily.temp$se), colour="black", width=.1, position = position_dodge(width = 0.05)) + #add standard error bars about the mean
  geom_point(aes(shape=Treatment), size = 2, position = position_dodge(width = 0.05)) + #include points in the shape of the treatments
  annotate("text", x=as.Date("2018-07-15"), y=7.95, label = "No Data") + #add text to the gratempic where data are missing
  annotate("text", x=as.Date("2018-08-15"), y=7.95, label = "No Data") + #add text to the gratempic where data are missing
  xlab("Time") + #label x axis
  ylab("temp Total Scale") + # label y axis
  ylim(10,20) + # set y axis scale
  geom_vline(xintercept = 16899, linetype="dotted", color = "gray", size=1) + #add vertical line
  geom_vline(xintercept = 16928, linetype="dashed", color = "gray", size=1) + #add vertical line
  geom_vline(xintercept = 17013, linetype="dotted", color = "gray", size=1) + #add vertical line
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position=c(.65, .25), #set legend location
        legend.text = element_text(size = 8), #set the legend text size
        legend.key = element_blank(), #remove the legend background
        legend.title = element_text(size=8, face="bold")) + #set legend title attributes
  ggtitle("B) Experimental temp") + #add a main title
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0)) #set title attributes
All.temp #view plot

#---------------------------------------------------------------------------------------------------
# HEATH TRAY Flow rate data 
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
flow_EXP1.TRMT.summary # view table

# EXP2 summary -----------------------
# grouped as 2 treatments (just Sec.treat) during reciprocal exposure
# NOTE: this shows flow difference influenced by conical overflow to heath tray pairs
flow_EXP2.TRMT.2<- do.call(data.frame,aggregate(LPM ~ Sec.treat *Day, 
                                                data = flow_EXP2, 
                                                function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by treatment and Day (4 tanks ber treatment)
flow_EXP2.TRMT.2.summary <- summarySE(flow_EXP2.TRMT.2, measurevar="LPM.mean", groupvars=c("Sec.treat")) # summary by treatment
flow_EXP2.TRMT.2.summary # view table
# grouped as 4 treatments during reciprocal exposure
# NOTE: this shows flow difference between treaments, but not influenced by the same conical overflow
flow_EXP2.TRMT.4<- do.call(data.frame,aggregate(LPM ~ Treatment_1_2*Day, 
                                                data = flow_EXP2, 
                                                function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by treatment and Day (4 tanks ber treatment)
flow_EXP2.TRMT.4.summary <- summarySE(flow_EXP2.TRMT.4, measurevar="LPM.mean", groupvars=c("Treatment_1_2")) # summary by treatment
flow_EXP2.TRMT.4.summary # view table

# EXP1 AND EXP2 summary -----------------------
# group by just day for daily flow rates
flow_ALL.Date<- do.call(data.frame,aggregate(LPM ~ Date, 
                                             data = flow_EXP1_2, 
                                             function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by Date
flow_ALL.Date.summary <- summarySE(flow_ALL.Date, measurevar="LPM.mean") # summary by treatment
flow_ALL.Date.summary # view table
# na omited when notes read "adjusted"
flow_EXP1_2.OMIT <- subset(flow_EXP1_2, Notes!="adjusted") # ommit rows when flow rate was adjusted
flow_ALL.Date.OMIT<- do.call(data.frame,aggregate(LPM ~ Date, 
                                                  data = flow_EXP1_2.OMIT, 
                                                  function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by Date, adjusted ommited
flow_ALL.Date.summary.OMIT <- summarySE(flow_ALL.Date.OMIT, measurevar="LPM.mean") # summary by treatment
flow_ALL.Date.summary.OMIT # view table

# group by treatment and day day for daily flow rates
flow_ALL.TRMT<- do.call(data.frame,aggregate(LPM ~ Date*Treatment, 
                                             data = flow_EXP1_2, 
                                             function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by Date
flow_ALL.TRMT.summary <- summarySE(flow_ALL.TRMT, measurevar="LPM.mean", groupvars=c("Treatment")) # summary by treatment
flow_ALL.TRMT.summary # view table
# na omited when notes read "adjusted"
flow_ALL.TRMT.OMIT<- do.call(data.frame,aggregate(LPM ~ Date*Treatment, 
                                                  data = flow_EXP1_2.OMIT, 
                                                  function(x) c(mean = mean(x), sd = sd(x)))) # mean and stdev by Date, adjusted ommited
flow_ALL.TRMT.summary.OMIT <- summarySE(flow_ALL.TRMT.OMIT, measurevar="LPM.mean", groupvars=c("Treatment")) # summary by treatment
flow_ALL.TRMT.summary.OMIT # view table

#---------------------------------------------------------------------------------------------------
# HEATH TRAY Discrete Seawater Chemistry Tables 
########################################################################################################################

#pH Tris Calibration Curves
#Data to calculate conversion equations from mV to total scale using tris standard for pH probe
path.tris <-("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/pH_Calibration_Files") #set path to calibration file folder
file.names.tris<-list.files(path = path.tris, pattern = "csv$") #list all the file names with csv 
pH.cals <- data.frame(matrix(NA, nrow=length(file.names.tris), ncol=4, dimnames=list(file.names.tris,c("Date", "Intercept", "Slope","R2")))) #generate an empty 3 column dataframe with specific column names

for(i in 1:length(file.names.tris)) { # for every file in list start at the first and run this following function
  Calib.Data <-read.table(file.path(path.tris,file.names.tris[i]), header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
  model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
  coe <- coef(model) #extracts the coeffecients
  R <- summary(model)$r.squared #extracts the R2
  pH.cals[i,2:3] <- coe #inserts coef in the dataframe
  pH.cals[i,4] <- R #inserts R2 in the dataframe
  pH.cals[i,1] <- substr(file.names.tris[i],1,8) #stores the file name in the Date column
}

colnames(pH.cals) <- c("Calib.Date",  "Intercept",  "Slope", "R2") #names the columns of the dataframe
pH.cals #view data


#call cumulative spreadsheet of discrete seawater chemistry
chem<-read.csv("Output/Seawater_chemistry_table_Output_All.csv", header=T, sep=",", na.string="NA", as.is=T) 
chem # view the file
chem.exp <-subset(chem, Treatment!="na") #remove na - na often set as the treatment for samples of the sump

chem.exp1 <-chem.exp[52:131,] # exposure 1 without Day 0 (20180715 - 20180724)
chem.exp1$Exposure <- "Exp1"
chem.exp1$tank.name <- substr(chem.exp1$Tank, start = 10, stop = 13) # new column for tank name without date

chem.exp2 <- chem.exp[157:204,] # exposure 2 without Day 0 (20180808 - 20180813)
chem.exp2$Exposure <- "Exp2"
chem.exp2$tank.name <- substr(chem.exp2$Tank, start = 10, stop = 13) # new column for tank name without date

chem.exp_1_2 <- rbind(chem.exp1,chem.exp2) # exposure 1 and 2

# melt - converts wide table to long
chem.long <- melt(chem.exp_1_2, id.vars=c("Date", "Tank", "Treatment", "tank.name", "Exposure")) # uses tidyr to make a long table from wide

Exp1.chem.long <-subset(chem.long, Exposure == "Exp1") #separate out exposure 1 for all data
Exp2.chem.long <-subset(chem.long, Exposure == "Exp2") #separate out exposure 2 for all data

#Test for tank and treatment differences in Temperature and Total Alkalinity in Exposure 1
Exp1.Temp <-subset(Exp1.chem.long, variable=="Temperature") #separate out exposure 1 for all data
temp1.tank <- aov(value~tank.name, data=Exp1.Temp) #test the hypothesis there is no difference in temperature between tanks
temp1.tank.res <-anova(temp1.tank) #view results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp1.tank$residuals) #plot histogram of residuals
boxplot(temp1.tank$residuals) #plot boxplot of residuals
plot(temp1.tank) #display residuals versus fitter, normal QQ plot, leverage plot

temp1.trt <- aov(value ~Treatment, data=Exp1.Temp) #test the hypothesis there is no difference in temperature between treatments
temp1.trt.res <- anova(temp1.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp1.trt$residuals) #plot histogram of residuals
boxplot(temp1.trt$residuals) #plot boxplot of residuals
plot(temp1.trt) #display residuals versus fitter, normal QQ plot, leverage plot

Exp1.TA <-subset(Exp1.chem.long, variable=="TA") #separate out exposure 1 for all data
TA1.tank <- aov(value ~tank.name, data=Exp1.TA) #test the hypothesis there is no difference in total alkalinity between tanks
TA1.tank.res <- anova(temp1.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA1.tank$residuals) #plot histogram of residuals
boxplot(TA1.tank$residuals) #plot boxplot of residuals
plot(TA1.tank) #display residuals versus fitter, normal QQ plot, leverage plot

TA1.trt <- aov(value ~Treatment, data=Exp1.TA) #test the hypothesis there is no difference in total alkalinity between treatments
TA1.trt.res <- anova(temp1.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA1.trt$residuals) #plot histogram of residuals
boxplot(TA1.trt$residuals) #plot boxplot of residuals
plot(TA1.trt) #display residuals versus fitter, normal QQ plot, leverage plot

#Test for tank and treatment differences in Temperature and Total Alkalinity in Exposure 2
Exp2.Temp <-subset(Exp2.chem.long, variable=="Temperature") #separate out exposure 1 for all data
temp2.tank <- aov(value~tank.name, data=Exp2.Temp) #test the hypothesis there is no difference in temperature between tanks
temp2.tank.res <-anova(temp2.tank) #view results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp2.tank$residuals) #plot histogram of residuals
boxplot(temp2.tank$residuals) #plot boxplot of residuals
plot(temp2.tank) #display residuals versus fitter, normal QQ plot, leverage plot

temp2.trt <- aov(value ~Treatment, data=Exp2.Temp) #test the hypothesis there is no difference in temperature between treatments
temp2.trt.res <- anova(temp2.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp2.trt$residuals) #plot histogram of residuals
boxplot(temp2.trt$residuals) #plot boxplot of residuals
plot(temp2.trt) #display residuals versus fitter, normal QQ plot, leverage plot

Exp2.TA <-subset(Exp2.chem.long, variable=="TA") #separate out exposure 1 for all data
TA2.tank <- aov(value ~tank.name, data=Exp2.TA) #test the hypothesis there is no difference in total alkalinity between tanks
TA2.tank.res <- anova(temp2.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA2.tank$residuals) #plot histogram of residuals
boxplot(TA2.tank$residuals) #plot boxplot of residuals
plot(TA2.tank) #display residuals versus fitter, normal QQ plot, leverage plot

TA2.trt <- aov(value ~Treatment, data=Exp2.TA) #test the hypothesis there is no difference in total alkalinity between treatments
TA2.trt.res <- anova(temp2.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA2.trt$residuals) #plot histogram of residuals
boxplot(TA2.trt$residuals) #plot boxplot of residuals
plot(TA2.trt) #display residuals versus fitter, normal QQ plot, leverage plot

#Calculate descriptive stats by Tank
SWC.Tanks <- ddply(chem.long, c("Exposure", "tank.name", "variable"), summarise, #apply functions to sewater chem data
                   N = length(na.omit(value)), #count the sample size removing NA
                   mean = mean(value), #calculate average 
                   sem = sd(value)/sqrt(n)) #calcualte the standard error of the mean

#Calculate descriptive stats by Treatment
SWC.Treatments <- ddply(chem.long, c("Exposure", "Treatment", "variable"), summarise,
                        N = length(na.omit(value)), #count the sample size removing NA
                        mean = mean(value), #calculate average 
                        sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

Exposure1.chem <-subset(SWC.Treatments, Exposure == "Exp1") #separate out exposure 1
Exposure2.chem <-subset(SWC.Treatments, Exposure == "Exp2") #separate out exposure 2
Exposure1.long <- reshape(Exposure1.chem, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
Exposure2.long <- reshape(Exposure2.chem, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout

write.table (Exposure1.long, file="C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Output/Seawater_chemistry_table_Output_exposure1.csv", sep=",", row.names = FALSE) #save data to output file
write.table (Exposure2.long, file="C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Output/Seawater_chemistry_table_Output_exposure2.csv", sep=",", row.names = FALSE) #save data to output file

### Overall temperature and salinity ###
salinity.overall<- do.call(data.frame,aggregate(Salinity ~ Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x)))) # table of means __ per day by treatment (4 tanks per treatment)
salinity.summary.overall <- summarySE(salinity.overall, measurevar="Salinity.mean")  # overall mean 

Temperature.overall<- do.call(data.frame,aggregate(Temperature ~ Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x)))) 
Temperature.summary.overall <- summarySE(Temperature.overall, measurevar="Temperature.mean") # overall mean 

### EXP1 ### 10-day OA exposure

salinity.1<- do.call(data.frame,aggregate(Salinity ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x)))) # table of means __ per day by treatment (4 tanks per treatment)
salinity.summary.1 <- summarySE(salinity.1, measurevar="Salinity.mean", groupvars=c("Treatment"))  # overall mean for each treatment 

Temperature.1<- do.call(data.frame,aggregate(Temperature ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x)))) 
Temperature.summary.1 <- summarySE(Temperature.1, measurevar="Temperature.mean", groupvars=c("Treatment"))

pH.1<- do.call(data.frame,aggregate(pH ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
pH.summary.1 <- summarySE(pH.1, measurevar="pH.mean", groupvars=c("Treatment"))

CO2.1<- do.call(data.frame,aggregate(CO2 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
CO2.summary.1 <- summarySE(CO2.1, measurevar="CO2.mean", groupvars=c("Treatment"))

pCO2.1<- do.call(data.frame,aggregate(pCO2 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
pCO2.summary.1 <- summarySE(pCO2.1, measurevar="pCO2.mean", groupvars=c("Treatment"))

HCO3.1<- do.call(data.frame,aggregate(HCO3 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
HCO3.summary.1 <- summarySE(HCO3.1, measurevar="HCO3.mean", groupvars=c("Treatment"))

CO3.1<- do.call(data.frame,aggregate(CO3 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
CO3.summary.1 <- summarySE(CO3.1, measurevar="CO3.mean", groupvars=c("Treatment"))

DIC.1<- do.call(data.frame,aggregate(DIC ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
DIC.summary.1 <- summarySE(DIC.1, measurevar="DIC.mean", groupvars=c("Treatment"))

TA.1<- do.call(data.frame,aggregate(TA ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
TA.summary.1 <- summarySE(TA.1, measurevar="TA.mean", groupvars=c("Treatment"))

Aragonite.Sat.1<- do.call(data.frame,aggregate(Aragonite.Sat ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), sd = sd(x))))
Aragonite.Sat.summary.1 <- summarySE(Aragonite.Sat.1, measurevar="Aragonite.Sat.mean", groupvars=c("Treatment"))

#Final table EXPOSURE 1 - 10-day
########################################################################################################################

EXP1_FinalTable <- as.data.frame(matrix(nrow = 2))  # make a data frame with 2 rows   

EXP1_FinalTable$Treatment <- salinity.summary.1$Treatment # column for treatments

EXP1_FinalTable$Days <- signif(salinity.summary.1$N, digits = 2) # column for number of dayaa as N

Flow.1 <- signif(flow_EXP1.TRMT.summary$LPM.mean, digits = 3) #significant figs for the mean of each value
Flow.sd.1 <- signif(flow_EXP1.TRMT.summary$sd, digits = 2)#significant figs for the standard dev of each value
EXP1_FinalTable$Flow <- paste(Flow.1, Flow.sd.1, sep="±") # combine mean ± st error as column in final table

Temperature.1 <- signif(Temperature.summary.1$Temperature.mean, digits = 4) #significant figs for the mean of each value
Temp.sd.1 <- signif(Temperature.summary.1$sd, digits = 2)#significant figs for the standard dev of each value
EXP1_FinalTable$Temperature <-paste(Temperature.1, Temp.sd.1, sep="±") # combine mean ± st error as column in final table

Salinity.1 <- signif(salinity.summary.1$Salinity.mean, digits = 4)
Sal.sd.1 <- signif(salinity.summary.1$sd, digits = 2)
EXP1_FinalTable$Salinity <-paste(Salinity.1, Sal.sd.1, sep="±")

pH.1 <- signif(pH.summary.1$pH.mean, digits = 3)
pH.sd.1 <- signif(pH.summary.1$sd, digits = 1)
EXP1_FinalTable$pH <-paste(pH.1, pH.sd.1, sep="±")

CO2.1 <- signif(CO2.summary.1$CO2.mean, digits = 4)
CO2.sd.1 <- signif(CO2.summary.1$sd, digits = 3)
EXP1_FinalTable$CO2 <-paste(CO2.1, CO2.sd.1, sep="±")

pCO2.1 <- signif(pCO2.summary.1$pCO2.mean, digits = 5)
pCO2.sd.1 <- signif(pCO2.summary.1$sd, digits = 4)
EXP1_FinalTable$pCO2 <-paste(pCO2.1, pCO2.sd.1, sep="±")

HCO3.1 <- signif(HCO3.summary.1$HCO3.mean, digits = 6)
HCO3.sd.1 <- signif(HCO3.summary.1$sd, digits = 3)
EXP1_FinalTable$HCO3 <-paste(HCO3.1, HCO3.sd.1, sep="±")

CO3.1 <- signif(CO3.summary.1$CO3.mean, digits = 4)
CO3.sd.1 <- signif(CO3.summary.1$sd, digits = 3)
EXP1_FinalTable$CO3 <-paste(CO3.1, CO3.sd.1, sep="±")

DIC.1 <- signif(DIC.summary.1$DIC.mean, digits = 4)
DIC.sd.1 <- signif(DIC.summary.1$sd, digits = 3)
EXP1_FinalTable$DIC <-paste(DIC.1, DIC.sd.1, sep="±")

TA.1 <- signif(TA.summary.1$TA.mean, digits = 4)
TA.sd.1 <- signif(TA.summary.1$sd, digits = 3)
EXP1_FinalTable$Total.Alkalinity <-paste(TA.1, TA.sd.1 , sep="±")

Aragonite.Sat.1 <- signif(Aragonite.Sat.summary.1$Aragonite.Sat.mean, digits = 2)
Arag.sd.1 <- signif(Aragonite.Sat.summary.1$sd, digits = 2)
EXP1_FinalTable$Aragonite.Saturation <-paste(Aragonite.Sat.1, Arag.sd.1, sep="±")

EXP1_FinalTable <- subset(EXP1_FinalTable, select = -c(V1)) # ommit the first column
head(EXP1_FinalTable) # view final table

### EXP2 ### 6-day OA exposure
########################################################################################################################

salinity.2<- do.call(data.frame,aggregate(Salinity ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
salinity.summary.2 <- summarySE(salinity.2, measurevar="Salinity.mean", groupvars=c("Treatment"))

Temperature.2<- do.call(data.frame,aggregate(Temperature ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
Temperature.summary.2 <- summarySE(Temperature.2, measurevar="Temperature.mean", groupvars=c("Treatment"))

pH.2<- do.call(data.frame,aggregate(pH ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
pH.summary.2 <- summarySE(pH.2, measurevar="pH.mean", groupvars=c("Treatment"))

CO2.2<- do.call(data.frame,aggregate(CO2 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
CO2.summary.2 <- summarySE(CO2.2, measurevar="CO2.mean", groupvars=c("Treatment"))

pCO2.2<- do.call(data.frame,aggregate(pCO2 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
pCO2.summary.2 <- summarySE(pCO2.2, measurevar="pCO2.mean", groupvars=c("Treatment"))

HCO3.2<- do.call(data.frame,aggregate(HCO3 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
HCO3.summary.2 <- summarySE(HCO3.2, measurevar="HCO3.mean", groupvars=c("Treatment"))

CO3.2<- do.call(data.frame,aggregate(CO3 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
CO3.summary.2 <- summarySE(CO3.2, measurevar="CO3.mean", groupvars=c("Treatment"))

DIC.2<- do.call(data.frame,aggregate(DIC ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
DIC.summary.2 <- summarySE(DIC.2, measurevar="DIC.mean", groupvars=c("Treatment"))

TA.2<- do.call(data.frame,aggregate(TA ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
TA.summary.2 <- summarySE(TA.2, measurevar="TA.mean", groupvars=c("Treatment"))

Aragonite.Sat.2<- do.call(data.frame,aggregate(Aragonite.Sat ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), sd = sd(x))))
Aragonite.Sat.summary.2 <- summarySE(Aragonite.Sat.2, measurevar="Aragonite.Sat.mean", groupvars=c("Treatment"))

#Final table EXPOSURE 2 - 6-day
########################################################################################################################

EXP2_FinalTable <- as.data.frame(matrix(nrow = 2)) 

EXP2_FinalTable$Treatment <- salinity.summary.2$Treatment

EXP2_FinalTable$Days <- signif(salinity.summary.2$N, digits = 2)

Flow.2 <- signif(flow_EXP2.TRMT.2.summary$LPM.mean, digits = 3) #significant figs for the mean of each value
Flow.sd.2 <- signif(flow_EXP2.TRMT.2.summary$sd, digits = 2)#significant figs for the standard dev of each value
EXP2_FinalTable$Flow <- paste(Flow.2, Flow.sd.2, sep="±") # combine mean ± st error as column in final table

Temperature.2 <- signif(Temperature.summary.2$Temperature.mean, digits = 4)
Temp.sd.2 <- signif(Temperature.summary.2$sd, digits = 2)
EXP2_FinalTable$Temperature <-paste(Temperature.2, Temp.sd.2, sep="±")

Salinity.2 <- signif(salinity.summary.2$Salinity.mean, digits = 4)
Sal.sd.2 <- signif(salinity.summary.2$sd, digits = 2)
EXP2_FinalTable$Salinity <-paste(Salinity.2, Sal.sd.2, sep="±")

pH.2 <- signif(pH.summary.2$pH.mean, digits = 3)
pH.sd.2 <- signif(pH.summary.2$sd, digits = 1)
EXP2_FinalTable$pH <-paste(pH.2, pH.sd.2, sep="±")

CO2.2 <- signif(CO2.summary.2$CO2.mean, digits = 4)
CO2.sd.2 <- signif(CO2.summary.2$sd, digits = 3)
EXP2_FinalTable$CO2 <-paste(CO2.2, CO2.sd.2, sep="±")

pCO2.2 <- signif(pCO2.summary.2$pCO2.mean, digits = 5)
pCO2.sd.2 <- signif(pCO2.summary.2$sd, digits = 4)
EXP2_FinalTable$pCO2 <-paste(pCO2.2, pCO2.sd.2, sep="±")

HCO3.2 <- signif(HCO3.summary.2$HCO3.mean, digits = 6)
HCO3.sd.2 <- signif(HCO3.summary.2$sd, digits = 3)
EXP2_FinalTable$HCO3 <-paste(HCO3.2, HCO3.sd.2, sep="±")

CO3.2 <- signif(CO3.summary.2$CO3.mean, digits = 4)
CO3.sd.2 <- signif(CO3.summary.2$sd, digits = 3)
EXP2_FinalTable$CO3 <-paste(CO3.2, CO3.sd.2, sep="±")

DIC.2 <- signif(DIC.summary.2$DIC.mean, digits = 4)
DIC.sd.2 <- signif(DIC.summary.2$sd, digits = 3)
EXP2_FinalTable$DIC <-paste(DIC.2, DIC.sd.2, sep="±")

TA.2 <- signif(TA.summary.2$TA.mean, digits = 4)
TA.sd.2 <- signif(TA.summary.2$sd, digits = 3)
EXP2_FinalTable$Total.Alkalinity <-paste(TA.2, TA.sd.2 , sep="±")

Aragonite.Sat.2 <- signif(Aragonite.Sat.summary.2$Aragonite.Sat.mean, digits = 2)
Arag.sd.2 <- signif(Aragonite.Sat.summary.2$sd, digits = 2)
EXP2_FinalTable$Aragonite.Saturation <-paste(Aragonite.Sat.2, Arag.sd.2, sep="±")

EXP2_FinalTable <- subset(EXP2_FinalTable, select = -c(V1)) # ommit the first column
head(EXP2_FinalTable) # view final table

### EXP_1_2 ### ALL OA exposure
########################################################################################################################

salinity.all<- do.call(data.frame,aggregate(Salinity ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
salinity.summary.all <- summarySE(salinity.all, measurevar="Salinity.mean", groupvars=c("Treatment"))

Temperature.all<- do.call(data.frame,aggregate(Temperature ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
Temperature.summary.all <- summarySE(Temperature.all, measurevar="Temperature.mean", groupvars=c("Treatment"))

pH.all<- do.call(data.frame,aggregate(pH ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
pH.summary.all <- summarySE(pH.all, measurevar="pH.mean", groupvars=c("Treatment"))

CO2.all<- do.call(data.frame,aggregate(CO2 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
CO2.summary.all <- summarySE(CO2.all, measurevar="CO2.mean", groupvars=c("Treatment"))

pCO2.all<- do.call(data.frame,aggregate(pCO2 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
pCO2.summary.all <- summarySE(pCO2.all, measurevar="pCO2.mean", groupvars=c("Treatment"))

HCO3.all<- do.call(data.frame,aggregate(HCO3 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
HCO3.summary.all <- summarySE(HCO3.all, measurevar="HCO3.mean", groupvars=c("Treatment"))

CO3.all<- do.call(data.frame,aggregate(CO3 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
CO3.summary.all <- summarySE(CO3.all, measurevar="CO3.mean", groupvars=c("Treatment"))

DIC.all<- do.call(data.frame,aggregate(DIC ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
DIC.summary.all <- summarySE(DIC.all, measurevar="DIC.mean", groupvars=c("Treatment"))

TA.all<- do.call(data.frame,aggregate(TA ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
TA.summary.all <- summarySE(TA.all, measurevar="TA.mean", groupvars=c("Treatment"))

Aragonite.Sat.all<- do.call(data.frame,aggregate(Aragonite.Sat ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), sd = sd(x))))
Aragonite.Sat.summary.all <- summarySE(Aragonite.Sat.all, measurevar="Aragonite.Sat.mean", groupvars=c("Treatment"))

#Final table EXPOSURE ALL OA Exposure
########################################################################################################################

EXP_1_2_FinalTable <- as.data.frame(matrix(nrow = 2)) 

EXP_1_2_FinalTable$Treatment <- salinity.summary.all$Treatment

EXP_1_2_FinalTable$Days <- signif(salinity.summary.all$N, digits = 2)

Flow._1_2 <- signif(flow_ALL.TRMT.summary$LPM.mean, digits = 3) #significant figs for the mean of each value
Flow.sd._1_2 <- signif(flow_ALL.TRMT.summary$sd, digits = 2)#significant figs for the standard dev of each value
EXP_1_2_FinalTable$Flow <- paste(Flow._1_2, Flow.sd._1_2, sep="±") # combine mean ± st error as column in final table

Temperature.all <- signif(Temperature.summary.all$Temperature.mean, digits = 4)
Temp.sd.all <- signif(Temperature.summary.all$sd, digits = 2)
EXP_1_2_FinalTable$Temperature <-paste(Temperature.all, Temp.sd.all, sep="±")

Salinity.all <- signif(salinity.summary.all$Salinity.mean, digits = 4)
Sal.sd.all <- signif(salinity.summary.all$sd, digits = 2)
EXP_1_2_FinalTable$Salinity <-paste(Salinity.all, Sal.sd.all, sep="±")

pH.all <- signif(pH.summary.all$pH.mean, digits = 3)
pH.sd.all <- signif(pH.summary.all$sd, digits = 1)
EXP_1_2_FinalTable$pH <-paste(pH.all, pH.sd.all, sep="±")

CO2.all <- signif(CO2.summary.all$CO2.mean, digits = 4)
CO2.sd.all <- signif(CO2.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$CO2 <-paste(CO2.all, CO2.sd.all, sep="±")

pCO2.all <- signif(pCO2.summary.all$pCO2.mean, digits = 5)
pCO2.sd.all <- signif(pCO2.summary.all$sd, digits = 4)
EXP_1_2_FinalTable$pCO2 <-paste(pCO2.all, pCO2.sd.all, sep="±")

HCO3.all <- signif(HCO3.summary.all$HCO3.mean, digits = 6)
HCO3.sd.all <- signif(HCO3.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$HCO3 <-paste(HCO3.all, HCO3.sd.all, sep="±")

CO3.all <- signif(CO3.summary.all$CO3.mean, digits = 4)
CO3.sd.all <- signif(CO3.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$CO3 <-paste(CO3.all, CO3.sd.all, sep="±")

DIC.all <- signif(DIC.summary.all$DIC.mean, digits = 4)
DIC.sd.all <- signif(DIC.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$DIC <-paste(DIC.all, DIC.sd.all, sep="±")

TA.all <- signif(TA.summary.all$TA.mean, digits = 4)
TA.sd.all <- signif(TA.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$Total.Alkalinity <-paste(TA.all, TA.sd.all , sep="±")

Aragonite.Sat.all <- signif(Aragonite.Sat.summary.all$Aragonite.Sat.mean, digits = 2)
Arag.sd.all <- signif(Aragonite.Sat.summary.all$sd, digits = 2)
EXP_1_2_FinalTable$Aragonite.Saturation <-paste(Aragonite.Sat.all, Arag.sd.all, sep="±")

EXP_1_2_FinalTable <- subset(EXP_1_2_FinalTable, select = -c(V1)) # ommit the first column
head(EXP_1_2_FinalTable) # view final table

# export all tables
cro(EXP_1_2_FinalTable)
png("Output/ALL_chem_table.png", height = 30*nrow(EXP_1_2_FinalTable), width = 120*ncol(EXP_1_2_FinalTable))
all_chem<-tableGrob(EXP_1_2_FinalTable)
grid.arrange(all_chem)
dev.off()

png("Output/EXP1_chem_table.png", height = 30*nrow(EXP1_FinalTable), width = 120*ncol(EXP1_FinalTable))
EXP1_chem<-tableGrob(EXP1_FinalTable)
grid.arrange(EXP1_chem)
dev.off()

png("Output/EXP2_chem_table.png", height = 30*nrow(EXP2_FinalTable), width = 120*ncol(EXP2_FinalTable))
EXP2_chem<-tableGrob(EXP2_FinalTable)
grid.arrange(EXP2_chem)
dev.off()


#---------------------------------------------------------------------------------------------------
# Respiration Data - Analysis, Graphs, Models  (summarized analysis from Stats_resp_analysis.R)
###################################################################################################

#Load BASAL Respiraiton Data before the exposures
respBASAL<-read.csv("Data/Basal_resp_calc_and_standardized.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(respBASAL) # view the names of the data

# plot the basal respiration rate 
resp_BASAL_plot <- ggplot(respBASAL, aes(x = factor(respBASAL$Date), y = respBASAL$LpcResp_alpha0.4_all)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(size = 2, shape = 21) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_BASAL_plot + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                            x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \ Basal"))


#Load Respiraiton Data
resp<-read.csv("Data/All_resp_calc_and_standardized.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(resp) # view the names of the data
# seperate into experiment 1 and 2 for the 10-day and 6-day OA exposure
resp_EXP1 <- subset(resp, EXP.numb=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
resp_EXP2 <- subset(resp, EXP.numb=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
resp_EXP2.0 <- subset(resp, EXP.numb=="EXP2") # same under a diff name (read why on next comment)
resp_EXP2.0 <-subset(resp_EXP2.0, Day!=0) #eliminate Day0 of the second exposure for graph on cumulative exposure to OA 


#### EXP1 ####

# Test automated LoLin Script and the Reference data
# Reference = each output observed at default critieria (alpha = 0.2, untruncated) and adjusted for acheive peak density of regressions
# Automated ouputs = at nine total settings of alpha= 0.2,0.4and0.6 at three different truncations
# Below used alpha = 0.4, no truncation for automated ouput because of the Least number of respiration values outside of Loess CI with Reference
plot(resp_EXP1[,1],resp_EXP1[,5], main= "Ref vs. alpha0.4_all") 
summary(lm(resp_EXP1[,1]~resp_EXP1[,5])) #Adjusted R-squared:  0.3905
# ggplot labeled with row numbers for individual respiration points - can can values outside of CI interval in loess curve
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) # outside of loess curve = 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94

# regress points outside loess against reference with all automated resp outputs 
newdata_resp_EXP1_ALL <- data.frame(resp_EXP1$Date, resp_EXP1[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP1_ALL_1 <-newdata_resp_EXP1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ] # only call points (rows) outside loess in first regression
plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,10]) # alpha = 0.6 truncation at 10-20 minutes (each resp trial was 30 mins)
summary(lm(newdata_resp_EXP1_ALL_1[,10]~newdata_resp_EXP1_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7235 

#create a new column 
resp_EXP1$FINALresp <- resp_EXP1$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation
resp_EXP1$FINALresp[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] <- resp_EXP1$LpcResp_alpha0.6_min10.20.csv[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] #inserted new rows from alpha = 0.6 10-20 min for points outside loess curve
# test correlation of this new column with the Ref
plot(resp_EXP1$Resp_individually_all.csv,resp_EXP1$FINALresp, main= "Ref vs. FINALresp") # plot the relationship
summary(lm(resp_EXP1[,1]~resp_EXP1$FINALresp)) # summarize linear model - Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# Now that we have Finalresp, overall mean SD SMR in EXP1
exp1_resp_all <- do.call(data.frame,aggregate(FINALresp ~ Treat1_Treat2*Date, data = resp_EXP1, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by date EXP1
exp1_resp_summary_all <- summarySE(exp1_resp_all, measurevar="FINALresp.mean", groupvars=c("Treat1_Treat2")) # SMR by treatment EXP1
exp1_resp_overall <- summarySE(exp1_resp_all, measurevar="FINALresp.mean") # overall SMR EXP1

resp_EXP2.0.T<- merge(resp_EXP1, resp_EXP2, by=c("tank"))  #merge to obtained combined treatments from EXP2
exp1_resp_4.T <- do.call(data.frame,aggregate(FINALresp ~ Treat1_Treat2.y*Date.x, data = resp_EXP2.0.T, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by date EXP1 and 4 treatments in EXP2
exp1_resp_summary.T <- summarySE(exp1_resp_4.T, measurevar="FINALresp.mean", groupvars=c(" Treat1_Treat2.y")) # SMR by four treatments EXP1xEXP2

# first visualize the data
#plot
Exp1.Fig.resp <- ggboxplot(resp_EXP1, x = "Day", y = "FINALresp", color = "Init.treat", ylab= "respiration",
                           palette = c("blue", "red"), main= "Initial Exposure")
Exp1.Fig.resp # view boxplot of all data

#table
x1 <- do.call(data.frame,aggregate(FINALresp ~ Day*Init.treat, data = resp_EXP1, function(x) c(mean = mean(x), se = std.error(x)))) #mean and st. error table
# mixed effect model
Init.lme.resp <- lmer(FINALresp ~ Init.treat*Day + (1|Day), data = resp_EXP1) #reteatment and day with day as a random factor
anova(Init.lme.resp) # anova of lmer
summary(Init.lme.resp) # summary of lmer
m1.resp <- lme(FINALresp~Init.treat*Day,random=~1|Day,data=resp_EXP1) # equivent function in lme
anova(m1.resp) # anova on lme model --------an effect of treatment on inital exposure to OA
EXP1.lme.anovatable <- anova(m1.resp) # assign name to output table late in code
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m1.resp)) #plot histogram of residuals
shapiro.test(residuals(m1.resp)) # residuals are normal
boxplot(residuals(m1.resp)) #plot boxplot of residuals
plot( fitted(m1.resp),residuals(m1.resp)) #display residuals versus fitter, normal QQ plot, leverage plot

sumresp_EXP1 <- summarySE(resp_EXP1, 
                          measurevar="FINALresp", 
                          groupvars=c("Date","Treat1_Treat2")) # summary table of resp with Date and treatment
sumresp_EXP1_means <- summarySE(sumresp_EXP1, 
                                measurevar="FINALresp", 
                                groupvars=c("Treat1_Treat2")) # summarize previous table for overall treatment 
percentdiff <- ((sumresp_EXP1_means[1,3] - sumresp_EXP1_means[2,3])/sumresp_EXP1_means[1,3])*100 # calculate percent difference
percentdiff # 25% lower respiration rates in low pH


#plot treatments and time WITH THE BASAL ADDED
colnames(respBASAL)[1] <- "FINALresp" # rename the calc resp values in Basal table to match the resp_EXP1
respBASAL$Treat1_Treat2 <- c("Ambient") # name the treatement 
respBASAL$Day <- 0
resp_EXP1_condensed <- resp_EXP1[,c(11,12,13,14,15,16,21)] # call the columns in respBasal to rbind
resp_EXP1_ALL <- rbind(resp_EXP1_condensed, respBASAL) # merge the two tables for the graph

resp_EXP1_plot <- ggplot(resp_EXP1_ALL, aes(x = factor(resp_EXP1_ALL$Day), y = resp_EXP1_ALL$FINALresp, fill = resp_EXP1_ALL$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  ylim(0, 0.6) +
  geom_point(aes(fill = resp_EXP1_ALL$Treat1_Treat2), size = 1.5, shape = 21, position = position_jitterdodge(0.15)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, position = position_jitterdodge(0.125)) +
theme(legend.position = c(.9, .9), legend.text=element_text(size=8)) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
  labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                            x = "Date", fill="") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments resp_EXP1")

# plot just treatment 
resp_EXP1_plot_2 <- ggplot(resp_EXP1, aes(x = factor(resp_EXP1$Treat1_Treat2), y = resp_EXP1$FINALresp, fill = resp_EXP1$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  geom_point(aes(fill = resp_EXP1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(legend.position = c(.9, .9), legend.text=element_text(size=8)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4, position = position_jitterdodge(0.125)) +
  labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                              x = "Treatment", fill= "") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments resp_EXP1")
resp_EXP1_plot_2

#### EXP2 ####

# Test automated LoLin Script and the Reference data
# Reference = each output observed at default critieria (alpha = 0.2, untruncated) and adjusted for acheive peak density of regressions
# Automated ouputs = at nine total settings of alpha= 0.2,0.4and0.6 at three different truncations
# Below used alpha = 0.4, no truncation for automated ouput because of the Least number of respiration values outside of Loess CI with Reference
plot(resp_EXP2[,1],resp_EXP2[,5], main= "Ref vs. alpha0.4_all")
summary(lm(resp_EXP2[,1]~resp_EXP2[,5])) #Adjusted R-squared:  0.8002 
# ggplot labeled with row numbers for individual respiration points - can can values outside of CI interval in loess curve
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01)) # outside of loess curve = 114, 153,155,156,150,141,105,130,152,98,188
tail(resp_EXP1) # end of exposure 1 is row #96
numberID <- (c(114,153,155,156,150,141,105,130,152,98,188)) - 96 #subtract by 96 to get the actual row number in resp_exp2
# actuall row numbers outside loess cruve = 18 57 59 60 54 45  9 34 56  2 92
# regress points outside loess against reference with all automated resp outputs 
newdata_resp_EXP2_ALL <- data.frame(resp_EXP2$Date , resp_EXP2[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP2_ALL_1 <-newdata_resp_EXP2_ALL[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), ]  # only call points (rows) outside loess in first regression
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,11]) # alpha = 0.6 truncation at 10-25 minutes (each resp trial was 30 mins)
summary(lm(newdata_resp_EXP2_ALL_1[,11]~newdata_resp_EXP2_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7169 

#create a new column 
resp_EXP2$FINALresp <- resp_EXP2$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation
resp_EXP2$FINALresp[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92)] <- resp_EXP2$LpcResp_alpha0.6_min10.25.csv[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92)] #inserted new rows from alpha = 0.6 10-25 min for points outside loess curve
# test correlation of this new column with the Ref
plot(resp_EXP2$FINALresp,resp_EXP2$Resp_individually_all.csv, main= "Ref vs. FINALresp")
summary(lm(resp_EXP2$Resp_individually_all.csv~resp_EXP2$FINALresp)) # summarize linear model - Multiple R-squared:  0.9304,	Adjusted R-squared:  0.9297 
ggplot(resp_EXP2, aes(x = resp_EXP2$Resp_individually_all.csv, y = resp_EXP2$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# overall SMR in EXP2
exp2_resp_all <- do.call(data.frame,aggregate(FINALresp ~ Treat1_Treat2*Date, data = resp_EXP2, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by init*sec treatments date EXP2
exp2_resp_summary_all <- summarySE(exp2_resp_all, measurevar="FINALresp.mean", groupvars=c("Treat1_Treat2")) # SMR by init*sec  treatments EXP2
exp2_resp_all.2 <- do.call(data.frame,aggregate(FINALresp ~ Sec.treat*Date, data = resp_EXP2, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by sec treatments date EXP2
exp2_resp_summary_all.2 <- summarySE(exp2_resp_all.2, measurevar="FINALresp.mean", groupvars=c("Sec.treat")) # SMR by sec  treatments EXP2
exp2_resp_overall <- summarySE(exp2_resp_all, measurevar="FINALresp.mean") # overall SMR EXP2

library(wesanderson)
# first visualize the data
#plot
Exp2.Fig.resp <- ggboxplot(resp_EXP2, x = "Day", y = "FINALresp", color = "Sec.treat", ylab= "respiration",palette = c())
Exp2.Fig.resp # view the figure

#plot treatments and time
resp_EXP2_plot <- ggplot(resp_EXP2, aes(x = factor(resp_EXP2$Day), y = resp_EXP2$FINALresp, fill = resp_EXP2$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  ylim(0, 0.6) +
  geom_point(aes(fill = resp_EXP2$Treat1_Treat2), size = 1.5, shape = 21, position = position_jitterdodge(0.05)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, position = position_jitterdodge(0.05)) +
  theme(legend.position = c(.92, .9), legend.text=element_text(size=8)) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                              x = "Date", fill="") + 
          ggtitle("Juvenile geoduck respirometry \nin OA treatments resp_EXP1")

  
#table
x2.resp <- do.call(data.frame,aggregate(FINALresp ~ Day*Init.treat*Sec.treat, data = resp_EXP2_om, function(x) c(mean = mean(x), se = std.error(x))))
x2.1.resp <- do.call(data.frame,aggregate(FINALresp ~ Init.treat*Sec.treat, data = resp_EXP2_om, function(x) c(mean = mean(x), se = std.error(x))))
x2.resp$treatments <- paste(x2.resp$Init.treat, x2.resp$Sec.treat, sep="_") # combine treatments in a column

# linear mixed effect model
m2.resp <- lme(FINALresp~Init.treat*Sec.treat,random=~1|Day/Init.treat/Sec.treat,data=resp_EXP2) #reteatment and day with day as a random factor
anova(m2.resp) # anova of lmer
summary(m2.resp) # summary of lmer
EXP2.lme.anovatable <- anova(m2.resp) # assign name to output table late in code
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2.resp)) #plot histogram of residuals
shapiro.test(residuals(m2.resp)) # residuals are NOT normal
boxplot(residuals(m2.resp)) #plot boxplot of residuals
plot( fitted(m1.resp),residuals(m2.resp)) #display residuals versus fitter, normal QQ plot, leverage plot

#residuals are not normal - need to transform the data
shapiro.test(resp_EXP2$FINALresp) # Fianl_resp not normal - ommit the data from row 186
hist(resp_EXP2$FINALresp) # positive or right skewed 
OutVals2 = boxplot(resp_EXP2$FINALresp)$out # test for ouliers of respFinal
which(resp_EXP2$FINALresp %in% OutVals2) #  83 and 90 are outliers for new resp file
OutVals2.0 = boxplot(resp_EXP2$Resp_individually_all.csv) # test for outliers of the Reference
which(resp_EXP2$Resp_individually_all.csv %in% OutVals2.0) # 83 and 90 are outliers in "REF"
OutVals2.1 = boxplot(resp_EXP2$LpcResp_alpha0.4_all.csv)$out # test for outliers of the alpha 0.4 no truncation
which(resp_EXP2$LpcResp_alpha0.4_all.csv %in% OutVals2.1) # 90 is an outlier 

resp_EXP2_om <- resp_EXP2[-c(83,90),] # new dataset called resp_om
shapiro.test(resp_EXP2_om$FINALresp) # normally distributed 
hist(resp_EXP2_om$FINALresp) # noted that 83 and 90 ommitted gives a norm distribution, now test with mixed effect model and test residuals

# mixed effect model with resp_om
m2.resp <- lme(FINALresp~Init.treat*Sec.treat,random=~1|Day/Init.treat/Sec.treat,data=resp_EXP2_om) #reteatment and day with day as a random factor
anova(m2.resp) # anova of lmer
summary(m2.resp) # summary of lmer
EXP2.lme.anovatable <- anova(m2.resp) # assign name to output table late in code
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2.resp)) #plot histogram of residuals
shapiro.test(residuals(m2.resp)) # residuals are  normal - ommit ouliers at row 83 and 90 allowed normal residuals
boxplot(residuals(m2.resp)) #plot boxplot of residuals
plot( fitted(m2.resp),residuals(m2.resp)) #display residuals versus fitter, normal QQ plot, leverage plot


#Exposure2 Plotting
#barplots for mean SD exp1 - exp2
exp1_resp_summary.T # exp 1 table grouped by intit*sec treatments
exp1_resp_summary.T$trial <- "initial" # add column for trial to merge
colnames(exp1_resp_summary.T)[1] <- "Treat1_Treat2" # to match and merge with eXP2
exp2_resp_summary_all # exp 2 table grouped by initiat*sec treatments
exp2_resp_summary_all$trial <- "secondary" # add column for trial to merge
Table_EXP_1_2 <- rbind(exp1_resp_summary.T, exp2_resp_summary_all) # bind rows by treatment column
Table_EXP_1_2 # view table

# barplot resp rate exp1 and exp 2
barplot_resp <- ggplot(Table_EXP_1_2, aes(x=as.factor(Treat1_Treat2), y=FINALresp.mean , fill=trial)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=FINALresp.mean-se, ymax=FINALresp.mean+se), width=.2,position=position_dodge(.9)) +
  xlab("treatment initial*secondary") +
  ylab("Respiration rate") +
  ylim(0,0.4) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle("SMR exp1 and exp 2 mean SE") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))
barplot_resp

# barplot percent difference
percent_av_resp <- Table_EXP_1_2%>%
  group_by(Treat1_Treat2) %>%
  arrange(trial) %>%
  mutate(pct.chg = 100*(FINALresp.mean - lag(FINALresp.mean))/(FINALresp.mean)) # calculate the percent change from (secondary - initial / secondary) *100
percent_av_resp <- percent_av_resp[5:8,] # only rows with data
percent_av_resp # view the table
# plot
barplot_resp_percent <- ggplot(percent_av_resp, aes(x=as.factor(Treat1_Treat2), y=pct.chg)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  xlab("treatment initial*secondary") +
  ylab("Respiration rate") +
  ylim(0,40) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle("Percent diff in average resp rate exp1 - exp2") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))
barplot_resp_percent # view the plot

#Day 0
Day0 <- subset(x2.resp, Day==0)

Fig.Exp2.D0.resp <- ggplot(Day0, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day0$FINALresp.mean -Day0$FINALresp.se, ymax=Day0$FINALresp.mean +Day0$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
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

Fig.Exp2.D0.resp

#Day 2
Day2 <- subset(x2.resp, Day==2)

Fig.Exp2.D2.resp <- ggplot(Day2, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day2$FINALresp.mean -Day2$FINALresp.se, ymax=Day2$FINALresp.mean +Day2$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
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

Fig.Exp2.D2.resp

#Day 4
Day4 <- subset(x2.resp, Day==4)

Fig.Exp2.D4.resp <- ggplot(Day4, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day4$FINALresp.mean -Day4$FINALresp.se, ymax=Day4$FINALresp.mean +Day4$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
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

Fig.Exp2.D4.resp

#Day 6
Day6 <- subset(x2.resp, Day==6)

Fig.Exp2.D6.resp <- ggplot(Day6, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day6$FINALresp.mean -Day6$FINALresp.se, ymax=Day6$FINALresp.mean +Day6$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
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

Fig.Exp2.D6.resp

#Averaged across Days

Fig.Exp2.All.resp <- ggplot(x2.1.resp, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=x2.1.resp$FINALresp.mean -x2.1.resp$FINALresp.se, ymax=x2.1.resp$FINALresp.mean +x2.1.resp$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1), #Set the text angle
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

Fig.Exp2.All.resp


# plot all data with mean standard error and time
LINEFig.Exp2.All.resp <- ggplot(x2.resp, aes(x=Day, y=FINALresp.mean , group=treatments)) + 
  geom_errorbar(aes(ymin=x2.resp$FINALresp.mean -x2.resp$FINALresp.se, ymax=x2.resp$FINALresp.mean +x2.resp$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=treatments), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=treatments), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
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

LINEFig.Exp2.All.resp


# Saving output plots

#Figure1.Exp1.Resp <- arrangeGrob(Exp1.Fig.resp, ncol=1)
#ggsave(file="Output/Geoduck_Resp_Exp1.pdf", Exp1.Fig.resp, width = 6.5, height = 8, units = c("in"))


#Figure1.Exp2.Resp <- arrangeGrob(Exp2.Fig.resp,Fig.Exp2.D0.resp, Fig.Exp2.D2.resp,Fig.Exp2.D4.resp,Fig.Exp2.D6.resp, ncol=1)
#ggsave(file="Output/Geoduck_Resp_Exp2.byDay.pdf", Figure1.Exp2.Resp, width = 6.5, height = 15, units = c("in"))

#Figure1.Exp2.AllResp <- arrangeGrob(Fig.Exp2.All.resp, ncol=1)
#ggsave(file="Output/Geoduck_Resp_Exp2.All.pdf", Figure1.Exp2.AllResp, width = 6.5, height = 8, units = c("in"))

# Saving anova tables 

#Table1.Exp1.Resp <- data.frame(EXP1.lme.anovatable)
#write.csv(file="Output/Geoduck_Resp.table_Exp1.csv", Table1.Exp1.Resp)

#Table12.Exp2.Resp <- data.frame(EXP2.lme.anovatable)
#write.csv(file="Output/Geoduck_Resp.table_Exp2.csv", Table12.Exp2.Resp)



#---------------------------------------------------------------------------------------------------
# Growth Data - Analysis, Graphs, Models  (summarized analysis from Stats_growth_analysis.R)
###################################################################################################

#Load Size Data
size<-read.csv("Data/All_growth_data.csv", header=T, sep=",", na.string="NA", as.is=T) 

size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals
# DIVIDE THE DATASET BETWEEN EXPERIMENTS 1 AND 2
size_EXP1_with_basal <- subset(size, Exp.Num=="Exp1") # exposure 1
size_EXP1 <- subset(size_EXP1_with_basal, Date!="20180713")
size_EXP2 <- subset(size, Exp.Num=="Exp2") # exposure 2
size_EXP2.0 <- subset(size, Exp.Num=="Exp2") 
size_EXP2.0 <-subset(size_EXP2.0, Day!=0) # exposure 2 without day 0 
size_Exp1.T<- merge(size_EXP1, size_EXP2, by=c("tank"))  #merge to obtained combined treatments from EXP2


inital_size <- subset(size_EXP1, Date=="20180716") # get starting size of indiivduals from first measurements
StartSize <- summarySE(inital_size, measurevar="shell_size", groupvars=c("Date")) #summary table for starting shell length = 5.077962 ± 0.6622871 (mean ± SD)
end_size <- subset(size_EXP1, Date=="20180724") # get the ned size on Day 10 of exposure 1
# overall shell size in EXP1 and EXP2
exp1_size_all <- do.call(data.frame,aggregate(shell_size ~ treatment*Date, data = size_EXP1, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by  treatments date EXP1
exp1_size_summary_all <- summarySE(exp1_size_all, measurevar="shell_size.mean", groupvars=c("treatment")) # shell length by  treatments EXP1
exp1_size_overall <- summarySE(exp1_size_all, measurevar="shell_size.mean") # overall SMR EXP2

exp1_size_4.treatments <- do.call(data.frame,aggregate(shell_size.x ~ treatment.y*Date.x, data = size_Exp1.T, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by  treatments date EXP1
exp1_size_summary_4.treatments <- summarySE(exp1_size_4.treatments, measurevar="shell_size.x.mean", groupvars=c("treatment.y")) # shell length by  treatments EXP1

exp2_size_all <- do.call(data.frame,aggregate(shell_size ~ treatment*Date, data = size_EXP2, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by init*sec treatments date EXP2
exp2_size_summary_all <- summarySE(exp2_size_all, measurevar="shell_size.mean", groupvars=c("treatment")) # shell length by init*sec  treatments EXP2

exp2_size_all.2 <- do.call(data.frame,aggregate(shell_size ~ Sec.Trt*Date, data = size_EXP2, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by sec treatments date EXP2
exp2_size_summary_all.2 <- summarySE(exp2_size_all.2, measurevar="shell_size.mean", groupvars=c("Sec.Trt")) # shell length by sec  treatments EXP2
exp2_size_overall <- summarySE(exp2_size_all, measurevar="shell_size.mean") # overall SMR EXP2


# Model and visualize EXP1 intiial size by treatment
# by treatment
treat.exp1 <- aov(shell_size~treatment,data=inital_size)
anova(treat.exp1) # no difference in shell size with treatment
summary(treat.exp1)
Fig.d2.EXP1.treatment <- ggboxplot(inital_size, x = "treatment", y = "shell_size", color = "treatment", ylab= "Shell Size (mm)",
                              palette = c(), main= "Day 2 Exp 1") 
# Model and visualize EXP1 end size by treatment
# by treatment
treat.exp1.Day10 <- aov(shell_size~treatment,data=end_size)
anova(treat.exp1.Day10) # no difference in shell size with treatment
Fig.d10.EXP1.treatment <- ggboxplot(end_size, x = "treatment", y = "shell_size", color = "treatment", ylab= "Shell Size (mm)",
                                   palette = c(), main= "Day 10 Exp 1") 

#plot end of exp 1 Day 10 with treatments 
length_EXP1_Day10 <- ggplot(end_size, aes(x = treatment, y = shell_size, fill = treatment)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  ylim(2,8.2) +
  geom_point(aes(fill = treatment), size = 0.5, shape = 21, position = position_jitterdodge(0.05)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, position = position_jitterdodge(0.3)) +
  theme(legend.position = c(.92, .9), legend.text=element_text(size=8)) +
  labs(y="shell length", x = "Treatment", fill="") + 
  ggtitle("Juvenile geoduck shell length \nin OA treatments length_EXP1")
length_EXP1_Day10


# Model and visualize EXP2 intiial size by treatment
exp2_inital_size <- subset(size_EXP2, Date=="20180807") # get starting size of indiivduals from first measurements on Day 0 in Exp 2
# by treatment
treat.exp2 <- aov(shell_size~treatment,data=exp2_inital_size)
anova(treat.exp2) # no difference in shell size with treatment
summary(treat.exp2)
Fig.d2.EXP2.treatment <- ggboxplot(exp2_inital_size, x = "treatment", y = "shell_size", color = "treatment", ylab= "Shell Size (mm)",
                                   palette = c(), main= "Day 2 Exp 1") 
#######################
# OA Exposure 1
# Visualize OA exposure 1 (with basal shell length from initial SMR readings)
Exp1.Fig <- ggboxplot(size_EXP1_with_basal, x = "Day", y = "shell_size", color = "treatment", ylab= "Shell Size (mm)",
                      palette = c("blue", "red"), main= "Initial Exposure")
Exp1.Fig # view the plot

#plot treatments and time
length_EXP1_plot <- ggplot(size_EXP1_with_basal, aes(x = factor(Day), y = shell_size, fill = treatment)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  ylim(2,8.2) + scale_x_discrete(limits=c("prebasal",2,5,8,10)) +
  geom_point(aes(fill = treatment), size = 0.5, shape = 21, position = position_jitterdodge(0.05)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, position = position_jitterdodge(0.3)) +
  theme(legend.position = c(.92, .9), legend.text=element_text(size=8)) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) + labs(y="shell length", 
                                                                  x = "Date", fill="") + 
  ggtitle("Juvenile geoduck shell length \nin OA treatments resp_EXP1")
length_EXP1_plot


x1 <- do.call(data.frame,aggregate(shell_size ~ Day*treatment, data = size_EXP1, function(x) c(mean = mean(x), se = std.error(x)))) #mean and st. error table
# Linear mixe effects model - Test for size differences in experiment 1
Init.lme <- lmer(shell_size ~ treatment*Day + (1|Day), data = size_EXP1) # test for treatment fixed and day as a random factor
anova(Init.lme) # anova on the model
summary(Init.lme) # view summary
m1 <- lme(shell_size~treatment*Day,random=~1|Day,data=size_EXP1) # run same model as lme for stats in anova table
anova(m1) # view anova table of lme and compare to anova table from lmer
EXP1.lme.size.anovatable <- anova(m1) # assign a name to anova table to save table output later
# view plots and check for assumptions of normality in residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m1)) #plot histogram of residuals
boxplot(residuals(m1)) #plot boxplot of residuals
plot(fitted(m1),residuals(m1)) #display residuals versus fitter, normal QQ plot, leverage plot
shapiro.test(residuals(m1)) # residuals are not normally distributed
# NO STATISTICAL DIFFERENCE IN SIZE AFTER THE INITIAL EXPOSURE

# Transform size_Exp1 because residuals were not normal in the model
#reflect data tp transform positive skew (slight positive from histogram)
max(size_EXP1$shell_size) # get the max value of exp1 shell size to reflect transform
size_EXP1$size_reflect <- sqrt(((max(size_EXP1$shell_size))+1) - size_EXP1$shell_size) # transform via sqrt(1+max - x)
shapiro.test(size_EXP1$size_reflect) # normally distributed
histogram(size_EXP1$size_reflect) # histogram shows normal didstribution

# linear mixed effects model on the tranfromed size data test 
Init.lme.trans <- lmer(size_reflect ~ treatment*Day + (1|Day), data = size_EXP1) # test for treatment fixed and day as a random factor
anova(Init.lme.trans)# anova on the model
summary(Init.lme.trans)# view summary
m1.trans <- lme(size_reflect~treatment*Day,random=~1|Day,data=size_EXP1)# run same model as lme for stats in anova table
anova(m1.trans)# view anova table of lme and compare to anova table from lmer
EXP1.lme.size.anovatable.transformed <- anova(m1.trans)# assign a name to anova table to save table output later
# view plots and check for assumptions of normality in residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m1.trans)) #plot histogram of residuals
boxplot(residuals(m1.trans)) #plot boxplot of residuals
plot(fitted(m1.trans),residuals(m1.trans)) #display residuals versus fitter, normal QQ plot, leverage plot
shapiro.test(residuals(m1.trans)) # residuals normally distributed
# NO STATISTICAL DIFFERENCE IN SIZE AFTER THE INITIAL EXPOSURE

#####################
#OA Exposure 2

#Visualize OA exposure 2
Exp2.Fig <- ggboxplot(size_EXP2, x = "Day", y = "shell_size", color = "treatment", ylab= "Size (mm)",
                      palette = c("light blue", "darkblue", "pink", "red"))
Exp2.Fig # view the plot

#plot treatments and time
length_EXP2_plot <- ggplot(size_EXP2, aes(x = factor(Day), y = shell_size, fill = treatment)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  ylim(2, 8.2) +
  geom_point(aes(fill = treatment), size = 0.5, shape = 21, position = position_jitterdodge(0.05)) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=2, position = position_jitterdodge(0.3)) +
  theme(legend.position = c(.92, .9), legend.text=element_text(size=8)) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) + labs(y="shell length", 
                                                                  x = "Date", fill="") + 
  ggtitle("Juvenile geoduck shell length \nin OA treatments length_EXP2")
length_EXP2_plot


x2 <- do.call(data.frame,aggregate(shell_size ~ Day*Init.Trt*Sec.Trt, data = size_EXP2, function(x) c(mean = mean(x), se = std.error(x))))#mean and st. error table (used from plotting later)
x2.1 <- do.call(data.frame,aggregate(shell_size ~ Init.Trt*Sec.Trt, data = size_EXP2.0, function(x) c(mean = mean(x), se = std.error(x))))#mean and st. error table(used from plotting later)
x2$treatments <- paste(x2$Init.Trt, x2$Sec.Trt, sep="_") # combine treatments in a column
m2 <- lme(shell_size~Init.Trt*Sec.Trt,random=~1|Day/Init.Trt/Sec.Trt,data=size_EXP2.0) # lme model with initial and secondary treatment effects fixed and time random
anova(m2) # view anova table
summary(m2) # view summary of anova table
EXP2.lme.size.anovatable <- anova(m2) # name anova table to save output later
exp2.ph <- lsmeans(m2, pairwise ~ Init.Trt*Sec.Trt)# pariwise Tukey Post-hoc test between repeated treatments
exp2.ph # view post hoc summary
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2)) #plot histogram of residuals
boxplot(residuals(m2)) #plot boxplot of residuals
plot( fitted(m2),residuals(m2)) #display residuals versus fitter, normal QQ plot, leverage plot
shapiro.test(residuals(m2)) # residuals NOT normally distributed

# RUN test again with tranformed data

# Test for size differences in experiment 2 (TRANSFORMED)
#reflect data tp transform positive skew
max(size_EXP2$shell_size) # get the max value of exp1 shell size to reflect transform
size_EXP2$size_reflect <- ((max(size_EXP2$shell_size)+1) - size_EXP2$shell_size) # transform via sqrt(1+max - x)
shapiro.test(size_EXP2$size_reflect) # homogeneity of variance test - normally distributed
histogram(size_EXP2$size_reflect) # histogram shows normal didstribution

m2.trans <- lme(shell_size~Init.Trt*Sec.Trt,random=~1|Day/Init.Trt/Sec.Trt,data=size_EXP2.0)

anova(m2.trans)
EXP2.lme.size.anovatable.transformed <- anova(m2.trans)
summary(m2.trans)

#Post hoc
exp2.ph.trans <- lsmeans(m2.trans, pairwise ~ Init.Trt*Sec.Trt)
exp2.ph.trans # view post hoc summary

par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2.trans)) #plot histogram of residuals
boxplot(residuals(m2.trans)) #plot boxplot of residuals
plot( fitted(m2.trans),residuals(m2.trans)) #display residuals versus fitter, normal QQ plot, leverage plot
shapiro.test(residuals(m2.trans)) # 

#Exposure2 Plotting

#barplots for mean SD exp1 - exp2
exp1_size_summary_4.treatments # exp 1 - 4 treatments
exp2_size_summary_all# exp 2 - 4 treatments
exp1_size_summary_4.treatments$trial <- "initial" # add column for trial to merge
colnames(exp1_size_summary_4.treatments)[1] <- "treatment" # to match and merge with eXP2
colnames(exp1_size_summary_4.treatments)[3] <- "shell_size.mean"
exp2_size_summary_all$trial <- "secondary" # add column for trial to merge
SIZETable_EXP_1_2 <- rbind(exp1_size_summary_4.treatments, exp2_size_summary_all) # bind rows by treatment column
SIZETable_EXP_1_2 # view table

# barplot size rate exp1 and exp 2
barplot_size <- ggplot(SIZETable_EXP_1_2, aes(x=as.factor(treatment), y=shell_size.mean , fill=trial)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=shell_size.mean-se, ymax=shell_size.mean+se), width=.2,position=position_dodge(.9)) +
  xlab("treatment initial*secondary") +
  ylab("Shell length") +
  ylim(0,8) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle("Shell length exp1 and exp 2 mean SE") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))
barplot_size

# barplot percent difference
percent_av_size <- SIZETable_EXP_1_2%>%
  group_by(treatment) %>%
  arrange(trial) %>%
  mutate(pct.chg = 100*(shell_size.mean - lag(shell_size.mean))/(shell_size.mean)) # calculate the percent change from (secondary - initial / secondary) *100
percent_av_size <- percent_av_size[5:8,] # only rows with data
percent_av_size # view the table
# plot
barplot_size_percent <- ggplot(percent_av_size, aes(x=as.factor(treatment), y=pct.chg)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  xlab("treatment initial*secondary") +
  ylab("Shell length") +
  ylim(0,15) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle("Percent diff in average shell length exp1 - exp2") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))
barplot_size_percent # view the plot





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
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1), #Set the text angle
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


# plot all data with mean standard error and time
LINEFig.Exp2.All.size <- ggplot(x2, aes(x=Day, y=shell_size.mean , group=treatments)) + 
  geom_errorbar(aes(ymin=x2$shell_size.mean -x2$shell_size.se  , ymax=x2$shell_size.mean +x2$shell_size.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=treatments), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=treatments), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
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

LINEFig.Exp2.All.size


# Saving output plots

#Figure1.Exp1.Size <- arrangeGrob(Exp1.Fig, ncol=1)
#ggsave(file="Output/Geoduck_Size_Exp1.pdf", Figure1.Exp1.Size, width = 6.5, height = 8, units = c("in"))


#Figure1.Exp2.Size <- arrangeGrob(Exp2.Fig,Fig.Exp2.D0.size, Fig.Exp2.D2.size,Fig.Exp2.D4.size,Fig.Exp2.D6.size, ncol=1)
#ggsave(file="Output/Geoduck_Size_Exp2.byDay.pdf", Figure1.Exp2.Size, width = 6.5, height = 15, units = c("in"))

#Figure1.Exp2.AllSize <- arrangeGrob(Fig.Exp2.All.size, ncol=1)
#ggsave(file="Output/Geoduck_Size_Exp2.All.pdf", Figure1.Exp2.AllSize, width = 6.5, height = 8, units = c("in"))

# Saving anova tables 

#Table1.Exp1.Size <- data.frame(EXP1.lme.size.anovatable)
#write.csv(file="Output/Geoduck_Size.table_Exp1.csv", Table1.Exp1.Size)

#Table12.Exp2.Size <- data.frame(EXP2.lme.size.anovatable)
#write.csv(file="Output/Geoduck_Size.table_Exp2.csv", Table12.Exp2.Size)

figure_1 <- ggarrange(resp_EXP1_plot, resp_EXP2_plot, length_EXP1_plot, length_EXP2_plot,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
figure_1

figure_2 <- ggarrange(resp_EXP1_plot_2, Fig.Exp2.All.resp, length_EXP1_Day10, Fig.Exp2.All.size,
                      labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)
figure_2

