# Project: Geoduck Conditioning
# Title: Chemistry Summary
# Supported by: FFAR
# Author: Sam Gurr and Hollie Putnam
# Date Updated: 20181024
# Contact: samuel_gurr@uri.edu

####################################################
rm(list=ls())

#LIBRARY-----------------------------------------------------------------------------------------------------------
library(Rmisc)

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/") #set working

#---------------------------------------------------------------------------------------------------
# Seawater chemistry Data - Analysis, Graphs, Tables (APEX DATA)
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
# Discrete Seawater Chemistry Tables 
########################################################################################################################


chem<-read.csv("Output/Seawater_chemistry_table_Output_All.csv", header=T, sep=",", na.string="NA", as.is=T) 

chem.exp <-subset(chem, Treatment!="na") #remove na - na often set as the treatment for samples of the sump
chem.exp1 <-chem.exp[44:131,] # exposure 1
chem.exp2 <- chem.exp[157:204,] # exposure 2
chem.exp_1_2 <- rbind(chem.exp1,chem.exp2) # exposure 1 and 2

### EXP1 ### 10-day OA exposure

salinity.1<- do.call(data.frame,aggregate(Salinity ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x)))) # table of means __ per day by treatment (4 tanks per treatment)
salinity.summary.1 <- summarySE(salinity.1, measurevar="Salinity.mean", groupvars=c("Treatment"))  # overall mean for each treatment 

Temperature.1<- do.call(data.frame,aggregate(Temperature ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x)))) 
Temperature.summary.1 <- summarySE(Temperature.1, measurevar="Temperature.mean", groupvars=c("Treatment"))

pH.1<- do.call(data.frame,aggregate(pH ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
pH.summary.1 <- summarySE(pH.1, measurevar="pH.mean", groupvars=c("Treatment"))

CO2.1<- do.call(data.frame,aggregate(CO2 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
CO2.summary.1 <- summarySE(CO2.1, measurevar="CO2.mean", groupvars=c("Treatment"))

pCO2.1<- do.call(data.frame,aggregate(pCO2 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
pCO2.summary.1 <- summarySE(pCO2.1, measurevar="pCO2.mean", groupvars=c("Treatment"))

HCO3.1<- do.call(data.frame,aggregate(HCO3 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
HCO3.summary.1 <- summarySE(HCO3.1, measurevar="HCO3.mean", groupvars=c("Treatment"))

CO3.1<- do.call(data.frame,aggregate(CO3 ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
CO3.summary.1 <- summarySE(CO3.1, measurevar="CO3.mean", groupvars=c("Treatment"))

DIC.1<- do.call(data.frame,aggregate(DIC ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
DIC.summary.1 <- summarySE(DIC.1, measurevar="DIC.mean", groupvars=c("Treatment"))

TA.1<- do.call(data.frame,aggregate(TA ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
TA.summary.1 <- summarySE(TA.1, measurevar="TA.mean", groupvars=c("Treatment"))

Aragonite.Sat.1<- do.call(data.frame,aggregate(Aragonite.Sat ~ Treatment*Date, data = chem.exp1, function(x) c(mean = mean(x), se = std.error(x))))
Aragonite.Sat.summary.1 <- summarySE(Aragonite.Sat.1, measurevar="Aragonite.Sat.mean", groupvars=c("Treatment"))

#Final table EXPOSURE 1 - 10-day
########################################################################################################################

EXP1_FinalTable <- as.data.frame(matrix(nrow = 2))  # make a data frame with 2 rows   

EXP1_FinalTable$Treatment <- salinity.summary.1$Treatment # column for treatments

EXP1_FinalTable$Days <- signif(salinity.summary.1$N, digits = 2) # column for number of dayaa as N

Temperature.1 <- signif(Temperature.summary.1$Temperature.mean, digits = 4) #significant figs for the mean of each value
Temp.sd.1 <- signif(Temperature.summary.1$sd, digits = 2)#significant figs for the standard dev of each value
EXP1_FinalTable$Salinity <-paste(Temperature.1, Temp.sd.1, sep="�") # combine mean � st error as column in final table

Salinity.1 <- signif(salinity.summary.1$Salinity.mean, digits = 4)
Sal.sd.1 <- signif(salinity.summary.1$sd, digits = 2)
EXP1_FinalTable$Salinity <-paste(Salinity.1, Sal.sd.1, sep="�")

pH.1 <- signif(pH.summary.1$pH.mean, digits = 3)
pH.sd.1 <- signif(pH.summary.1$sd, digits = 1)
EXP1_FinalTable$pH <-paste(pH.1, pH.sd.1, sep="�")

CO2.1 <- signif(CO2.summary.1$CO2.mean, digits = 4)
CO2.sd.1 <- signif(CO2.summary.1$sd, digits = 3)
EXP1_FinalTable$CO2 <-paste(CO2.1, CO2.sd.1, sep="�")

pCO2.1 <- signif(pCO2.summary.1$pCO2.mean, digits = 5)
pCO2.sd.1 <- signif(pCO2.summary.1$sd, digits = 4)
EXP1_FinalTable$pCO2 <-paste(pCO2.1, pCO2.sd.1, sep="�")

HCO3.1 <- signif(HCO3.summary.1$HCO3.mean, digits = 6)
HCO3.sd.1 <- signif(HCO3.summary.1$sd, digits = 3)
EXP1_FinalTable$HCO3 <-paste(HCO3.1, HCO3.sd.1, sep="�")

CO3.1 <- signif(CO3.summary.1$CO3.mean, digits = 4)
CO3.sd.1 <- signif(CO3.summary.1$sd, digits = 3)
EXP1_FinalTable$CO3 <-paste(CO3.1, CO3.sd.1, sep="�")

DIC.1 <- signif(DIC.summary.1$DIC.mean, digits = 4)
DIC.sd.1 <- signif(DIC.summary.1$sd, digits = 3)
EXP1_FinalTable$DIC <-paste(DIC.1, DIC.sd.1, sep="�")

TA.1 <- signif(TA.summary.1$TA.mean, digits = 4)
TA.sd.1 <- signif(TA.summary.1$sd, digits = 3)
EXP1_FinalTable$Total.Alkalinity <-paste(TA.1, TA.sd.1 , sep="�")

Aragonite.Sat.1 <- signif(Aragonite.Sat.summary.1$Aragonite.Sat.mean, digits = 2)
Arag.sd.1 <- signif(Aragonite.Sat.summary.1$sd, digits = 2)
EXP1_FinalTable$Aragonite.Saturation <-paste(Aragonite.Sat.1, Arag.sd.1, sep="�")

EXP1_FinalTable <- subset(EXP1_FinalTable, select = -c(V1)) # ommit the first column
head(EXP1_FinalTable) # view final table

### EXP2 ### 6-day OA exposure
########################################################################################################################

salinity.2<- do.call(data.frame,aggregate(Salinity ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
salinity.summary.2 <- summarySE(salinity.2, measurevar="Salinity.mean", groupvars=c("Treatment"))

Temperature.2<- do.call(data.frame,aggregate(Temperature ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
Temperature.summary.2 <- summarySE(Temperature.2, measurevar="Temperature.mean", groupvars=c("Treatment"))

pH.2<- do.call(data.frame,aggregate(pH ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
pH.summary.2 <- summarySE(pH.2, measurevar="pH.mean", groupvars=c("Treatment"))

CO2.2<- do.call(data.frame,aggregate(CO2 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
CO2.summary.2 <- summarySE(CO2.2, measurevar="CO2.mean", groupvars=c("Treatment"))

pCO2.2<- do.call(data.frame,aggregate(pCO2 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
pCO2.summary.2 <- summarySE(pCO2.2, measurevar="pCO2.mean", groupvars=c("Treatment"))

HCO3.2<- do.call(data.frame,aggregate(HCO3 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
HCO3.summary.2 <- summarySE(HCO3.2, measurevar="HCO3.mean", groupvars=c("Treatment"))

CO3.2<- do.call(data.frame,aggregate(CO3 ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
CO3.summary.2 <- summarySE(CO3.2, measurevar="CO3.mean", groupvars=c("Treatment"))

DIC.2<- do.call(data.frame,aggregate(DIC ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
DIC.summary.2 <- summarySE(DIC.2, measurevar="DIC.mean", groupvars=c("Treatment"))

TA.2<- do.call(data.frame,aggregate(TA ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
TA.summary.2 <- summarySE(TA.2, measurevar="TA.mean", groupvars=c("Treatment"))

Aragonite.Sat.2<- do.call(data.frame,aggregate(Aragonite.Sat ~ Treatment*Date, data = chem.exp2, function(x) c(mean = mean(x), se = std.error(x))))
Aragonite.Sat.summary.2 <- summarySE(Aragonite.Sat.2, measurevar="Aragonite.Sat.mean", groupvars=c("Treatment"))

#Final table EXPOSURE 2 - 6-day
########################################################################################################################

EXP2_FinalTable <- as.data.frame(matrix(nrow = 2)) 

EXP2_FinalTable$Treatment <- salinity.summary.2$Treatment

EXP2_FinalTable$Days <- signif(salinity.summary.2$N, digits = 2)

Temperature.2 <- signif(Temperature.summary.2$Temperature.mean, digits = 4)
Temp.sd.2 <- signif(Temperature.summary.2$sd, digits = 2)
EXP2_FinalTable$Salinity <-paste(Temperature.2, Temp.sd.2, sep="�")

Salinity.2 <- signif(salinity.summary.2$Salinity.mean, digits = 4)
Sal.sd.2 <- signif(salinity.summary.2$sd, digits = 2)
EXP2_FinalTable$Salinity <-paste(Salinity.2, Sal.sd.2, sep="�")

pH.2 <- signif(pH.summary.2$pH.mean, digits = 3)
pH.sd.2 <- signif(pH.summary.2$sd, digits = 1)
EXP2_FinalTable$pH <-paste(pH.2, pH.sd.2, sep="�")

CO2.2 <- signif(CO2.summary.2$CO2.mean, digits = 4)
CO2.sd.2 <- signif(CO2.summary.2$sd, digits = 3)
EXP2_FinalTable$CO2 <-paste(CO2.2, CO2.sd.2, sep="�")

pCO2.2 <- signif(pCO2.summary.2$pCO2.mean, digits = 5)
pCO2.sd.2 <- signif(pCO2.summary.2$sd, digits = 4)
EXP2_FinalTable$pCO2 <-paste(pCO2.2, pCO2.sd.2, sep="�")

HCO3.2 <- signif(HCO3.summary.2$HCO3.mean, digits = 6)
HCO3.sd.2 <- signif(HCO3.summary.2$sd, digits = 3)
EXP2_FinalTable$HCO3 <-paste(HCO3.2, HCO3.sd.2, sep="�")

CO3.2 <- signif(CO3.summary.2$CO3.mean, digits = 4)
CO3.sd.2 <- signif(CO3.summary.2$sd, digits = 3)
EXP2_FinalTable$CO3 <-paste(CO3.2, CO3.sd.2, sep="�")

DIC.2 <- signif(DIC.summary.2$DIC.mean, digits = 4)
DIC.sd.2 <- signif(DIC.summary.2$sd, digits = 3)
EXP2_FinalTable$DIC <-paste(DIC.2, DIC.sd.2, sep="�")

TA.2 <- signif(TA.summary.2$TA.mean, digits = 4)
TA.sd.2 <- signif(TA.summary.2$sd, digits = 3)
EXP2_FinalTable$Total.Alkalinity <-paste(TA.2, TA.sd.2 , sep="�")

Aragonite.Sat.2 <- signif(Aragonite.Sat.summary.2$Aragonite.Sat.mean, digits = 2)
Arag.sd.2 <- signif(Aragonite.Sat.summary.2$sd, digits = 2)
EXP2_FinalTable$Aragonite.Saturation <-paste(Aragonite.Sat.2, Arag.sd.2, sep="�")

EXP2_FinalTable <- subset(EXP2_FinalTable, select = -c(V1)) # ommit the first column
head(EXP2_FinalTable) # view final table

### EXP_1_2 ### ALL OA exposure
########################################################################################################################

salinity.all<- do.call(data.frame,aggregate(Salinity ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
salinity.summary.all <- summarySE(salinity.all, measurevar="Salinity.mean", groupvars=c("Treatment"))

Temperature.all<- do.call(data.frame,aggregate(Temperature ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
Temperature.summary.all <- summarySE(Temperature.all, measurevar="Temperature.mean", groupvars=c("Treatment"))

pH.all<- do.call(data.frame,aggregate(pH ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
pH.summary.all <- summarySE(pH.all, measurevar="pH.mean", groupvars=c("Treatment"))

CO2.all<- do.call(data.frame,aggregate(CO2 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
CO2.summary.all <- summarySE(CO2.all, measurevar="CO2.mean", groupvars=c("Treatment"))

pCO2.all<- do.call(data.frame,aggregate(pCO2 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
pCO2.summary.all <- summarySE(pCO2.all, measurevar="pCO2.mean", groupvars=c("Treatment"))

HCO3.all<- do.call(data.frame,aggregate(HCO3 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
HCO3.summary.all <- summarySE(HCO3.all, measurevar="HCO3.mean", groupvars=c("Treatment"))

CO3.all<- do.call(data.frame,aggregate(CO3 ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
CO3.summary.all <- summarySE(CO3.all, measurevar="CO3.mean", groupvars=c("Treatment"))

DIC.all<- do.call(data.frame,aggregate(DIC ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
DIC.summary.all <- summarySE(DIC.all, measurevar="DIC.mean", groupvars=c("Treatment"))

TA.all<- do.call(data.frame,aggregate(TA ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
TA.summary.all <- summarySE(TA.all, measurevar="TA.mean", groupvars=c("Treatment"))

Aragonite.Sat.all<- do.call(data.frame,aggregate(Aragonite.Sat ~ Treatment*Date, data = chem.exp_1_2, function(x) c(mean = mean(x), se = std.error(x))))
Aragonite.Sat.summary.all <- summarySE(Aragonite.Sat.all, measurevar="Aragonite.Sat.mean", groupvars=c("Treatment"))

#Final table EXPOSURE ALL OA Exposure
########################################################################################################################

EXP_1_2_FinalTable <- as.data.frame(matrix(nrow = 2)) 

EXP_1_2_FinalTable$Treatment <- salinity.summary.all$Treatment

EXP_1_2_FinalTable$Days <- signif(salinity.summary.all$N, digits = 2)

Temperature.all <- signif(Temperature.summary.all$Temperature.mean, digits = 4)
Temp.sd.all <- signif(Temperature.summary.all$sd, digits = 2)
EXP_1_2_FinalTable$Salinity <-paste(Temperature.all, Temp.sd.all, sep="�")

Salinity.all <- signif(salinity.summary.all$Salinity.mean, digits = 4)
Sal.sd.all <- signif(salinity.summary.all$sd, digits = 2)
EXP_1_2_FinalTable$Salinity <-paste(Salinity.all, Sal.sd.all, sep="�")

pH.all <- signif(pH.summary.all$pH.mean, digits = 3)
pH.sd.all <- signif(pH.summary.all$sd, digits = 1)
EXP_1_2_FinalTable$pH <-paste(pH.all, pH.sd.all, sep="�")

CO2.all <- signif(CO2.summary.all$CO2.mean, digits = 4)
CO2.sd.all <- signif(CO2.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$CO2 <-paste(CO2.all, CO2.sd.all, sep="�")

pCO2.all <- signif(pCO2.summary.all$pCO2.mean, digits = 5)
pCO2.sd.all <- signif(pCO2.summary.all$sd, digits = 4)
EXP_1_2_FinalTable$pCO2 <-paste(pCO2.all, pCO2.sd.all, sep="�")

HCO3.all <- signif(HCO3.summary.all$HCO3.mean, digits = 6)
HCO3.sd.all <- signif(HCO3.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$HCO3 <-paste(HCO3.all, HCO3.sd.all, sep="�")

CO3.all <- signif(CO3.summary.all$CO3.mean, digits = 4)
CO3.sd.all <- signif(CO3.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$CO3 <-paste(CO3.all, CO3.sd.all, sep="�")

DIC.all <- signif(DIC.summary.all$DIC.mean, digits = 4)
DIC.sd.all <- signif(DIC.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$DIC <-paste(DIC.all, DIC.sd.all, sep="�")

TA.all <- signif(TA.summary.all$TA.mean, digits = 4)
TA.sd.all <- signif(TA.summary.all$sd, digits = 3)
EXP_1_2_FinalTable$Total.Alkalinity <-paste(TA.all, TA.sd.all , sep="�")

Aragonite.Sat.all <- signif(Aragonite.Sat.summary.all$Aragonite.Sat.mean, digits = 2)
Arag.sd.all <- signif(Aragonite.Sat.summary.all$sd, digits = 2)
EXP_1_2_FinalTable$Aragonite.Saturation <-paste(Aragonite.Sat.all, Arag.sd.all, sep="�")

EXP_1_2_FinalTable <- subset(EXP_1_2_FinalTable, select = -c(V1)) # ommit the first column
head(EXP_1_2_FinalTable) # view final table