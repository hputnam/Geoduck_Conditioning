#Title: Juvenile Repeat Exposure Experiment 2018
#Project: FFAR
#Author: HM Putnam & Sam Gurr
#Edited by: Sam Gurr
#Date Last Modified: 20181226
#See Readme file for details

rm(list=ls())
## Install packages if not already in your library
if ("dplyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('dplyr') 
if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 
if ("ggplot2" %in% rownames(installed.packages()) == 'FALSE') install.packages('ggplot2') 
if ("ggpubr" %in% rownames(installed.packages()) == 'FALSE') install_github('ggpubr') 
if ("Rmisc" %in% rownames(installed.packages()) == 'FALSE') install.packages('Rmisc') 
#if ("nlme" %in% rownames(installed.packages()) == 'FALSE') install.packages('nlme') 
#if ("lme4" %in% rownames(installed.packages()) == 'FALSE') install.packages('lme4') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("lsmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('lsmeans') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("reshape" %in% rownames(installed.packages()) == 'FALSE') install.packages('reshape') 
if ("multcompView" %in% rownames(installed.packages()) == 'FALSE') install.packages('multcompView') 
if ("tidyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyr') 
if ("Rcmdr" %in% rownames(installed.packages()) == 'FALSE') install.packages('Rcmdr') 
if ("rlang" %in% rownames(installed.packages()) == 'FALSE') install.packages('rlang') 
# Load packages and pacage version/date/import/depends info
library(dplyr)          # Version 0.7.6, Packaged: 2018-06-27, Depends: R (>= 3.1.2)Imports: assertthat (>= 0.2.0), bindrcpp (>= 0.2.0.9000), glue (>=1.1.1), magrittr (>= 1.5), methods, pkgconfig (>= 2.0.1), R6(>= 2.2.2), Rcpp (>= 0.12.15), rlang (>= 0.2.0), tibble (>=1.3.1), tidyselect (>= 0.2.3), utils
library(plyr)           # Version 1.8.4, Packaged: 2016-06-07, Depends: R (>= 3.1.0) Imports: Rcpp (>= 0.11.0)
library(ggplot2)        # Version 3.1.0, Date/Publication: 2018-10-25, Depends: R (>= 3.1)Imports: digest, grid, gtable (>= 0.1.1), lazyeval, MASS, mgcv, plyr(>= 1.7.1), reshape2, rlang (>= 0.2.1), scales (>= 0.5.0),stats, tibble, viridisLite, withr (>= 2.0.0)
library(ggpubr)         # Version: 0.1.8 Date: 2018-08-30, Depends: R (>= 3.1.0), ggplot2, magrittrImports: ggrepel, grid, ggsci, stats, utils, tidyr, purrr, dplyr(>=0.7.1), cowplot, ggsignif, scales, gridExtra, glue, polynom
library(Rmisc)          # Version: 1.5 Packaged: 2013-10-21, Depends: lattice, plyr
#library(nlme)           # Version: 3.1-137, Date: 2018-04-07, Depends: R (>= 3.4.0) Imports: graphics, stats, utils, lattice
#library(lme4)           # Version: 1.1-17, Date/Publication: 2018-04-03, Depends: R (>= 3.2.0), Matrix (>= 1.2-1), methods, stats
library(plotrix)        # Version: 3.7-4, Date/Publication: 2018-10-03
library(lsmeans)        # Version: 2.27-62, Date/Publication: 2018-05-11, Depends: methods, R (>= 3.2)
library(gridExtra)      # Version: 2.3, Date/Publication: 2017-09-09, Imports: gtable, grid, grDevices, graphics, utils
library(reshape)        # Version: 0.8.7, Date/Publication: 2017-08-06, Depends: R (>= 2.6.1) Imports: plyr
library(multcompView)   # Version: 0.1-7, Date/Publication: 2015-07-31, Imports: grid
library(tidyr)          # Version: 0.8.1, Date/Publication: 2018-05-18, Depends: R (>= 3.1) Imports: dplyr (>= 0.7.0), glue, magrittr, purrr, Rcpp, rlang, stringi, tibble, tidyselect
library(Rcmdr)          # Version: 2.5-1. Date/Publication: 2018-09-11, Depends: R (>= 3.5.0), grDevices, graphics, methods, stats, utils,splines, RcmdrMisc (>= 2.5-0), car (>= 3.0-1), effects (>=4.0-3) Imports: tcltk, tcltk2 (>= 1.2-6), abind, relimp (>= 1.0-5)
library(rlang)          # Version: 0.3.0.1 Date/Publication: 2018-10-25, Depends: R (>= 3.1.0)

#Required Data files
# ----Conical Chemistry (APEX data)
#20180724_Apex_Data_Output.csv
#20180801_Apex_Data_Output.csv
#20180805_Apex_Data_Output.csv
#20180814_Apex_Data_Output.csv
# ----Heath Tray Chemistry (discrete probe data)
#Flow_rates.csv
#pH_Calibration_Files (tris data)
#Seawater_chemistry_table_Output_All.csv
# ----Respiration data
#PreExposure_resp_calc_and_standardized.csv
#All_resp_calc_and_standardized.csv
# ----Shell size data
#All_growth_data.csv

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/") #set working

### CONICAL Seawater chemistry Data - Analysis, Graphs, Tables (APEX DATA) ####

#CONTINUOUS EXPERIMENTAL APEX DATA 
#Load Apex Data 
APEX_1<-read.csv("Data/Apex_data/20180724_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_2<-read.csv("Data/Apex_data/20180801_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_3<-read.csv("Data/Apex_data/20180805_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_4<-read.csv("Data/Apex_data/20180814_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_data<- do.call("rbind", list(APEX_1, APEX_2, APEX_3, APEX_4)) # bind all data together
APEX_data$Date.Time <-as.POSIXct(APEX_data$Date.Time, format="%Y-%m-%d %H:%M:%S") #convert date format

#plot raw data
plot(APEX_data$Date.Time,APEX_data$pH_T0) # tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$pH_T1) # tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$pH_T2) # start and tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$pH_T3) # tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$TMP_T0)
plot(APEX_data$Date.Time,APEX_data$TMP_T1)
plot(APEX_data$Date.Time,APEX_data$TMP_T2)
plot(APEX_data$Date.Time,APEX_data$Temp_T3)
# truncate start and end
start.elim <- 72 # create a start time at noon on day 0
APEX_data <- APEX_data[-(1:start.elim),]
APEX_data <- APEX_data[-(4442:4538),] # deleted last rows post-treatment 
plot(APEX_data$Date.Time,APEX_data$pH_T0) # tail end pH increase is ommited

APEX_data$datehour <- cut(as.POSIXct(APEX_data$Date.Time),
                              format="%d-%m-%Y %H:%M:%S", breaks="hour") # create new column for datehour to aggregate data
head(APEX_data) # check this new column

date <- format(as.POSIXct(APEX_data$Date.Time) ,format = "%Y-%m-%d %H") # call year month and date
days <- as.Date(date) - as.Date(date[1]) # subtract from start to get number of days
days <- as.numeric(days) # convert to numeric

# pH tables and graphs for all exposures 
pH.low_t0 <- do.call(data.frame,aggregate(pH_T0 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
pH.low_t3 <- do.call(data.frame,aggregate(pH_T3 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
pH.amb_t1 <- do.call(data.frame,aggregate(pH_T1 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
pH.amb_t2 <- do.call(data.frame,aggregate(pH_T2 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour

pH.low_t0$Treatment <- "Low_1" #Add treatment Information
colnames(pH.low_t0) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
pH.low_t3$Treatment <- "Low_2" #Add treatment Information
colnames(pH.low_t3) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
pH.amb_t1$Treatment <- "Ambient_1" #Add treatment Information
colnames(pH.amb_t1) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
pH.amb_t2$Treatment <- "Ambient_2" #Add treatment Information
colnames(pH.amb_t2) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
hourly.pH <- rbind(pH.low_t0, pH.low_t3, pH.amb_t1, pH.amb_t2) #bind treatment data 
hourly.pH <- hourly.pH[!is.na(hourly.pH$se), ] # ommit rows with NA for stand error
hourly.pH <- hourly.pH[!(hourly.pH$se > 0.2),] # ommit rows with high stand error (conical cleaning)
hourly.pH #view data

# subset the data
APEX.pH.Exp1 <- hourly.pH %>%
  filter(days <= 9) # pH APEX Exp1

APEX.pH.commongarden <- hourly.pH %>%
  filter(days >= 10 & days <= 23) # pH APEX common garden

APEX.pH.Exp2 <- hourly.pH %>%
  filter(days >= 24 & days <= 30) # pH APEX Exp2

# Plot daily averages of pH data for the complete experiment (continuous APEX data)
APEX.pH.Exp1$datehour <- as.POSIXct(APEX.pH.Exp1$datehour, format="%Y-%m-%d %H:%M:%S") #format datehour 
Exp1.pH.Apex.FIG <- ggplot(APEX.pH.Exp1, aes(x=datehour, y=mean, group=Treatment)) +#Plot average diurnal cycle of temperature data
                    #geom_line() +
                    geom_point(aes(x = datehour, y = mean, group=Treatment), colour="black", cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                    geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                    position=position_dodge(0.9), data=APEX.pH.Exp1, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                    ggtitle("A) Exp1") + #Label the graph with the main title
                    #scale_x_date(date_minor_breaks = "1 day") +
                    #scale_x_date(breaks = APEX.pH.Exp1$datehour[seq(1, length(APEX.pH.Exp1$datehour), by = 24)]) +
                    ylim(7,8) + #Set Y axis limits
                    xlab("Time") + #Label the X Axis
                    ylab("pH (NBS)") + #Label the Y Axis
                    #scale_x_date(date_minor_breaks = "1 day") +
                    theme_bw() + #Set the background color
                    theme(axis.line = element_line(color = 'black'), #Set the axes color
                          axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                          panel.grid.major = element_blank(), #Set the major gridlines
                          panel.grid.minor = element_blank(), #Set the minor gridlines
                          plot.background=element_blank(), #Set the plot background
                          panel.border=element_rect(size=1.25, fill = NA), #set outer border
                          plot.title=element_text(hjust=0),
                          legend.position="bottom", #set legend location
                          legend.text = element_text(size = 8), #set the legend text size
                          legend.key = element_blank(), #remove the legend background
                          legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp1.pH.Apex.FIG  

APEX.pH.commongarden$datehour <- as.POSIXct(APEX.pH.commongarden$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
CommGarden.pH.Apex.FIG <- ggplot(APEX.pH.commongarden, aes(x=datehour, y=mean, group=Treatment)) +#Plot average diurnal cycle of temperature data
                          #geom_line() +
                          geom_point(aes(x = datehour, y = mean, group=Treatment), colour="black", cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                          geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                          position=position_dodge(0.9), data=APEX.pH.commongarden, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                          ggtitle("B) Common garden") + #Label the graph with the main title
                          #scale_x_date(breaks = APEX.pH.Exp2$hour[seq(1, length(APEX.pH.Exp2$hour), by = 24)]) +
                          ylim(7,8) + #Set Y axis limits
                          xlab("Time") + #Label the X Axis
                          ylab("pH (NBS)") + #Label the Y Axis
                          #scale_x_date(date_minor_breaks = "1 day") +
                          theme_bw() + #Set the background color
                          theme(axis.line = element_line(color = 'black'), #Set the axes color
                                axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                                axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                                axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                                panel.grid.major = element_blank(), #Set the major gridlines
                                panel.grid.minor = element_blank(), #Set the minor gridlines
                                plot.background=element_blank(), #Set the plot background
                                panel.border=element_rect(size=1.25, fill = NA), #set outer border
                                plot.title=element_text(hjust=0),
                                legend.position="bottom", #set legend location
                                legend.text = element_text(size = 8), #set the legend text size
                                legend.key = element_blank(), #remove the legend background
                                legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
CommGarden.pH.Apex.FIG 

APEX.pH.Exp2$datehour <- as.POSIXct(APEX.pH.Exp2$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
Exp2.pH.Apex.FIG <- ggplot(APEX.pH.Exp2, aes(x=datehour, y=mean, group=Treatment)) +#Plot average diurnal cycle of temperature data
                    #geom_line() +
                    geom_point(aes(x = datehour, y = mean, group=Treatment), colour="black", cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                    geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                    position=position_dodge(0.9), data=APEX.pH.Exp2, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                    ggtitle("C) Exp2") + #Label the graph with the main title
                    #scale_x_date(breaks = APEX.pH.Exp2$hour[seq(1, length(APEX.pH.Exp2$hour), by = 24)]) +
                    ylim(7,8) + #Set Y axis limits
                    xlab("Time") + #Label the X Axis
                    ylab("pH (NBS)") + #Label the Y Axis
                    #scale_x_date(date_minor_breaks = "1 day") +
                    theme_bw() + #Set the background color
                    theme(axis.line = element_line(color = 'black'), #Set the axes color
                          axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                          panel.grid.major = element_blank(), #Set the major gridlines
                          panel.grid.minor = element_blank(), #Set the minor gridlines
                          plot.background=element_blank(), #Set the plot background
                          panel.border=element_rect(size=1.25, fill = NA), #set outer border
                          plot.title=element_text(hjust=0),
                          legend.position="bottom", #set legend location
                          legend.text = element_text(size = 8), #set the legend text size
                          legend.key = element_blank(), #remove the legend background
                          legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp2.pH.Apex.FIG 

## Temp tables and graTemps for all exposures
Temp.low_t0 <- do.call(data.frame,aggregate(TMP_T0 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
Temp.low_t3 <- do.call(data.frame,aggregate(Temp_T3 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
Temp.amb_t1 <- do.call(data.frame,aggregate(TMP_T1 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
Temp.amb_t2 <- do.call(data.frame,aggregate(TMP_T2 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour

Temp.low_t0$Treatment <- "Low_1" #Add treatment Information
colnames(Temp.low_t0) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
Temp.low_t3$Treatment <- "Low_2" #Add treatment Information
colnames(Temp.low_t3) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
Temp.amb_t1$Treatment <- "Ambient_1" #Add treatment Information
colnames(Temp.amb_t1) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
Temp.amb_t2$Treatment <- "Ambient_2" #Add treatment Information
colnames(Temp.amb_t2) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
hourly.Temp <- rbind(Temp.low_t0, Temp.low_t3, Temp.amb_t1, Temp.amb_t2) #bind treatment data 
hourly.Temp <- hourly.Temp[!is.na(hourly.Temp$se), ] # ommit rows with NA for stand error
hourly.Temp <- hourly.Temp[!(hourly.Temp$se > 0.2),] # ommit rows with high stand error (conical cleaning)
hourly.Temp #view data

# subset the data
APEX.Temp.Exp1 <- hourly.Temp %>%
  filter(days <= 9) # Temp APEX Exp1

APEX.Temp.commongarden <- hourly.Temp %>%
  filter(days >= 10 & days <= 23) # Temp APEX common garden

APEX.Temp.Exp2 <- hourly.Temp %>%
  filter(days >= 24 & days <= 30) # Temp APEX Exp2

# Plot daily averages of Temp data for the complete experiment (continuous APEX data)
APEX.Temp.Exp1$datehour <- as.POSIXct(APEX.Temp.Exp1$datehour, format="%Y-%m-%d %H:%M:%S") #format datehour 
Exp1.Temp.Apex.FIG <- ggplot(APEX.Temp.Exp1, aes(x=datehour, y=mean, group=Treatment)) +#Plot average diurnal cycle of temperature data
                      #geom_line() +
                      geom_point(aes(x = datehour, y = mean, group=Treatment), colour="black", cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                      geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                    position=position_dodge(0.9), data=APEX.Temp.Exp1, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                      ggtitle("D) Exp1") + #Label the graTemp with the main title
                      #scale_x_date(date_minor_breaks = "1 day") +
                      #scale_x_date(breaks = APEX.Temp.Exp1$datehour[seq(1, length(APEX.Temp.Exp1$datehour), by = 24)]) +
                      ylim(14,21) + #Set Y axis limits
                      xlab("Time") + #Label the X Axis
                      ylab("Temp (°C)") + #Label the Y Axis
                      #scale_x_date(date_minor_breaks = "1 day") +
                      theme_bw() + #Set the background color
                      theme(axis.line = element_line(color = 'black'), #Set the axes color
                            axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                            axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                            axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                            panel.grid.major = element_blank(), #Set the major gridlines
                            panel.grid.minor = element_blank(), #Set the minor gridlines
                            plot.background=element_blank(), #Set the plot background
                            panel.border=element_rect(size=1.25, fill = NA), #set outer border
                            plot.title=element_text(hjust=0),
                            legend.position="bottom", #set legend location
                            legend.text = element_text(size = 8), #set the legend text size
                            legend.key = element_blank(), #remove the legend background
                            legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp1.Temp.Apex.FIG  

APEX.Temp.commongarden$datehour <- as.POSIXct(APEX.Temp.commongarden$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
CommGarden.Temp.Apex.FIG <- ggplot(APEX.Temp.commongarden, aes(x=datehour, y=mean, group=Treatment)) +#Plot average diurnal cycle of temperature data
                            #geom_line() +
                            geom_point(aes(x = datehour, y = mean, group=Treatment), colour="black", cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                            geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                            position=position_dodge(0.9), data=APEX.Temp.commongarden, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                            ggtitle("E) Common garden") + #Label the graTemp with the main title
                            #scale_x_date(breaks = APEX.Temp.Exp2$hour[seq(1, length(APEX.Temp.Exp2$hour), by = 24)]) +
                            ylim(14,21) + #Set Y axis limits
                            ylab("Temp (°C)") + #Label the Y Axis
                            xlab("Time") + #Label the X Axis
                            #scale_x_date(date_minor_breaks = "1 day") +
                            theme_bw() + #Set the background color
                            theme(axis.line = element_line(color = 'black'), #Set the axes color
                                  axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                                  axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                                  axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                                  panel.grid.major = element_blank(), #Set the major gridlines
                                  panel.grid.minor = element_blank(), #Set the minor gridlines
                                  plot.background=element_blank(), #Set the plot background
                                  panel.border=element_rect(size=1.25, fill = NA), #set outer border
                                  plot.title=element_text(hjust=0),
                                  legend.position="bottom", #set legend location
                                  legend.text = element_text(size = 8), #set the legend text size
                                  legend.key = element_blank(), #remove the legend background
                                  legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
CommGarden.Temp.Apex.FIG 

APEX.Temp.Exp2$datehour <- as.POSIXct(APEX.Temp.Exp2$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
Exp2.Temp.Apex.FIG <- ggplot(APEX.Temp.Exp2, aes(x=datehour, y=mean, group=Treatment)) +#Plot average diurnal cycle of temperature data
                      #geom_line() +
                      geom_point(aes(x = datehour, y = mean, group=Treatment), colour="black", cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                      geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                      position=position_dodge(0.9), data=APEX.Temp.Exp2, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                      ggtitle("F) Exp2") + #Label the graTemp with the main title
                      #scale_x_date(breaks = APEX.Temp.Exp2$hour[seq(1, length(APEX.Temp.Exp2$hour), by = 24)]) +
                      ylim(14,21) + #Set Y axis limits
                      ylab("Temp (°C)") + #Label the Y Axis
                      xlab("Time") + #Label the X Axis
                      #scale_x_date(date_minor_breaks = "1 day") +
                      theme_bw() + #Set the background color
                      theme(axis.line = element_line(color = 'black'), #Set the axes color
                            axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                            axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                            axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                            panel.grid.major = element_blank(), #Set the major gridlines
                            panel.grid.minor = element_blank(), #Set the minor gridlines
                            plot.background=element_blank(), #Set the plot background
                            panel.border=element_rect(size=1.25, fill = NA), #set outer border
                            plot.title=element_text(hjust=0),
                            legend.position="bottom", #set legend location
                            legend.text = element_text(size = 8), #set the legend text size
                            legend.key = element_blank(), #remove the legend background
                            legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp2.Temp.Apex.FIG 

Supplem.Fig.conical.pH.temp <- grid.arrange(arrangeGrob(Exp1.pH.Apex.FIG, CommGarden.pH.Apex.FIG, Exp2.pH.Apex.FIG, left="Conical pH", ncol=3), 
                                            arrangeGrob(Exp1.Temp.Apex.FIG, CommGarden.Temp.Apex.FIG, Exp2.Temp.Apex.FIG, 
                                                        left="Conical Temperature", ncol=3), ncol=1)
Supplem.Fig.conical.pH.temp # view the figure
###  HEATH TRAY Flow rate data ##################################################################
# NOTE: heath tray pairs were gravity-fed SW from conical overflow (1 conical to every heath tray pair)
# conicals were set to 1 LPM and teed with PVC to target 500 LPM in each heath tray

flow<-read.csv("Data/Flow_rates.csv", header=T, sep=",", na.string="NA", as.is=T) #upload file
flow # view the data

EXP1 <- subset(flow, Exp.num=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
flow_EXP1 <- subset(EXP1, Day!=0) # ommit day 0

EXP2 <- subset(flow, Exp.num=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
flow_EXP2 <- subset(EXP2, Day!=0) # ommit day 0

flow_EXP1_2 <- rbind(flow_EXP1, flow_EXP2) # bind exp1 and 2, day 0 already ommited

# EXP1 
# flow exp by treatment
flow_EXP1.treat <- summarySE(flow_EXP1, measurevar="LPM", groupvars=c("Treatment")) # summary by treatment
flow_EXP1.treat # view table

# EXP2 summary 
# flow by treatment
flow_EXP2.treat <- summarySE(flow_EXP2, measurevar="LPM", groupvars=c("Sec.treat")) # summary by treatment
flow_EXP2.treat # view summary table

# EXP1 AND EXP2 summary 
# flow by treatment
flow_EXP_1_2.treat <- summarySE(flow_EXP1_2, measurevar="LPM", groupvars=c("Treatment")) # summary by treatment
flow_EXP_1_2.treat # view summary table

### HEATH TRAY Discrete Seawater Chemistry Tables #######################################################################

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

chem.exp_1_2 <- rbind(chem.exp1,chem.exp2) # bind exposure 1 and 2

chem.common.garden <-chem.exp[132:156,] # common garden between exposure periods
chem.common.garden$Exposure <- "Common_garden" 
chem.common.garden$tank.name <- substr(chem.common.garden$Tank, start = 10, stop = 13) # new column for tank name without date

# melt - converts wide table to long
chem.long <- melt(chem.exp_1_2, id.vars=c("Date", "Tank", "Treatment", "tank.name", "Exposure")) # uses tidyr to make a long table from wide
garden.chem.long <- melt(chem.common.garden, id.vars=c("Date", "Tank", "Treatment", "tank.name", "Exposure"))

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

#Calculate descriptive stats by Treatment and exposure 
SWC.Treatments <- ddply(chem.long, c("Exposure", "Treatment", "variable"), summarise,
                        N = length(na.omit(value)), #count the sample size removing NA
                        mean = mean(value), #calculate average 
                        sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

#Calculate descriptive stats by Treatment and exposure 
SWC.Treatments.all <- ddply(chem.long, c("Treatment", "variable"), summarise,
                        N = length(na.omit(value)), #count the sample size removing NA
                        mean = mean(value), #calculate average 
                        sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

#Calculate descriptive stats by Treatment and exposure for common garden period
SWC.common.garden <- ddply(garden.chem.long, c("Exposure", "Treatment", "variable"), summarise,
                           N = length(na.omit(value)), #count the sample size removing NA
                           mean = mean(value), #calculate average 
                           sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

#subset chem data for exp 1 and exp 2
Exposure1.chem <-subset(SWC.Treatments, Exposure == "Exp1") #separate out exposure 1
Exposure2.chem <-subset(SWC.Treatments, Exposure == "Exp2") #separate out exposure 2

# create tables for exp 1 , exp2 and all data
Exposure1.long <- reshape(Exposure1.chem, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
Exposure2.long <- reshape(Exposure2.chem, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
ALL.Exposure.long <- reshape(SWC.Treatments.all, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
CommGard.chem <- reshape(SWC.common.garden, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N"))

# write out tables
write.table (Exposure1.long, file="C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Output/Seawater_chemistry_table_Output_exposure1.csv", sep=",", row.names = FALSE) #save data to output file
write.table (Exposure2.long, file="C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Output/Seawater_chemistry_table_Output_exposure2.csv", sep=",", row.names = FALSE) #save data to output file
write.table (ALL.Exposure.long, file="C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Output/Seawater_chemistry_table_Output_exposure_1_2.csv", sep=",", row.names = FALSE) #save data to output file
write.table (CommGard.chem, file="C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Output/Seawater_chemistry_table_common_garden.csv", sep=",", row.names = FALSE) #save data to output file

### Respiration Data - Analysis, Graphs, Models  (summarized analysis from Stats_resp_analysis.R)#############

#Load PreExposure Respiraiton Data before the exposures ("Day 0") 
respPreExposure<-read.csv("Data/PreExposure_resp_calc_and_standardized.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(respPreExposure) # view the names of the data

# plot the PreExposure respiration rate 
resp_PreExposure_plot <- ggplot(respPreExposure, aes(x = factor(respPreExposure$Date), y = respPreExposure$LpcResp_alpha0.4_all)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(size = 2, shape = 21) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_PreExposure_plot + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                            x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \ PreExposure"))

#Load Respiraiton Data for exposure 1 and 2
resp<-read.csv("Data/All_resp_calc_and_standardized.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(resp) # view  names in the dataframe 

# seperate into experiment 1 and 2 for the 10-day and 6-day OA exposure
resp_EXP1 <- subset(resp, EXP.numb=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
resp_EXP2 <- subset(resp, EXP.numb=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
resp_EXP2.0 <- subset(resp, EXP.numb=="EXP2") # same under a diff name (read why on next comment)
resp_EXP2.0 <-subset(resp_EXP2.0, Day!=0) # omit Day0 of the second exposure for graph on cumulative exposure to OA 

### EXP1 ####
# The following are condenced steps to merge a reproducible respiration rates from LoLin R ouputs.
# Test strength of automated LoLin Script for nine constants each with the "Reference" data
# Reference = each resp value computed with a visial criteria and default constants  (alpha = 0.2, untruncated); adjusted to acheive peak emp distribution  of regressions
# Automated ouputs = at nine total settings of alpha= 0.2,0.4 and0.6 at three different truncations (all, 10-20 minutes, 10-25 minutes)

# AUTOMATED RESP RATES WITH "LOLIN" PACKAGE OUTPUTS
# First step - nine LoLin outputs to reference; choose constants with least respiration values outside of 95% Loess CI with Reference
# linear regression with reference plot
plot(resp_EXP1[,1],resp_EXP1[,5], main= "Ref vs. alpha0.4_all") # linear regression with the reference 
summary(lm(resp_EXP1[,1]~resp_EXP1[,5])) #Adjusted R-squared:  0.3905

# label with row numbers  - shows values outside of CI interval in loess curve
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) 
# outside of loess curve = 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94

# call all points outside of the CI and create a new dataframe with these values and all non LoLin outputs
newdata_resp_EXP1_ALL <- data.frame(resp_EXP1$Date, resp_EXP1[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP1_ALL_1 <-newdata_resp_EXP1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ] # only call points (rows) outside loess in first regression

# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,10]) # strongest correlation was with alpha = 0.6 truncation at 10-20 minutes 
summary(lm(newdata_resp_EXP1_ALL_1[,10]~newdata_resp_EXP1_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7235 

#Merge a FINAL COLUMN as "Finalresp" for both datasets both datasets 
resp_EXP1$FINALresp <- resp_EXP1$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation 
resp_EXP1$FINALresp[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] <- resp_EXP1$LpcResp_alpha0.6_min10.20.csv[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] #inserted new rows from alpha = 0.6 10-20 min for points outside loess curve

# test correlation of Finalresp with the Ref
plot(resp_EXP1$Resp_individually_all.csv,resp_EXP1$FINALresp, main= "Ref vs. FINALresp") # plot the relationship
summary(lm(resp_EXP1[,1]~resp_EXP1$FINALresp)) # summarize linear model - Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# TABLES
# Now that we have Finalresp, overall mean SD SMR in EXP1
# Exp1.table <- do.call(data.frame,aggregate(FINALresp ~ Day*Init.treat, data = resp_EXP1, function(x) c(mean = mean(x), se = std.error(x)))) #mean and st. error table
# #...other tables to view the data
# exp1_resp_all <- do.call(data.frame,aggregate(FINALresp ~ Treat1_Treat2*Date, data = resp_EXP1, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by date EXP1
# exp1_resp_summary_all <- summarySE(exp1_resp_all, measurevar="FINALresp.mean", groupvars=c("Treat1_Treat2")) # SMR by treatment EXP1
# exp1_resp_overall <- summarySE(exp1_resp_all, measurevar="FINALresp.mean") # overall SMR EXP1
# resp_EXP2.0.T<- merge(resp_EXP1, resp_EXP2, by=c("tank")) # merge combined treatments from EXP2
# exp1_resp_4.T <- do.call(data.frame,aggregate(FINALresp ~ Treat1_Treat2.y*Date.x, data = resp_EXP2.0.T, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by date EXP1 and 4 treatments in EXP2
# exp1_resp_summary.T <- summarySE(exp1_resp_4.T, measurevar="FINALresp.mean", groupvars=c(" Treat1_Treat2.y")) # SMR by four treatments EXP1xEXP2

# PLOTS
#treatments and time (with PreExposure added)
colnames(respPreExposure)[1] <- "FINALresp" # rename the calc resp values in PreExposure table to match the resp_EXP1
respPreExposure$Init.treat <- c("Ambient") # name the treatement as ambient for the preexposure "day 0" data
respPreExposure$Day <- 0 # zero dafults correctly plotted position - renamed in ggplot script at "prebasal"
resp_EXP1_condensed <- resp_EXP1[,c(11,12,13,14,15,17,21)] # call the columns in respPreExposure to rbind (must be the same)
resp_EXP1_ALL <- rbind(resp_EXP1_condensed, respPreExposure) # merge the two tables for the graph

#plot by exp 1 by treatment
Exp1.Fig.resp.A <- ggplot(resp_EXP1_ALL, aes(x = factor(resp_EXP1_ALL$Day), 
                                            y = resp_EXP1_ALL$FINALresp, 
                                                            fill = resp_EXP1_ALL$Init.treat)) +
                  scale_fill_manual(values=c("white","gray1"), 
                    labels=c("Ambient","Elevated")) +
                  geom_boxplot(alpha = 0.5, # color hue
                  width=0.6, # boxplot width
                  outlier.size=0,
                  position = position_dodge(preserve = "single")) +
                  geom_point(pch = 19, position = position_jitterdodge(0.01), size=1) +
                  theme_classic() +
                  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                  theme(legend.position = c(0.55,0.9),legend.direction="horizontal", legend.title=element_blank(), 
                        axis.line = element_line(color = 'black'), #Set the axes color
                        axis.ticks.length=unit(0.2, "cm")) + #turn ticks inward
                  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                  scale_x_discrete(labels = c("0",2,5,8,10))    +
                  labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*mm^{-1})), x=expression("Days"),fill= "")  
Exp1.Fig.resp.B <- Exp1.Fig.resp.A + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                            axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                            axis.line = element_line(color = 'black'),
                            axis.title.x = element_text(size = 14),
                            axis.title.y = element_text(size = 14))
Exp1.Fig.resp_FINAL <-  Exp1.Fig.resp.B + scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0))
Exp1.Fig.resp_FINAL

# ANALYSIS
# Two-Way anova for respiration rate under Initial OA Exposure
resp_EXP1$Day <- as.character(resp_EXP1$Day) # convert day to character 
EXP1.resp.aov.mod <- aov(FINALresp ~ Init.treat * Day, data = resp_EXP1) # run anova on treatment and time
anova(EXP1.resp.aov.mod) # significant effect of  treatment 
# Levene's test for homogeneity 
leveneTest(EXP1.resp.aov.mod) # p 0.2236
# Shapiro test
EXP1.resp.mod.residuals <- residuals(object = EXP1.resp.aov.mod) # call residuals from the model
shapiro.test(x = EXP1.resp.mod.residuals) #  0.09055
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.resp.aov.mod)) #plot histogram of residuals
shapiro.test(residuals(EXP1.resp.aov.mod)) # residuals are normal
boxplot(residuals(EXP1.resp.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP1.resp.aov.mod),residuals(EXP1.resp.aov.mod)) 
# explore the effect
#summary tables to calculate the percent difference between resp in treatments
sumresp_EXP1 <- summarySE(resp_EXP1, 
                          measurevar="FINALresp", 
                          groupvars=c("Date","Init.treat")) # summary table of resp with Date and treatment
sumresp_EXP1_means <- summarySE(sumresp_EXP1, 
                                measurevar="FINALresp", 
                                groupvars=c("Init.treat")) # summarize previous table for overall treatment 

percentdiff <- ((sumresp_EXP1_means[1,3] - sumresp_EXP1_means[2,3])/sumresp_EXP1_means[1,3])*100 # calculate percent difference
percentdiff # 25% lower respiration rates in low pH

### EXP2 ####
par(mfrow=c(1,1)) #set plotting configuration

# AUTOMATED RESP RATES WITH "LOLIN" PACKAGE OUTPUTS
# First step - nine LoLin outputs to reference; choose constants with least respiration values outside of 95% Loess CI with Reference
# Below  alpha = 0.4, no truncation for automated ouput - yielded the least respiration values outside of Loess CI with Reference
plot(resp_EXP2[,1],resp_EXP2[,5], main= "Ref vs. alpha0.4_all") # linear regression with the reference 
summary(lm(resp_EXP2[,1]~resp_EXP2[,5])) #Adjusted R-squared:  0.8002 

# label with row numbers  - shows values outside of CI interval in loess curve
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) +
          geom_point() +
          geom_smooth(method = 'loess') +
          geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01)) 
# outside of loess curve = 114, 153,155,156,150,141,105,130,152,98,188
tail(resp_EXP1) # end of exposure 1 is row #96
numberID <- (c(114,153,155,156,150,141,105,130,152,98,188)) - 96 #subtract by 96 to get the actual row number in resp_exp2
# actuall row numbers outside loess cruve = 18 57 59 60 54 45  9 34 56  2 92

# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
newdata_resp_EXP2_ALL <- data.frame(resp_EXP2$Date , resp_EXP2[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP2_ALL_1 <-newdata_resp_EXP2_ALL[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), ]  # only call points (rows) outside loess in first regression

# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,11]) # alpha = 0.6 truncation at 10-25 minutes (each resp trial was 30 mins)
summary(lm(newdata_resp_EXP2_ALL_1[,11]~newdata_resp_EXP2_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7169 

#Merge a FINAL COLUMN as "Finalresp" for both datasets both datasets 
resp_EXP2$FINALresp <- resp_EXP2$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation
resp_EXP2$FINALresp[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92)] <- resp_EXP2$LpcResp_alpha0.6_min10.25.csv[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92)] #inserted new rows from alpha = 0.6 10-25 min for points outside loess curve

# test relationship between Finalresp and the Ref
plot(resp_EXP2$FINALresp,resp_EXP2$Resp_individually_all.csv, main= "Ref vs. FINALresp")
summary(lm(resp_EXP2$Resp_individually_all.csv~resp_EXP2$FINALresp)) # summarize linear model - Multiple R-squared:  0.9304,	Adjusted R-squared:  0.9297 
ggplot(resp_EXP2, aes(x = resp_EXP2$Resp_individually_all.csv, y = resp_EXP2$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# overall Resp in EXP2
resp_EXP2_2.4.6. <- subset(resp_EXP2, Day!=0) # subset out pre-exposure measurements on day 0 (day 24 of experiment)
resp_EXP2_d0 <- subset(resp_EXP2, Day==0) # subset of just day 0 data (for the plot)
# exp2_resp_all <- do.call(data.frame,aggregate(FINALresp ~ Treat1_Treat2*Date, data = resp_EXP2_2.4.6., function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by init*sec treatments date EXP2
# exp2_resp_summary_all <- summarySE(exp2_resp_all, measurevar="FINALresp.mean", groupvars=c("Treat1_Treat2")) # SMR by init*sec  treatments EXP2
# exp2_resp_all.2 <- do.call(data.frame,aggregate(FINALresp ~ Sec.treat*Date, data = resp_EXP2_2.4.6., function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by sec treatments date EXP2
# exp2_resp_summary_all.2 <- summarySE(exp2_resp_all.2, measurevar="FINALresp.mean", groupvars=c("Sec.treat")) # resp rate by sec  treatments EXP2
# exp2_resp_overall <- summarySE(exp2_resp_all, measurevar="FINALresp.mean") # overall resp rate EXP2

# PLOTTING
resp_EXP2_d0$Treat1_Treat2 <- resp_EXP2_d0$Init.treat # Day 0 in Exp2 still has potential carry over from 2 treatments in intial exp, not 4 treatments in Exp2
resp_EXP2_merge <- rbind(resp_EXP2_d0, resp_EXP2_2.4.6.) # merge the two tables for the graph
#plot treatments and time (PreExposure added as day 0 data)
Exp2.Fig.resp.A <- ggplot(resp_EXP2_merge, aes(x = factor(resp_EXP2_merge$Day), y = resp_EXP2_merge$FINALresp, fill = resp_EXP2_merge$Treat1_Treat2)) +
                  scale_fill_manual(values=c("white", "white", "grey80", "gray1", "gray50", "gray1"), 
                      labels=c("Ambient","Ambient × Ambient","Ambient × Elevated","Elevated","Elevated × Ambient","Elevated × Elevated")) +
                  geom_boxplot(alpha = 0.5, # color hue
                               width=0.6, # boxplot width
                               outlier.size=0, # make outliers small
                               position = position_dodge(preserve = "single")) + 
                  geom_point(pch = 19, position = position_jitterdodge(0.01), size=1) +
                  theme_classic() + ylim(0, 0.6) +
                  #geom_point(aes(fill = resp_EXP2$Treat1_Treat2), size = 1.5, shape = 21, position = position_jitterdodge(0.05)) +
                  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
                               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
                  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                  scale_x_discrete(labels = c("0",2,4,6)) +
                  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                  labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*indiv^{-1})), x=expression("Days"), fill="") 
Exp2.Fig.resp.A # view the plot
Exp2.Fig.resp.B <- 
                  Exp2.Fig.resp.A + 
                  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                        axis.line = element_line(color = 'black'),
                        axis.ticks.length=unit(0.2, "cm"),
                        axis.title.x = element_text(size = 14),
                        axis.title.y = element_text(size = 14)) +
                  scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0))
Exp2.Fig.resp.B # view the plot

# ANALYSIS
# Three-Way anova for respiration rate under Secondary OA Exposure
resp_EXP2_2.4.6.$Day <- as.character(resp_EXP2_2.4.6.$Day) # covert day to character 
EXP2.resp.aov.mod <- aov(FINALresp ~ Init.treat*Sec.treat*Day, data = resp_EXP2_2.4.6.) # run anova on treatment and time
anova(EXP2.resp.aov.mod) # significant effect from day and marginal effect from secondary treatment
# Levene's test for homogeneity 
leveneTest(EXP2.resp.aov.mod) # p 0.4541
# Shapiro test
EXP2.resp.mod.residuals <- residuals(object = EXP2.resp.aov.mod) # call residuals from the model
shapiro.test(x = EXP2.resp.mod.residuals) # 0.005096
#post-hoc 
#effect of day (time)
exp2.resp.ph.day <- lsmeans(EXP2.resp.aov.mod, pairwise ~ Day)# pariwise Tukey Post-hoc test between repeated treatments
exp2.resp.ph.day # view post hoc summary
E2.pairs.RESP.day.05 <- cld(exp2.resp.ph.day, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.RESP.day.05 #view results
E2.pairs.RESP.day.1 <- cld(exp2.resp.ph.day, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.RESP.day.1 #view results
#effect of secondary treatment 
exp2.resp.ph.sec <- lsmeans(EXP2.resp.aov.mod, pairwise ~ Sec.treat)# pariwise Tukey Post-hoc test between repeated treatments
exp2.resp.ph.sec # view post hoc summary
E2.pairs.RESP.sec.05 <- cld(exp2.resp.ph.sec, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.RESP.sec.05 #view results
E2.pairs.RESP.sec.1 <- cld(exp2.resp.ph.sec, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.RESP.sec.1 #view results

# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.resp.aov.mod)) #plot histogram of residuals
shapiro.test(residuals(EXP2.resp.aov.mod)) # residuals are normal
boxplot(residuals(EXP2.resp.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP2.resp.aov.mod),residuals(EXP2.resp.aov.mod)) 

# t-test for differences at the end of exp1 (day 10) and start of exp 2 (day 0) by treatment
exp1_d10.resp  <- subset(resp_EXP1, Date=="20180724") # starting resp  on Day 10 in Exp 1
t.test(exp1_d10.resp$FINALresp~exp1_d10.resp$Init.treat) # p-value = 0.08471; t-test shows no difference between treatment at the start of Exp2

exp2_d0.resp <- subset(resp_EXP2, Date=="20180807") # starting resp  on Day 0 in Exp 2
t.test(exp2_d0.resp$FINALresp~exp2_d0.resp$Init.treat) # p-value = 0.01469; t-test shows significant difference between treatment at the start of Exp2

#ADD MODEL RESULTS TO THE PLOT
Exp2.Fig.resp_FINAL <- 
  Exp2.Fig.resp.B + 
  geom_segment(aes(x = 1.6, y = 0.55, xend = 2.4, yend = 0.55)) +
  geom_segment(aes(x = 2.6, y = 0.55, xend = 3.4, yend = 0.55)) + 
  geom_segment(aes(x = 3.6, y = 0.55, xend = 4.4, yend = 0.55)) +
  geom_segment(aes(x = .6, y = 0.55, xend = 1.4, yend = 0.55)) +
  annotate("text", x="0", y=0.54, label = "**", size = 4) + # t-test with p < 0.05 between treatments at day 0
  annotate("text", x="2", y=0.54, label = "*a", size = 4) + # add text to the graphic for posthoc letters - effect of time
  annotate("text", x="4", y=0.54, label = "*ab", size = 4) + # add text to the graphic for posthoc letters - effect of time
  annotate("text", x="6", y=0.54, label = "*b", size = 4) # add text to the graphic for posthoc letters - effect of time
Exp2.Fig.resp_FINAL

# TEST FOR EFFECTS OF THE FOUR TREATMENT GROUPS  FOR ALL DAYS
# anovas for signifcant between treatments for all days in EXP2 
Resp_EXP2_d2 <- subset(resp_EXP2, Day==2)
Resp_d2_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_d2)
summary(Resp_d2_EXP2) # day 2 no difference between treatments

Resp_EXP2_d4 <- subset(resp_EXP2, Day==4)
Resp_d4_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_d4)
summary(Resp_d4_EXP2) # day4 no difference between treatments

Resp_EXP2_d6 <- subset(resp_EXP2, Day==6)
Resp_d6_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_d6)
summary(Resp_d6_EXP2) # day 6 no difference between treatments

Resp_EXP2_ALL <- subset(resp_EXP2, Day!=0)
Resp_alldays_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_ALL)
summary(Resp_alldays_EXP2) # ALL days no difference between treatments

##### PERCENT DIFF BARPLOTS
# #barplots for mean SD exp1 - exp2
# exp1_resp_summary.T # exp 1 table grouped by intit*sec treatments
# exp1_resp_summary.T$trial <- "initial" # add column for trial to merge
# colnames(exp1_resp_summary.T)[1] <- "Treat1_Treat2" # to match and merge with eXP2
# exp2_resp_summary_all # exp 2 table grouped by initiat*sec treatments
# exp2_resp_summary_all$trial <- "secondary" # add column for trial to merge
# Table_EXP_1_2 <- rbind(exp1_resp_summary.T, exp2_resp_summary_all) # bind rows by treatment column
# Table_EXP_1_2 # view table
# 
# #  mean percent difference between exposure periods
# percent_av_resp_exposures <- Table_EXP_1_2                %>% # view the table
#   select(trial, FINALresp.mean) %>% # select desired data
#   group_by(trial)               %>% # group by exposure period 'trial'
#   dplyr::summarise(mean = mean(FINALresp.mean), # summary table
#                    min = min(FINALresp.mean),
#                    sd = sd(FINALresp.mean),
#                    SEM = (sd/sqrt(n())),
#                    count =n())                   %>%
#   mutate(pct.chg = (100*((mean[2] - (mean[1]))/(mean[1])))) # calculate the percent change 
# percent_av_resp_exposures # view summary table 
# 
# #  percent difference between treatment
# percent_av_resp <- Table_EXP_1_2            %>% # view the table
#   group_by(Treat1_Treat2) %>%
#   spread(trial, FINALresp.mean)             %>% # create a horizonatal table for the mean resp in initial (n = 4 per treat) and secondary (n=3 per treat)
#   select(Treat1_Treat2, initial, secondary) %>% # select only the treatments and the mean resp rates
#   mutate(secondary=lag(secondary))          %>% # shift the secondary down to align with
#   na.omit()                                 %>% # omit na 
#   mutate(pct.chg = (100*((secondary - (initial))/(initial)))) # calculate the percent change from (secondary - initial / secondary) *100
# 
# percent_av_resp # view the table

### Growth Data - Analysis, Graphs, Models  (summarized analysis from Stats_growth_analysis.R)#######################################

#Load Size Data
size<-read.csv("Data/All_growth_data.csv", header=T, sep=",", na.string="NA", as.is=T) 

size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals
# CREATE DATASETS FOR EXPOSURES 1 AND 2
size_EXP1_with_PreExposure <- subset(size, Exp.Num=="Exp1") # all Exp1 data
size_EXP1 <- subset(size_EXP1_with_PreExposure, trial!="prebasal") # Exp1 without Day 0

size_EXP1_PreExposure <- subset(size_EXP1_with_PreExposure, Day=="prebasal") # Seperate Day 0 of Exp 1 for Figure
size_EXP1_PreExposure$Day <- "0" # name day as 0 (before it was "prebasal")
size_EXP1_all <- rbind(size_EXP1_PreExposure, size_EXP1) # merge two datasets for the first size figure Exp 1

size_EXP2 <- subset(size, Exp.Num=="Exp2") # all Exp 2
size_EXP2.d0 <- subset(size_EXP2, Day==0)
size_EXP2.d0$Treat1_Treat2 <- size_EXP2.d0$Init.Trt # merge column or initial treat as treat1_treat2 for Exp2 figure
size_EXP2.0 <-subset(size_EXP2, Day!=0) # Exp 2 without day 0 
size_EXP2_all <- rbind(size_EXP2.d0, size_EXP2.0) # merge two datasets for the first size figure Exp 2
size_Exp1.T<- merge(size_EXP1, size_EXP2, by=c("tank"))  #merge to obtained combined treatments from EXP2

inital_size <- subset(size_EXP1, Date=="20180716") # get starting size of indiivduals from first measurements
StartSize <- summarySE(inital_size, measurevar="shell_size", groupvars=c("Date")) #summary table for starting shell length = 5.077962 ± 0.6622871 (mean ± SD)
end_size <- subset(size_EXP1, Date=="20180724") # view the efinal size on Day 10 of exposure 1

# # overall shell size in EXP1 
# exp1_size_all <- do.call(data.frame,aggregate(shell_size ~ treatment*Date, data = size_EXP1, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by  treatments date EXP1
# exp1_size_summary_all <- summarySE(exp1_size_all, measurevar="shell_size.mean", groupvars=c("treatment")) # shell length by  treatments EXP1
# exp1_size_overall <- summarySE(exp1_size_all, measurevar="shell_size.mean") # overall SMR EXP2
# exp1_size_4.treatments <- do.call(data.frame,aggregate(shell_size.x ~ treatment.y*Date.x, data = size_Exp1.T, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by  treatments date EXP1
# exp1_size_summary_4.treatments <- summarySE(exp1_size_4.treatments, measurevar="shell_size.x.mean", groupvars=c("treatment.y")) # shell length by  treatments EXP1
# # overall shell size in EXP2
# exp2_size_all <- do.call(data.frame,aggregate(shell_size ~ treatment*Date, data = size_EXP2, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by init*sec treatments date EXP2
# exp2_size_summary_all <- summarySE(exp2_size_all, measurevar="shell_size.mean", groupvars=c("treatment")) # shell length by init*sec  treatments EXP2
# exp2_size_all.2 <- do.call(data.frame,aggregate(shell_size ~ Sec.Trt*Date, data = size_EXP2, function(x) c(mean = mean(x), sd = sd(x)))) # mean SD table by sec treatments date EXP2
# exp2_size_summary_all.2 <- summarySE(exp2_size_all.2, measurevar="shell_size.mean", groupvars=c("Sec.Trt")) # shell length by sec  treatments EXP2
# exp2_size_overall <- summarySE(exp2_size_all, measurevar="shell_size.mean") # overall SMR EXP2

# plot end of exp 1 Day 10 with treatments 
length_EXP1_Day10 <- ggplot(end_size, aes(x = treatment, y = shell_size, fill = treatment)) +
      geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
      ylim(2,8.2) +
      geom_point(pch = 19, position = position_jitterdodge(0.1), size=1) +
      #geom_point(aes(fill = treatment), size = 0.5, shape = 21, position = position_jitterdodge(0.05)) +
      stat_summary(fun.y=mean, geom="point", pch="+", size=3, position = position_jitterdodge(0.01)) +
      theme(legend.position = c(.92, .9), legend.text=element_text(size=8)) +
      labs(y="shell length", x = "Treatment", fill="") 
length_EXP1_Day10

### EXP1 ####
#plot treatments and time (with prePreExposure)
Exp1.Fig.size.A <- ggplot(size_EXP1_all, aes(x = factor(Day), y = shell_size, fill = treatment)) +
                  theme_classic() +
                  scale_fill_manual(values=c("white", "grey3"), labels=c("Ambient","Elevated")) +
                  geom_boxplot(alpha = 0.5, # color hue
                               width=0.6, # boxplot width
                               outlier.size=0, # make outliers small
                               position = position_dodge(preserve = "single")) + 
                  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
                  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
                    width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
                  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                  ylim(2,8.2) + 
                  scale_x_discrete(limits=c("0",2,5,8,10)) +
                  labs(y=expression("Shell length"~(mm)), x=expression("Days"))
Exp1.Fig.size.B <- Exp1.Fig.size.A + 
                  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                  axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                  axis.line = element_line(color = 'black'),
                  axis.ticks.length=unit(0.2, "cm"),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14)) +
                  scale_y_continuous(limits = c(2, 8), expand = c(0, 0))
Exp1.Fig.size.B # view the plot

# Two-Way anova for shell size under Initial OA Exposure
EXP1.size.aov.mod <- aov(shell_size ~ Init.Trt * Day, data = size_EXP1) # run anova on treatment and time
anova(EXP1.size.aov.mod) # significant effect of time; no effect from treatment
# Levene's test for homogeneity 
leveneTest(EXP1.size.aov.mod) # p 0.5609
# post-hoc
exp1.size.ph <- lsmeans(EXP1.size.aov.mod, pairwise ~  Day)# pariwise Tukey Post-hoc test between repeated treatments
exp1.size.ph # view post hoc summary
E1.pairs.SIZE.05 <- cld(exp1.size.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.SIZE.05 #view results
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.size.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP1.size.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP1.size.aov.mod),residuals(EXP1.size.aov.mod)) 

Exp1.Fig.size_FINAL <- 
  Exp1.Fig.size.B +
  geom_segment(aes(x = 1.6, y = 7.3, xend = 2.4, yend = 7.3)) +
  geom_segment(aes(x = 2.6, y = 7.3, xend = 3.4, yend = 7.3)) + 
  geom_segment(aes(x = 3.6, y = 7.3, xend = 4.4, yend = 7.3)) +
  geom_segment(aes(x = 4.6, y = 7.3, xend = 5.4, yend = 7.3)) +
  annotate("text", x="2", y=7.2, label = "a", size = 4) + # t-test with p < 0.05 between treatments at day 0
  annotate("text", x="5", y=7.2, label = "ab", size = 4) + # add text to the graphic for posthoc letters - effect of time
  annotate("text", x="8", y=7.2, label = "ab", size = 4) + # add text to the graphic for posthoc letters - effect of time
  annotate("text", x="10", y=7.2, label = "b", size = 4) # add text to the graphic for posthoc letters - effect of time
Exp1.Fig.size_FINAL
  
### EXP2 ####
#PLOTTING
Exp2.Fig.size.A <- ggplot(size_EXP2_all, aes(x = factor(Day), y = shell_size, fill = Treat1_Treat2)) +
                  theme_classic() +
                  scale_fill_manual(values=c("white", "white", "grey80", "grey3", "gray50", "grey3"), 
                                      labels=c("Ambient","Ambient × Ambient","Ambient × Elevated","Elevated","Elevated × Ambient","Elevated × Elevated")) +
                  geom_boxplot(alpha = 0.5, # color hue
                               width=0.6, # boxplot width
                               outlier.size=0, # make outliers small
                               position = position_dodge(preserve = "single")) + 
                  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
                  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
                               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
                  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                  ylim(2,8.2) + 
                  scale_x_discrete(labels = c("0",2,4,6)) +
                  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) + 
                  labs(y=expression("Shell size"~(mm)), x=expression("Days"))
Exp2.Fig.size.FINAL <- Exp2.Fig.size.A  + 
                  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                        axis.line = element_line(color = 'black'),
                        axis.ticks.length=unit(0.2, "cm"),
                        axis.title.x = element_text(size = 14),
                        axis.title.y = element_text(size = 14)) +
                  scale_y_continuous(limits = c(2, 8), expand = c(0, 0))
Exp2.Fig.size.FINAL # view the plot

# ANALYSIS
# t-test for differences at the end of exp1 (day 10) and start of exp 2 (day 0) by treatment
exp1_d10.size <- subset(size_EXP1, Date=="20180724") # starting size on Day 10 in Exp 1
t.test(exp1_d10.size$shell_size~exp1_d10.size$Init.Trt) # p-value = 0.6083; t-test shows no difference between treatment at the start of Exp2

exp2_d0.size <- subset(size_EXP2, Date=="20180807") # starting size on Day 0 in Exp 2
t.test(exp2_d0.size$shell_size~exp2_d0.size$Init.Trt) # p-value = 0.2531; t-test shows significant difference between treatment at the start of Exp2

# Three-Way anova for shell size under Secondary OA Exposure
EXP2.size.aov.mod <- aov(shell_size ~ Init.Trt*Sec.Trt* Day, data = size_EXP2.0) # run anova on treatment and time
anova(EXP2.size.aov.mod) # significant effect fromboth inital and secondary treatment
# Levene's test for homogeneity 
leveneTest(EXP2.size.aov.mod) # p 0.8418
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.size.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP2.size.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP2.size.aov.mod),residuals(EXP2.size.aov.mod))
# post-hoc
exp2.size.ph <- lsmeans(EXP2.size.aov.mod, pairwise ~  Init.Trt*Sec.Trt* Day)# pariwise Tukey Post-hoc test between repeated treatments
exp2.size.ph # view post hoc summary
E2.pairs.SIZE.05 <- cld(exp2.size.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.SIZE.05 #view results
# effect of initial exposure
exp2.size.ph.initial <- lsmeans(EXP2.size.aov.mod, pairwise ~  Init.Trt)# pariwise Tukey Post-hoc test between repeated treatments
exp2.size.ph.initial # view post hoc summary
E2.pairs.SIZE.05.initial <- cld(exp2.size.ph.initial, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.SIZE.05.initial #view results
# effect of initial exposure
exp2.size.ph.sec <- lsmeans(EXP2.size.aov.mod, pairwise ~  Sec.Trt)# pariwise Tukey Post-hoc test between repeated treatments
exp2.size.ph.sec  # view post hoc summary
E2.pairs.SIZE.05.sec <- cld(exp2.size.ph.sec, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.SIZE.05.sec #view results

# #  mean percent difference between exposure periods
# percent_av_size_exposures <- SIZETable_EXP_1_2            %>% # view the table
#                             select(trial, shell_size.mean  ) %>% # select desired data
#                             group_by(trial)               %>% # group by exposure period 'trial'
#                             dplyr::summarise(mean = mean(shell_size.mean), # summary table
#                             min = min(shell_size.mean),
#                             sd = sd(shell_size.mean),
#                             SEM = (sd/sqrt(n())),
#                             count =n())                   %>%
#                             mutate(pct.chg = (100*((mean[2] - (mean[1]))/(mean[1])))) # calculate the percent change 
# percent_av_size_exposures # view summary table 
# # percent difference between treatment
# percent_av_size <- SIZETable_EXP_1_2        %>% # view the table
#                   group_by(treatment)                       %>%
#                   spread(trial, shell_size.mean)            %>% # create a horizonatal table for the mean size in initial (n = 4 per treat) and secondary (n=4 per treat)
#                   select(treatment, initial, secondary)    %>% # select only the treatments and the mean shell size  
#                   mutate(secondary=lag(secondary))          %>% # shift the secondary down to align with
#                   na.omit()                                 %>% # omit na 
#                   mutate(pct.chg = (100*((secondary - (initial))/(initial)))) # calculate the percent change from (secondary - initial / secondary) *100
# percent_av_size  # view the table

### Ouput ####

figure_1 <- ggarrange(Exp1.Fig.resp_FINAL, Exp2.Fig.resp_FINAL,
                                 ncol = 1, nrow = 2)
figure_1 # view the figure

figure_2 <- ggarrange(Exp1.Fig.size_FINAL, Exp2.Fig.size.FINAL,
                      ncol = 1, nrow = 2)
figure_2 # view the figure

# Saving output plots
ggsave(file="Output/Supplem.Fig.conical.pH.temp.pdf", Supplem.Fig.conical.pH.temp, width = 12, height = 8, units = c("in"))
ggsave(file="Output/Output_Figure_1.pdf", figure_1, width = 12, height = 8, units = c("in"))
ggsave(file="Output/Output_Figure_2.pdf", figure_2, width = 12, height = 8, units = c("in"))

