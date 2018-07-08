#Title: Respiration Calculations
#Author: HM Putnam
#Edited by: HM Putnam
#Date Last Modified: 20180627
#See Readme file for details

rm(list=ls()) #clears workspace 

## install packages if you dont already have them in your library
if ("devtools" %in% rownames(installed.packages()) == 'FALSE') install.packages('devtools') 
library(devtools)
if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install_github('colin-olito/LoLinR') 
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron') 
if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 
if ("dplyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('dplyr') 


#Read in required libraries
##### Include Versions of libraries
#install_github('colin-olito/LoLinR')
library("ggplot2")
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('plyr')
library('dplyr')

#Required Data files

# Set Working Directory:
setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working

##### Respiration #####
path.p<-"Data/Respirometry/" #the location of all your respirometry files 

# bring in the respiration files
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders
#basename above removes the subdirectory name from the file
#add file names that include the subdirectory name
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) #list all csv file names in the folder and subfolders

#generate a 3 column dataframe with specific column names
Resp.R <- data.frame(matrix(NA, nrow=length(file.names)*2, ncol=5))
colnames(Resp.R) <- c("Sample.ID","Intercept", "umol.L.sec","Temp.C","PR")

#Load Sample Info
Sample.Info <- read.csv(file="../Data/MetaData/Adult_Sample_Info.csv", header=T) #read sample.info data
#includes treatment, tank, chamber volume, animal size etc for normalization

#take the average of the two volumes measures for each sample
Chamber.Vol.mL<-aggregate(Sample.Info$Volume~Sample.Info$Sample.ID, FUN=mean)
colnames(Chamber.Vol.mL)<-c("Sample.ID","Volume")

for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Resp.Data1 <-read.table(file.path(path.p,file.names[i]), skip = 1, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data1  <- Resp.Data1[,c(2,9,16)] #subset columns of interest
  Resp.Data1$Time <- as.POSIXct(Resp.Data1$Time,format="%H:%M:%S", tz = "") #convert time from character to time

  Resp.Data   <-  thinData(Resp.Data , by=20)$newData1 #thin data by every 10 points
  Resp.Data $sec <- as.numeric(rownames(Resp.Data )) #maintain numeric values for time
  plot(Value ~ sec, data=Resp.Data , xlab='Time (seconds)', ylab=substitute(' O'[2]~' (µmol/L)'), type='n', axes=FALSE) #plot thinned data
  usr  <-  par('usr')
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
  whiteGrid()
  box()
  points(Resp.Data $Value ~ Resp.Data $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1)
  axis(2, las=1)
  dev.off()
  
  Regs  <-  rankLocReg(xall=Resp.Data $sec, yall=Resp.Data $Value, alpha=0.3, 
                       method="pc", verbose=TRUE) 
  pdf(paste0("~/Desktop/Data/20170822/T4_output",rename,"_",j,"regression.pdf"))
  plot(Regs)
  dev.off()
}

