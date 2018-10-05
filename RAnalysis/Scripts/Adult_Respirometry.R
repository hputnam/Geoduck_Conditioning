#Title: Respiration Calculations
#Author: HM Putnam
#Edited by: HM Putnam
#Date Last Modified: 20181005
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
path.p<-"Data/Respirometry/Adult/" #the location of all your respirometry files 

# bring in the respiration files
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders
#basename above removes the subdirectory name from the file
#add file names that include the subdirectory name
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) #list all csv file names in the folder and subfolders

#generate a 3 column dataframe with specific column names
Resp.R <- data.frame(matrix(NA, nrow=length(file.names), ncol=4))
colnames(Resp.R) <- c("Sample.ID","Intercept", "umol.L.sec","Temp.C")

#Load Sample Info
Sample.Info <- read.csv(file="Data/MetaData/Adult_Sample_Info.csv", header=T) #read sample.info data
#includes treatment, tank, chamber volume, animal size etc for normalization


for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  thin=30
  a=0.4
  rename <- sub("_.*", "", file.names[i])
  Resp.Data <-read.table(file.path(path.p,file.names[i]), header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data.orig  <- Resp.Data[,c(2,9,16)] #subset columns of interest
  Resp.Data.orig$Time <- as.POSIXct(Resp.Data$Time,format="%H:%M:%S", tz = "") #convert time from character to time
  Resp.Data   <-  thinData(Resp.Data.orig, by=thin)$newData1 #thin data 
  Resp.Data$sec <- as.numeric(rownames(Resp.Data )) #maintain numeric values for time
  Resp.Data$Temp <- NA # add a new column to fill with the thinned data
  Resp.Data$Temp <-  thinData(Resp.Data.orig, xy=c(2,3),by=thin)$newData1[,2] #thin data  for the temp values
  plot(Value ~ sec, data=Resp.Data , xlab='Time (seconds)', ylab=substitute(' O'[2]~' (µmol/L)'), type='n', axes=FALSE) #plot thinned data
  usr  <-  par('usr')
  rect(usr[1], usr[3], usr[2], usr[4], col='grey90', border=NA)
  whiteGrid()
  box()
  points(Resp.Data $Value ~ Resp.Data $sec, pch=16, col=transparentColor('dodgerblue2', 0.6), cex=1.1)
  axis(1)
  axis(2, las=1)
  dev.off()
  
  Regs  <-  rankLocReg(xall=Resp.Data $sec, yall=Resp.Data $Value, alpha=a, 
                       method="pc", verbose=TRUE) 
  pdf(paste0("Output/",rename,"_regression.pdf"))
  plot(Regs)
  dev.off()

  #s <- seq(0,nrow(Resp.R),length(file.names)) #to order the file output sequence in correct order in data frame
  Resp.R[i,2:3] <- Regs$allRegs[1,c(4,5)] #inserts slope and intercept in the dataframe
  Resp.R[i,1] <- paste0(rename) #stores the file name in the Date column
  Resp.R[i,4] <- mean(Resp.Data$Temp, na.rm=T)  #stores the Temperature in the Temp.C column
  
}

Resp.R 

#Merge rates with sample info
Data <- merge(Resp.R, Sample.Info, by="Sample.ID")

#correct for the size of the chamber
Data$micromol.s <- Data$umol.L.sec * (Data$Chamber.Vol.mL/1000)

#calculate the average of the blanks from each time step
blnks <- subset(Data, Sample.Type=="Blank")
blnks <- aggregate(micromol.s ~ pH.Treatment, data=blnks, FUN=mean)
Data <- merge(Data, blnks, by="pH.Treatment")
colnames(Data)[colnames(Data) == 'micromol.s.x'] <- 'sample.micromol.s'
colnames(Data)[colnames(Data) == 'micromol.s.y'] <- 'blank.micromol.s'

#subtract the average of the blanks from each time step
Data$corr.micromol.s <- Data$sample.micromol.s - Data$blank.micromol.s
Data$micromol.bivol.s <- Data$corr.micromol.s/Data$Adult.Vol.ml
Data$micromol.bivol.h <- Data$micromol.bivol.s*3600

GeoResp <- cbind(Data$Date, Data$Sample.ID, Data$pH.Treatment, Data$Sample.Type, Data$micromol.bivol.h)

write.csv(GeoResp,"~/MyProjects/Geoduck_Conditioning/RAnalysis/Output/Adult.Resp.Test.csv")


