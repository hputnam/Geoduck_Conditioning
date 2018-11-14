#Title: Respiration Calculations
#Author: HM Putnam
#Edited by: HM Putnam
#Date Last Modified: 20181002
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
# setwd("~/MyProjects/Geoduck_Conditioning/RAnalysis/") #set working
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/")

##### Respiration #####
# path.p<-"Data/Respirometry/Larval" #the location of all your respirometry files 
path.p<-"Data/SDR_data/All_data_csv " #the location of all your respirometry files 

# bring in the respiration file names
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders
#basename above removes the subdirectory name from the file
#add file names that include the subdirectory name
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) #list all csv file names in the folder and subfolders

#generate a 3 column dataframe with specific column names
Resp.R <- data.frame(matrix(NA, nrow=length(file.names)*2, ncol=3))
colnames(Resp.R) <- c("Sample.ID","Intercept", "umol.L.sec")

#Load Sample Info
# Sample.Info <- read.csv(file="Data/Larval_Sample_Info.csv", header=T) #read sample.info data
Sample.Info <- read.csv(file="Data/All_growth_data.csv", header=T) #read sample.info data

#includes treatment, tank, chamber volume, animal size/number etc for normalization

for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Resp.Data <-read.table(file.path(path.p,file.names[i]), skip = 56, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data$Time.Min. <- seq.int(0.017, (nrow(Resp.Data))*0.25, by=0.25) #set time in min
  Resp.Data[ Resp.Data[,] == "No Sensor" ] <- as.numeric(runif(nrow(Resp.Data), min=0, max=100)) #convert any vials with no data
  Resp.Data <- Resp.Data[,2:26]
  
  
  for(j in 2:(ncol(Resp.Data)-1)){
    model <- rankLocReg(
      xall=Resp.Data$Time.Min., yall=as.numeric(Resp.Data[, j]),
      alpha=0.4, method="pc", verbose=TRUE)
  
  pdf(paste0("~/MyProjects/Geoduck_Conditioning/RAnalysis/Data/Respirometry/Larval/Resp_Plots/",i,"_",j,"_regression.pdf"))
  plot(model)
  dev.off()
}
}

