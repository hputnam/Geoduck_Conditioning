#Title: Larval Resp. LoLin script
#Author: Sam Gurr & HM Putnam
#Edited by: Sam Gurr
#Date Last Modified: 20190329
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
#Load Sample Info
Sample.Info <- read.csv(file="Data/SDR_data/REFERENCE_number.individuals_shell.size.csv", header=T) #read sample.info data


# CHANGE THE FOLLOWING ..THEN CONTROL A + ENTER
# Respiration File
path.p<-"Data/SDR_data/All_data_csv" #the location of all your respirometry files 
a <- 0.4
ouputNAME<-"Data/SDR_data/All_data_csv/Output_157d_post/Respiration/Resp_157d_post_alpha0.4_10-20mins.csv" 


# bring in the respiration file names CHANGE LINE 53 TO RUN NEW DATASETS
file.names.full<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders
file.names <- file.names.full[c(19:20)] # call the files on 20190116 157 dyas post experiment
file.names # look at the names of the csv files you will call in the following for loop

df_total <- data.frame() # start dataframe 
resp.table <- data.frame(matrix(nrow = 1, ncol = 7)) # create dataframe to save cumunalitively during for loop
colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'Lpc', 'Leq' , 'Lz', 'alpha') # names for comuns in the for loop

for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Resp.Data <-read.table(file.path(path.p,file.names[i]), skip = 56, header=T, sep=",", na.string="NA", fill = TRUE, as.is=TRUE, fileEncoding="latin1") #reads in the data files
  Resp.Data$Time.Min. <- seq.int(0.017, (nrow(Resp.Data))*0.25, by=0.25) #set time in min
  #Resp.Data[Resp.Data[,] == "No Sensor"] <- as.numeric(runif(nrow(Resp.Data), min=0, max=300)) #convert any vials with no data
  Resp.Data <- Resp.Data[,2:27] #use only res values - 24 total in the 24 well plate (SDR SensorDish)
  Resp.Data <- Resp.Data[41:89,] # truncated for 10-20 minnute record (used for juv geoduck 20190116)
  # tail(Resp.Data) # check the dataset

  for(j in 2:(ncol(Resp.Data)-1)){
    model <- rankLocReg(
      xall=Resp.Data$Time.Min., yall=as.numeric(Resp.Data[, j]),
      alpha=a, method="pc", verbose=TRUE) # run the LoLin script
    
    sum.table<-summary(model)
    resp.table$Date <- substr(file.names[i], 1,8) # all files have date in the form of yyyymmdd at the start of each csv name
    resp.table$RUN <- substr(file.names[i], 15,15) # assign the run to the number in the title for the trials completed that day
    resp.table$SDR_position <- colnames(Resp.Data[j]) # assign the vial position - this will be related to contents (blank or individuals) later in script
    resp.table$alpha <- a # set at start of script - reresents the proportion of data for final estimate of slopes (Lpc, Leq, Lz)
    resp.table$Lpc <-sum.table$Lcompare[3,6] # Lpc slope 
    resp.table$Leq <-sum.table$Lcompare[2,6] # Leq slope 
    resp.table$Lz <-sum.table$Lcompare[1,6]  # Lz slope 
    #resp.table$ci.Lz<-sum.table$Lcompare[1,9]
    #resp.table$ci.Leq<-sum.table$Lcompare[2,9]
    #resp.table$ci.Lpc<-sum.table$Lcompare[3,9]
    
    
    df <- data.frame(resp.table) # name dataframe for this singl e row
    df_total <- rbind(df_total,df) #bind to a cumulative list dataframe
    print(df_total) # print to monitor progress
  
    # save plots every inside loop and name by date_run_vialposition
  pdf(paste0("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv/Output_157d_post/Respiration/plots_alpha0.4_10-20mins/",substr(file.names[i], 1,8),"_", "RUN",substr(file.names[i], 15,15),"_",colnames(Resp.Data[j]),"_regression.pdf"))
  plot(model)
  dev.off()
} # end of inside for loop
} # end of outside for loop

# merge with the preexisiting table?
new_table <- (df_total) # bind the new table from the for loop to the pre exisiting table
write.table(new_table,ouputNAME,sep=",", row.names=FALSE)  # write out to the path names outputNAME
