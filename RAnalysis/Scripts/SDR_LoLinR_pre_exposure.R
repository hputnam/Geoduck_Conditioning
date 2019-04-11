# Project: Geoduck Conditioning
# Title: SDR_LoLinR.R
# Supported by: FFAR
# Author: Sam Gurr
# Date Updated: 20181002
# Contact: samuel_gurr@uri.edu

# OBJECTIVE: use the LoLinR package for respiration measurements with a Presens SDR SensorDish (24-vial plate)
# This script was first written for experiments on juvenile (~5mm shell width) Pacific geoduck (Panopea genorosa)

# WHY LoLinR package?: respiration rate is often misinterpreted due to visual bias or use of entire datasets that
# include noise due to intrumentation error, poor mixing, physiological thresholds (decelione of resp or oxyconformity), etc.
# the Lolin package minimizes this error and allows for a relatively non-bias criteria 

# This script allows for AUTOMATED and REPRODUCIBLE output for the LoLinR package from several output files
# script targets use of Lpc with the ability to easily change constants for alpha and truncated datasets (time time based on row numbers)

# Assumption: raw SDR ouput files are edited for headers (row1) in vial order A1....D1, A2, ...D2, etc.  
####################################################
rm(list=ls())

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data")
main<-getwd()

#LIBRARY-----------------------------------------------------------------------------------------------------------
library(LoLinR)

# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"All_data_csv" #the location of all your respiration files

# assign your alpha value
a <- 0.4 #---------------------------------------------------------------------CHANGE THE NAME HERE EVERY TIME ( AND in the model itself!)
#assign your "Notes" column - refer to the amount of data used 
n <- "all" #---------------------------------------------------------------CHANGE THE NAME HERE EVERY TIME
# time gap for assigned resp vials
# write this for minutes 10-25 resp_vials[40:100,] min10-25_2
# write this for minutes 10-20 resp_vials[40:80,] min10-20_2
ouputNAME<-"Basal_resp_alpha0.4_all.csv" # -------------------------CHANGE THE NAME HERE EVERY TIME

# size data to merge to output
path2<-"All_data_csv/Cumulative_output/"
size.file<-"sampleID_number_size_with_basal.csv"
#load Data------------------------------------------------------------------------------------------
size<-read.csv(file.path(path2,size.file), header=T, sep=",", na.string="NA", as.is=T) 
# use only the basal data
size <- subset(size, Date=="20180713")
# combine a common ID to merge size with resp
size$Sample.ID <- paste(size$Date, size$SDR_position, size$RUN, sep="_")
#omit the rows that you just merged to avoid redundant data in the final output
size$RUN <- NULL
size$Date<-NULL
size$SDR_position<- NULL

###THIS SHOULD ALL COME FROM YOUR SAMPLE INFO FILE##
# make a table for to refer in the outer for loop - filenames, run, and date
TABLE_files <- data.frame(matrix(nrow = 1))
TABLE_files$run_number <- c(1)
TABLE_files$date <- c("20180713")

# each file name for the outer for loop
TABLE_files$filenames <- c(
"20180713_resp_pre_basal_Oxygen.csv")


df_total = data.frame()

# FOR loop calls from LoLin, models sequencially for each ind.var column, 
# calls Lpc from summary table, writes to a dataframe (24 rows / file - 1 row populated per column)
for(i in 1:nrow(TABLE_files)){
  resp<-read.csv(file.path("All_data_csv", (TABLE_files[1,4])), header=T, sep=",", na.string="NA", as.is=T) 
  # omit the Date.Time from the dataframe
  resp$Date.Time <-NULL

  Date <- TABLE_files[1,3]
  
  RUN <- TABLE_files[1,2]

  # your predicted values
  resp_vials <- resp[, !(colnames(resp) %in% c("Time.Min."))] 
  resp_vials 

  # inside for loop calls the 24 columns individually for the LoLin model
  # ouputs the Lpc individually to the created dataframe and rbinds it to df_total 
  for(j in t(1:ncol(resp_vials))){
    model <- rankLocReg(
  xall=resp$Time.Min.[], yall=resp_vials[, j],
  alpha=0.4, method="pc", verbose=TRUE)

    sum.table<-summary(model)

    resp.table <- data.frame(matrix(nrow = 1, ncol = 6))

    colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'b1.Lpc', 'alpha', 'Notes')
    
    resp.table$Date <- Date

    resp.table$RUN <- RUN

    resp.table$alpha <- a
    
    resp.table$Notes <- n
    
    #take data from your model summary (example is the slope and range of confidence interval)
    #resp.table$b1.Lz<-sum.table$Lcompare[1,6]
    #resp.table$b1.Leq<-sum.table$Lcompare[2,6]
    resp.table$b1.Lpc<-sum.table$Lcompare[3,6]
    #resp.table$ci.Lz<-sum.table$Lcompare[1,9]
    #resp.table$ci.Leq<-sum.table$Lcompare[2,9]
    #resp.table$ci.Lpc<-sum.table$Lcompare[3,9]

    #name dataframe for this single row
    df <- data.frame(resp.table)
    #bind to a cumulative list dataframe
    df_total <- rbind(df_total,df)
 print(df_total)
   }
}

#look at your cumulative dataframe
df_total

#### THIS SHOULD COME FROM YOUR SAMPLE INFO FILE####
#names from a1 to D6 in order of default in SDR sensor dish - X16 for the 16 files
      names <- c("A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6")
    
    # name the SDR position in the cumulative dataframe to merge with size
    df_total$SDR_position <- names
    # create a column in the cumulative dataframe that matches the size dataframe
    # generate new row with concatenated sample id
    df_total$Sample.ID <- paste(df_total$Date, df_total$SDR_position, df_total$RUN, sep='_') 
    # merge the cumulative dataframe from the loop with the size dataframe
    update.data <- merge(df_total,size, by="Sample.ID", all = TRUE, sort = T) 
    # write out to the path names outputNAME
    write.table(update.data,ouputNAME,sep=",", row.names=FALSE)
    
    
    

  





