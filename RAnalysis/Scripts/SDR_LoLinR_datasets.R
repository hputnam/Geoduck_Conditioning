# Project: Juvenile_Geoduck_OA
# Title: SDR_LoLinR_datasets.R
# Supported by: FFAR
# Author: Sam Gurr
# Date Updated: 20190410
# Contact: samuel_gurr@uri.edu

# OBJECTIVE: This script was used to assemble datasets at different LoLin.R parameters. Instead of tediously
# going through each diagnostic plot for every respiration run (as in the script, "SDR_LoLinR_Reference"), 
# this scirpt was  written in order to change alpha, truncate, and run all respiration data for a cumunaltive csv.
# We decided to run the following perameters:
# alpha = 0.2 + no truncation of 30 minute record
# alpha = 0.2 + truncation for minute 10 - 20
# alpha = 0.2 + truncation for minute 10 - 25
# alpha = 0.4 + no truncation of 30 minute record
# alpha = 0.4 + truncation for minute 10 - 20
# alpha = 0.4 + truncation for minute 10 - 25
# alpha = 0.6 + no truncation of 30 minute record
# alpha = 0.6 + truncation for minute 10 - 20
# alpha = 0.6 + truncation for minute 10 - 25

rm(list=ls()) # removes all prior objects

# Install packages if not already in your library
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install.packages('LoLinR') 
# Load packages and pacage version/date/import/depends info
library(LoLinR) # Version: version 0.0.0.9000

rm(list=ls()) # removes all prior objects

setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data") # set working directory
main<-getwd()


path<-"All_data_csv" # the location of all respiration files


a <- 0.4 # assign alpha and add a comment on truncation 
n <- "min10-25" # assign your "Notes" column - refer to the amount of data used / truncation info
ouputNAME<-"Cumulative_resp_alpha0.4_min10-25.csv" # name this cumunative sheet with the alpha and truncation (i.e. "Cumulative_resp_alpha0.4_min10-25.csv")

# size data to merge to output
path2<-"All_data_csv/Cumulative_output"
size.file<-"/sampleID_number_size.csv"
#load Data------------------------------------------------------------------------------------------
size<-read.csv(file.path(path2,size.file), header=T, sep=",", na.string="NA", as.is=T) 
# combine a common ID to merge size with resp
size$Sample.ID <- paste(size$Date, size$SDR_position, size$RUN, sep="_")
#omit the rows that you just merged to avoid redundant data in the final output
size$RUN <- NULL
size$Date<-NULL
size$SDR_position<- NULL

###THIS SHOULD ALL COME FROM YOUR SAMPLE INFO FILE##
# make a table for to refer in the outer for loop - filenames, run, and date
TABLE_files <- data.frame(matrix(nrow = 16))
TABLE_files$run_number <- c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)
TABLE_files$date <- c("20180716", "20180716", "20180719", "20180719", "20180722", "20180722", 
                      "20180724", "20180724", "20180807", "20180807", "20180809", "20180809", 
                      "20180811", "20180811", "20180813", "20180813")

# each file name for the outer for loop
TABLE_files$filenames <- c(
"20180716_resp_Day2_RUN1_Oxygen.csv",
"20180716_resp_Day2_RUN2_Oxygen.csv",
               
"20180719_resp_Day5_RUN1_Oxygen.csv",
"20180719_resp_Day5_RUN2_Oxygen.csv",
               
"20180722_resp_Day8_RUN1_Oxygen.csv",
"20180722_resp_Day8_RUN2_Oxygen.csv",
               
"20180724_resp_Day10_RUN1_Oxygen.csv",
"20180724_resp_Day10_RUN2_Oxygen.csv",
              
"20180807_resp_Day0_RUN1_Oxygen.csv",
"20180807_resp_Day0_RUN2_Oxygen.csv",
              
"20180809_resp_Day2_RUN1_Oxygen.csv",
"20180809_resp_Day2_RUN2_Oxygen.csv",
              
"20180811_resp_Day4_RUN1_Oxygen.csv",
"20180811_resp_Day4_RUN2_Oxygen.csv",
              
"20180813_resp_Day6_RUN1_Oxygen.csv",
"20180813_resp_Day6_RUN2_Oxygen.csv")

df_total = data.frame() # start a data frame

# for loop to analyze all respirtion data 
for(i in 1:nrow(TABLE_files)){
  resp<-read.csv(file.path("All_data_csv", (TABLE_files[i,4])), header=T, sep=",", na.string="NA", as.is=T) 
  resp$Date.Time <-NULL
  Date <- TABLE_files[i,3]
  RUN <- TABLE_files[i,2]
  resp_vials <- resp[40:100, !(colnames(resp) %in% c("Time.Min."))]  # TRUNCATION CHANGE HERE! no trunc = [,], min10-20 = [40:80,], and min10-25 = [40:100,]
  resp_vials 

  # inside for loop calls each individual respiration vial (column) individually  for the LoLin model
  for(j in t(1:ncol(resp_vials))){
    model <- rankLocReg(
  xall=resp$Time.Min.[40:100], yall=resp_vials[, j], # TRUNCATION CHANGE HERE! no trunc = [,], min10-20 = [40:80,], and min10-25 = [40:100,]
  alpha= 0.4, method="pc", verbose=TRUE) # ALPHA CHANGE HERE! run the model for your asssigned alpha value
    sum.table<-summary(model) # call the summary data
    resp.table <- data.frame(matrix(nrow = 1, ncol = 6)) # create a new data table
    colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'b1.Lpc', 'alpha', 'Notes') # assign headers
    resp.table$Date <- Date # fill date
    resp.table$RUN <- RUN # fill run number 
    resp.table$alpha <- a # fill with the chosen alpha value (assigned at start of script)
    resp.table$Notes <- n # fill with the note (assigned at start os script)
    resp.table$b1.Lpc<-sum.table$Lcompare[3,6] # save the Lpc calculation for respiration rate 

    df <- data.frame(resp.table) # name dataframe for this single row
    df_total <- rbind(df_total,df) # bind to a cumulative list dataframe
 print(df_total) # show loop progress in the console
   }
}

df_total # view data - this will contain all respirtion rates from exposure 1 and exposure 2 for the assigned alpha value and truncation

# vial ID for 24 resp vial in each of the 16 files(fixed order in raw output from Presens)
      names <- c("A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6",
                 "A1", "B1", "C1", "D1", "A2", "B2", "C2", "D2", 
                 "A3", "B3", "C3", "D3", "A4", "B4" ,"C4", "D4", 
                 "A5", "B5", "C5", "D5", "A6", "B6", "C6", "D6")
    
df_total$SDR_position <- names # name the SDR position in the cumulative dataframe to merge with size
df_total$Sample.ID <- paste(df_total$Date, df_total$SDR_position, df_total$RUN, sep='_')  # generate new row with concatenated sample id
update.data <- merge(df_total,size, by="Sample.ID", all = TRUE, sort = T)  # merge the cumulative dataframe from the loop with the size dataframe
write.table(update.data,ouputNAME,sep=",", row.names=FALSE) # write table to the path name
