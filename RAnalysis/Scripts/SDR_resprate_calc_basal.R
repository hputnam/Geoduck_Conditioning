# Geoduck Conditioning
# Title: SDR_resprate_calc_basal.R
# Supported by: FFAR
# This script was written by Sam J Gurr in Fall 2018
# Contact: samuel_gurr@uri.edu

library(plyr)

###################################################################################################
rm(list=ls())

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv")
main<-getwd()
# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Cumulative_output/" #the location of all your respiration files

# make a table for all the filenames to call
table_o_files <- data.frame(matrix(nrow = 1))
table_o_files$rownumber <- c(1)
table_o_files$name <- c("Basal_resp_alpha0.4_all.csv")
table_o_files$matrix.nrow...10. <- NULL

# make a dataframe to rbind the ouput
cumulative_respCALC <- data.frame(matrix(nrow = 24, ncol = 7)) 

# names here are the same order as the imported files in table_o_filenames$name
colnames(cumulative_respCALC) <- c("Basal_resp_alpha0.4_all.csv", # now has 10 columns for each file output
                                    "Date", "SDR_pos", "RUN", "tank", "Day_trial", "Treat1_Treat2")

resp<-read.csv(file.path(path, (table_o_files[1,3])), header=T, sep=",", na.string="NA", as.is=T) 
#resp<-read.csv(file.path(path, "Cumulative_resp_individually_all.csv")) 
resp


resp$newname <- paste(
  resp$Date, (substr(resp$ID, 1, 8)), sep ="_")
  
  dataMEANS <- ddply(resp, "newname", summarise, mean = mean(abs(b1.Lpc)))
  
  resp_values <- subset(resp, ID=="prebasal")
  
  ###########################################################################################begin from here
  resp_values$row <- seq.int(nrow(resp_values))

  for (j in 1:nrow(resp_values)){
  
    resp_values$resprate_CORRECTED <- ((abs(resp_values$b1.Lpc)) - (dataMEANS[1,2]))
     
    resp_values$resprate_CORRECTED
  # bind the columns in cumulative resp sequentially with the files uploaded and calculated (NOTE: names in table_o_filename are SAME ORDER for Cumulatice_respCALC)
  
  
    resp_values$resprate_CALC <- ((((resp_values$resprate_CORRECTED/(1000/4))*(60))*31.998)/(resp_values$number_indivs*resp_values$mean_size))
    resp_values
  
  }

  resp_FINAL<-completeFun(resp_values, "resprate_CALC")
  

  resp_FINAL <- resp_values[
    with(resp_values, order(resp_values$Date, resp_values$RUN, resp_values$SDR_position)),
    ]
  
  
  # write the output
  resp_FINAL[,1] <- resp_values$resprate_CALC
  resp_FINAL[,2] <- resp_values$Date
  resp_FINAL[,3] <- resp_values$SDR_position
  resp_FINAL[,4] <- resp_values$RUN
  resp_FINAL[,5] <- resp_values$tank
  resp_FINAL[,6] <- resp_values$Day_Trial
  resp_FINAL[,7] <- resp_values$Treat1_Treat2

names(resp_FINAL)<- c("LpcResp_alpha0.4_all", "Date", "SDR_pos", "RUN", "tank", "Day", "Treat1_Treat2")

resp_FINAL <- resp_FINAL[,c(1:7)]

write.table(resp_FINAL,"Basal_resp_calc_and_standardized.csv",sep=",", row.names=FALSE)

