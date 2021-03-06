# Geoduck Conditioning
# Title: SDR_LoLinR.R
# Supported by: FFAR
# This script was written by Sam J Gurr in Fall 2018
# Contact: samuel_gurr@uri.edu


library(plyr)


# OBJECTIVE:
# calculate final respiration rate for several automate runs from the SDR_LoLin script
# ouputs dataframe with final respiration rate values in units ug L-1 h-1 indiv-1

# with the output table... (follow-up objective after this script is run)
# test the difference between atuomated runs (alpha = 0.2, 0.4, etc; trunc = all, 10-20min, etc.)
# to choose the constants that best represent our data - similarity to the "individually" run file
# in which a visual criteria was completed by hand for proximity to highest regression density from LoLin plots

# NOTE & Assumptions:
# character-matching plays a major role in this script- a merge, an if statement, and mean calculations are based on characters
# units in the called files (column name "bi.Lpc") are assumed to be umol min-1 when uploaded to the script
# current script is written for 10 files each with 16 runs of the SensorDish (SDR) 24-well plate 
###################################################################################################
rm(list=ls())

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv")
main<-getwd()
# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Cumulative_output/" #the location of all your respiration files

# make a table for all the filenames to call
table_o_files <- data.frame(matrix(nrow = 10))
table_o_files$rownumber <- c(1,2,3,4,5,6,7,8,9,10)
table_o_files$name <- c("Cumulative_resp_individually_all.csv",
                        "Cumulative_resp_alpha0.2_all.csv", "Cumulative_resp_alpha0.2_min10-20.csv", "Cumulative_resp_alpha0.2_min10-25.csv",
                        "Cumulative_resp_alpha0.4_all.csv", "Cumulative_resp_alpha0.4_min10-20.csv", "Cumulative_resp_alpha0.4_min10-25.csv",
                        "Cumulative_resp_alpha0.6_all.csv", "Cumulative_resp_alpha0.6_min10-20.csv", "Cumulative_resp_alpha0.6_min10-25.csv")
table_o_files$matrix.nrow...10. <- NULL

# make a dataframe to rbind the ouput
cumulative_respCALC <- data.frame(matrix(nrow = 384, ncol = 16)) # 384 is the size of each file
                                                      # composed of cumulative LoLin regression anlaysis of 16 files with
                                                      # 24 measurements (rows) each (12 animals + 12 replicates per "RUN")
# names here are the same order as the imported files in table_o_filenames$name
colnames(cumulative_respCALC) <- c("Resp_individually_all.csv",
                                   "LpcResp_alpha0.2_all.csv", "LpcResp_alpha0.2_min10-20.csv", "LpcResp_alpha0.2_min10-25.csv",
                                   "LpcResp_alpha0.4_all.csv", "LpcResp_alpha0.4_min10-20.csv", "LpcResp_alpha0.4_min10-25.csv",
                                   "LpcResp_alpha0.6_all.csv", "LpcResp_alpha0.6_min10-20.csv", "LpcResp_alpha0.6_min10-25.csv", # now has 10 columns for each file output
                                    "Date", "SDR_pos", "RUN", "tank", "Day_trial", "Treat1_Treat2")

# OUTSIDE FOR LOOP --------------------------------------------------------------------------------
# calls each target file in the data.frame created above; all files are within the folder specified in the path
for(i in 1:nrow(table_o_files)){
 resp<-read.csv(file.path(path, (table_o_files[i,2])), header=T, sep=",", na.string="NA", as.is=T) 
  #resp<-read.csv(file.path(path, "Cumulative_resp_individually_all.csv")) 
  resp
  
  
  # the inside loop depends on the order of the file - must be ordered by "Date" and by "RUN" 
  resp_sorted <- resp[
    with(resp, order(resp$Date, resp$RUN)),
    ]
  resp_sorted

  
  resp_sorted$newname <- paste(
    resp_sorted$Date, (substr(resp_sorted$tank, 1, 4)), (substr(resp_sorted$ID, 6, 10)), sep="_")
  
  dataMEANS <- ddply(resp_sorted, "newname", summarise, mean = mean(abs(b1.Lpc)))
  
  MEANSblanks <- dataMEANS[(which(nchar(dataMEANS$newname) == 19)),]
  
  resp_sorted_2 <- merge(resp_sorted,MEANSblanks, by="newname", all = TRUE, sort = T) 
  
  ###########################################################################################begin from here
  resp_sorted_2$row <- seq.int(nrow(resp_sorted_2))

  for (j in 1:nrow(resp_sorted_2)){
  
  resp_sorted_2$resprate_CORRECTED <- ((abs(resp_sorted_2$b1.Lpc)) - (resp_sorted_2$mean[(resp_sorted_2$row+3)]))
     
  resp_sorted_2$resprate_CORRECTED
  # bind the columns in cumulative resp sequentially with the files uploaded and calculated (NOTE: names in table_o_filename are SAME ORDER for Cumulatice_respCALC)
  
  
  resp_sorted_2$resprate_CALC <- ((((resp_sorted_2$resprate_CORRECTED/(1000/4))*(60))*31.998)/(resp_sorted_2$number_indivs*resp_sorted_2$mean_size))
  resp_sorted_2
  
  }
  # This small function eliminates a datafram based on NA of a certain columns
  # In this case, the NA are in resprate_CALC
  desiredCols <- resp_sorted_2[,c(3,4,5,7,10,11,12,16,19)]
  completeFun <- function(resp_sorted_2,  desiredCols) {
    completeVec <- complete.cases(resp_sorted_2[, desiredCols])
    return(resp_sorted_2[completeVec, ])
  }
  
  resp_FINAL<-completeFun(resp_sorted_2, "resprate_CALC")
  

  resp_FINAL <- resp_sorted_2[
    with(resp_sorted_2, order(resp_sorted_2$Date, resp_sorted_2$RUN, resp_sorted_2$SDR_position)),
    ]
  
  cumulative_respCALC[,i] <- resp_FINAL$resprate_CALC
  
  #cumulative_respCALC[,1] <- resp_sorted$resprate_CALC
  cumulative_respCALC
  print(cumulative_respCALC)
  
  # write the output
  cumulative_respCALC[,11] <- resp_sorted$Date
  cumulative_respCALC[,12] <- resp_sorted$SDR_position
  cumulative_respCALC[,13] <- resp_sorted$RUN
  cumulative_respCALC[,14] <- resp_sorted$tank
  cumulative_respCALC[,15] <- resp_sorted$Day_Trial
  cumulative_respCALC[,16] <- resp_sorted$Treat1_Treat2

  cumulative_respCALC[,c(11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10)]
} # closed OUTSIDE for loop


# This small function eliminates a datafram based on NA of a certain columns
# In this case, the NA are in resprate_CALC
desiredCols2 <- cumulative_respCALC[,c(11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10)] # we want all of the columns
completeFun <- function(cumulative_respCALC,  desiredCols2) {
  completeVec <- complete.cases(cumulative_respCALC[, desiredCols2])
  return(cumulative_respCALC[completeVec, ])
}
# eliminates all rows with NA in "LpcResp_alpha0.6_min10-25.csv"
cumulative_respCALC_FINAL <-   resp_FINAL<-completeFun(cumulative_respCALC, "LpcResp_alpha0.6_min10-25.csv") # choose an example of a data file
print(cumulative_respCALC_FINAL)

write.table(cumulative_respCALC_FINAL,"All_resp_calc_and_standardized.csv",sep=",", row.names=FALSE)

