# Geoduck Conditioning
# Title: SDR_LoLinR.R
# Supported by: FFAR
# This script was written by Sam J Gurr in Fall 2018
# Contact: samuel_gurr@uri.edu

# OBJECTIVE:
# calculate final respiration rate for several automate runs from the SDR_LoLin script
# ouputs dataframe with final respiration rate values in units ug L-1 h-1 indiv-1

# with the output table... (follow-up objective after this script is run)
# test the difference between atuomated runs (alpha = 0.2, 0.4, etc; trunc = all, 10-20min, etc.)
# to choose the constants that best represent our data - similarity to the "individually" run file
# in which a visual criteria was completed by hand for proximity to highest regression density from LoLin plots

# NOTE & Assumptions:
# order plays a major role in this script and will need to be adjusted for future measurements
# the 24-well plate without randomization in each of the "RUNS" - adjustment to blank triplets uses an if statments on row numbers 
# This works for the experiment in summer 2018, but will need to rewrite to call categorically in next version...
# units in the called files (column name "bi.Lpc") are assumed to be umol min-1 when uploaded to the script
# current script is written for 10 files each with 16 runs of the SensorDish (SDR) 24-well plate 
###################################################################################################
rm(list=ls())

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv")
main<-getwd()
# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Cumulative_output" #the location of all your respiration files

# make a table for all the filenames to call
table_o_files <- data.frame(matrix(nrow = 10))
table_o_files$rownumber <- c(1,2,3,4,5,6,7,8,9,10)
table_o_files$name <- c("Cumulative_resp_individually_all.csv",
                        "Cumulative_resp_alpha0.2_all.csv", "Cumulative_resp_alpha0.2_min10-20.csv", "Cumulative_resp_alpha0.2_min10-25.csv",
                        "Cumulative_resp_alpha0.4_all.csv", "Cumulative_resp_alpha0.4_min10-20.csv", "Cumulative_resp_alpha0.4_min10-25.csv",
                        "Cumulative_resp_alpha0.6_all.csv", "Cumulative_resp_alpha0.6_min10-20.csv", "Cumulative_resp_alpha0.6_min10-25.csv")
table_o_files$matrix.nrow...10. <- NULL

# make a dataframe to rbind the ouput
cumulative_respCALC <- data.frame(matrix(nrow = 384, ncol = 10)) # 384 is the size of each file
                                                      # composed of cumulative LoLin regression anlaysis of 16 files with
                                                      # 24 measurements (rows) each (12 animals + 12 replicates per "RUN")
# names here are the same order as the imported files in table_o_filenames$name
colnames(cumulative_respCALC) <- c("Resp_individually_all.csv",
                                   "LpcResp_alpha0.2_all.csv", "LpcResp_alpha0.2_min10-20.csv", "LpcResp_alpha0.2_min10-25.csv",
                                   "LpcResp_alpha0.4_all.csv", "LpcResp_alpha0.4_min10-20.csv", "LpcResp_alpha0.4_min10-25.csv",
                                   "LpcResp_alpha0.6_all.csv", "LpcResp_alpha0.6_min10-20.csv", "LpcResp_alpha0.6_min10-25.csv") # now has 10 columns for each file output


# OUTSIDE FOR LOOP --------------------------------------------------------------------------------
# calls each target file in the data.frame created above; all files are within the folder specified in the path
for(i in 1:nrow(table_o_files)){
  resp<-read.csv(file.path(path, (table_o_files[i,2])), header=T, sep=",", na.string="NA", as.is=T) 
  
  # the inside loop depends on the order of the file - must be ordered by "Date" and by "RUN" 
  resp_sorted <- resp[
    with(resp, order(resp$Date, resp$RUN)),
    ]
  
  # make a data.frame x - within x a column called rownumbers equal to the number of rows in i
  x <- data.frame()
  x$rownumbers <- 1:nrow(resp_sorted)
  t <- x$rownumbers
  N<-length(t)
  # this is a 'run-around' way of getting what we need out of the inside for loop
  # only possible becasue the 24-well plate was always arranged in the same manner
  A<-t[seq(1, N, 3)] 
  B<-t[seq(2, N, 3)]
  C<-t[seq(3, N, 3)]
  
  # INSIDE FOR LOOP --------------------------------------------------------------------------------
  # skips to every 3rd row and calls the blanks to make a mean
  # E.g. A1 + B1 are animals measurements with corresponding blank triplets at 12 - 14 rows ahead or C1-C3 and D1-D3, respectively (A)
  # A2 + B2 are  animal measurements that must be corrected for their corresponding blank triplets (C1 - C3 and D1 - D3)
  # however the blanks are now -1 rows ahead than the animal vials at the #1 position - hence calls 11 and 13 rows ahead (B)
  # same criteria for animal measurements at position A3 + B3 (C). 
  # WHY? before we proceed with converting our raw value we must correct for the blanks
        for (j in 1:nrow(x)){

        if (t == A) {
          p<-12
          q<-14
    }  else if (t == B) {
          p<-11
          q<-13
    }  else if (t == C){
          p<-10
          q<-12
    }  else {
          p<-0
          q<-0
    }
  
  # make a new column in resp_sorted for the absolute value of the raw resp value 
  resp_sorted$abs_resp <- abs(resp_sorted$b1.Lpc)
  # BEFORE WE CLOSE THE INSIDE FOR LOOP - calculate raw resp rate for each row [i] by subtrating from the called range of blank triplets (take a mean)
  resp_sorted$resprate_raw[j] <- resp_sorted$abs_resp[j] - mean(resp_sorted$abs_resp[(t+p):(t+q)])

    } # closed INSIDE for loop
  
  # OUSIDE FOR LOOP AFTER INSIDE CLOSED 
  # calculation in ug per L
  # Volume correction (L-1) = 1000/4 corrects for use of a 4ml vial
  # Time correction (h-1) = *60 to bring the rate from umol L-1 min-1 --> umol L-1 h-1
  # Concentration unit change = umol --> ug; (umol L-1 h-1)*31.998; 31.998 = moler mass of O2
  # Individual standardization = (umol L-1 h-1) / (# individuals per vial * mean size (mm length))
  # units after =  ug L-1 h-1 indiv-1
  resp_sorted$resprate_CALC <- ((((resp_sorted$resprate_raw/(1000/4))*(60))*31.998)/(resp_sorted$number_indivs*resp_sorted$mean_size))
  
  # bind the columns in cumulative resp sequentially with the files uploaded and calculated (NOTE: names in table_o_filename are SAME ORDER for Cumulatice_respCALC)
  rbind(cumulative_respCALC[i,]) <- resp_sorted$resprate_CALC
  
  # write the output
  write.table(cumulative_respCALC,"All_resp_calc_and_standardized",sep=",", row.names=FALSE)
  
} # closed OUTSIDE for loop


