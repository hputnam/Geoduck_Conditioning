# Project: Juvenile_Geoduck_OA
# Title: SDR_LoLinR_reference.R
# Supported by: FFAR
# Author: Sam Gurr
# Date Updated: 20190410
# Contact: samuel_gurr@uri.edu

# OBJECTIVE: This script was used to assemble the initial "Reference" respiration rate data in which a criterion with the LoLin R package was used 
# to calculate rates of oxygen consumption on each indiviual rate to alter alpha or weighting method (Lpc, Leq, or Lz) when needed  
# This script was first written for data on juvenile (~5mm shell width) Pacific geoduck (Panopea genorosa)

# WHY LoLinR package?: respiration rate is often misinterpreted due to visual bias or use of entire datasets that
# include noise due to intrumentation error, poor mixing, physiological thresholds, etc.
# the Lolin package minimizes this error and allows for calculation of relatively non-bias rates of oxygen consumption 

# CRTIERIA = alpha or weighting method that best approximates the peak empirical distribution of local linear regressions
# via diagnostic plots; LoLin package perameters = alpha=0.2 and Lpc in MOST cases 

rm(list=ls()) # removes all prior objects

# Install packages if not already in your library
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') install.packages('LoLinR') 
# Load packages and pacage version/date/import/depends info
library(LoLinR) # Version: version 0.0.0.9000

setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data") # set working directory
main<-getwd()

#CHANGE HERE TO CALL DATAFILES
path<-"All_data_csv" #the location of all respiration csv files
respfile<-"20180724_resp_Day10_RUN1_Oxygen.csv" # call each file individually for this initial diagnostic
resp<-read.csv(file.path(path,respfile), header=T, sep=",", na.string="NA", as.is=T) # call data
resp # view the data

# assign headers
resp.data <- resp[,c(1:26)] # assign headers, select columns = 26 total (Date + Time + 24 vials on SDR plate)
#SDR plate organization - name columns accordingly as TANK_vial (Ex: H1_T_B1 where H1_T = Tank name; B1 = vial position)

# "RUN1" files
#  Tray H0 & Tray H1
colnames(resp.data) <- c("Date", "minutes",
# Row 1 (A1, B1, C1, D1)
 "H0_T_A1", "H0_B_B1", "H0_T_blank_C1", "H0_B_blank_D1", 

# Row 2 (A2, B2, C2, D2)
"H0_T_A2","H0_B_B2", "H0_T_blank_C2", "H0_B_blank_D2",

# Row 3 (A3, B3, C3, D3)
"H0_T_A3", "H0_B_B3", "H0_T_blank_C3", "H0_B_blank_D3",

# Row 4 (A4, B4, C4, D4)
 "H1_T_A4", "H1_B_B4", "H1_T_blank_C4", "H1_B_blank_D4",

# Row 5 (A5, B5, C5, D5)
"H1_T_A5", "H1_B_B5", "H1_T_blank_C5", "H1_B_blank_D5",

# Row 6 (A6, B6, C6, D6)
"H1_T_A6", "H1_B_B6", "H1_T_blank_C6", "H1_B_blank_D6") #rename columns

#  "RUN2" files
# Tray H2 & Tray H3; for files named "RUN2"
# colnames(resp.data) <- c("Date", "minutes",
# Row 1 (A1, B1, C1, D1)
# "H2_T_A1", "H2_B_B1", "H2_T_blank_C1", "H2_B_blank_D1", 

# Row 2 (A2, B2, C2, D2)
# "H2_T_A2","H2_B_B2", "H2_T_blank_C2", "H2_B_blank_D2",

# Row 3 (A3, B3, C3, D3)
# "H2_T_A3", "H2_B_B3", "H2_T_blank_C3", "H2_B_blank_D3",

# Row 4 (A4, B4, C4, D4)
# "H3_T_A4", "H3_B_B4", "H3_T_blank_C4", "H3_B_blank_D4",

# Row 5 (A5, B5, C5, D5)
# "H3_T_A5", "H3_B_B5", "H3_T_blank_C5", "H3_B_blank_D5",

# Row 6 (A6, B6, C6, D6)
# "H3_T_A6", "H3_B_B6", "H3_T_blank_C6", "H3_B_blank_D6") #rename columns

# fill this in and press Cnrt A + enter for each vial 
Date <- "20180724"
tank_name <- "H0_B"
vial_pos <- "B1"
ID <- "H0_B_B1"
tank_vial <- resp.data$H1_B_B6
alpha <- 0.5

#?rankLocReg (if you want to read more about the LoLin R model)

# LoLin R model - "rankLogReg"
model<-rankLocReg(
  xall=resp.data$minutes,yall=tank_vial, # call the repiration data by ID of the tank and vial position in the 24-well SDR dish
  alpha=0.2, method="pc", verbose=TRUE) # alpha 0.2 and Lpc as default criterion

plot(model, rank=1) # diagnostic plot - view for proximity of Lpc to the peak distribution of local linear regressions

# Now that you have seen the plot, do you want to save this data? If so, proceeed below...
sum.table<-summary(model) # name the LoLin model summary to prepare for cumulative extraction


resp.table <- data.frame(matrix(nrow = 1, ncol = 13)) # start a summary table 
nrows<-nrow(resp.data[3,26]) # assign the nnumber of rows needed
colnames(resp.table )<-c('Date', 'ID','tank', 'SDR_position','number_indivs','b1.Lz',
                         'b1.Leq', 'b1.Lpc', 'ci.Lz', 'ci.Leq', 'ci.Lpc', 'alpha', 'Notes') # name headers

resp.table$Date <- Date # add date
resp.table$ID <- ID # add ID
resp.table$SDR_position <- vial_pos # add the position on the SDR dish
resp.table$tank <- tank_name # add the tray ID

# model summary data - although we set the perameter or Lpc, the model summary table outputs all weighting method calculations
sum.table$Lcompare 
resp.table$b1.Lz<-sum.table$Lcompare[1,6] # output from weighting method Lz
resp.table$b1.Leq<-sum.table$Lcompare[2,6] # output from weighting method Leq
resp.table$b1.Lpc<-sum.table$Lcompare[3,6] # output from weighting method Lpc (default)

#extract ci range
resp.table$ci.Lz<-sum.table$Lcompare[1,9]  # conf.interv from weighting method Lz
resp.table$ci.Leq<-sum.table$Lcompare[2,9] # conf.interv from weighting method Leq
resp.table$ci.Lpc<-sum.table$Lcompare[3,9] # conf.interv from weighting method Lpc (default)

resp.table$alpha <- alpha # save your alpha value used to run the model

# export the data to a cumulative csv file - this is the "Reference" data set
all.data <- read.csv("All_data_csv/Cumulative_Output/Cumulative_resp_reference_Output.csv", header=TRUE, sep=",") # call the existing csv
update.data <- rbind(all.data , resp.table) # bind to cumulatve spreadsheet
write.table(update.data,"All_data_csv/Cumulative_Output/Cumulative_resp_reference_Output.csv",sep=",", row.names=FALSE) # write output table

