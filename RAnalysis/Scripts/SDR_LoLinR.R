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
a <- 0.4
#assign your "Notes" column - refer to the amount of data used 
n <- "10-25"
# time gap for assigned resp vials
# write this for minutes 10 - resp_vials[40:100,]

#---------(5)--------------name the output as "Cumulative_resp_youralphavalue_amountofdataused"

ouputNAME<-"Cumulative_resp_alpha0.4_min10-25.csv"


# size data to merge to output
path2<-"All_data_csv/Cumulative_output"
size.file<-"/sampleID_number_size.csv"
#load Data------------------------------------------------------------------------------------------
size<-read.csv(file.path(path2,size.file), header=T, sep=",", na.string="NA", as.is=T) 
# combine a common ID to merge size with resp
size$Sample.ID <- paste(size$Date, size$SDR_position, size$RUN, sep="_")
#omit the rows that you just merged in order to not have redundant data in the final output
size$RUN <- NULL
size$Date<-NULL
size$SDR_position<- NULL


# make a table for to refer the loop of filenames, run and and date
TABLE_files <- data.frame(matrix(nrow = 16))
TABLE_files$run_number <- c(1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2)
TABLE_files$date <- c("20180716", "20180716", "20180719", "20180719", "20180722", "20180722", 
                      "20180724", "20180724", "20180807", "20180807", "20180809", "20180809", 
                      "20180811", "20180811", "20180813", "20180813")

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




#respfile<-"20180716_resp_Day2_RUN1_Oxygen.csv" # name of your file with resp data
#load Data--------------------------------------------------------------------------------------------------------------


# FOR loop calls from LoLin, models sequencially for each ind.var column, calls Lpc from summary table, writes to a dataframe (24 rows 1 per column)
df_total = data.frame()
for(i in 1:nrow(TABLE_files)){
  resp<-read.csv(file.path("All_data_csv", (TABLE_files[i,4])), header=T, sep=",", na.string="NA", as.is=T) 
  # omit the Date.Time from the dataframe
  resp$Date.Time <-NULL
  # FILL THE FOLLOWING:-------------------------------------------FIVE CHANGES TO MAKE-------------------------------------------
  # --------(1)--------------Date from the respfile name
  
  Date <- TABLE_files[i,3]
  
  # --------(2)--------------RUN number from the respfile name
  
  RUN <- TABLE_files[i,2]
  
  # --------(3)--------------change alpha value
  
  alpha <- a
  
  #---------(4)--------------comment on the amount of data used
  
  Notes <- n
  
  
  #-------------------------------------------NOW YOU CAN CONTROL A + ENTER---------------------------------------------------------
  #----------------------------repeat for all RUNs and Dates of the trial(s) in your size file (below)------------------------------
  #---------------This will make a seperate ouput file for each of the alpha/data frame replicated with Lpc-------------------------
  
  
  #resp_time <- resp[, !(colnames(resp) %in% c("Time.Min."))]
  # your predicted values
  resp_vials <- resp[40:100, !(colnames(resp) %in% c("Time.Min."))]
  resp_vials 
  
  #resp.table <- data.frame(matrix(nrow = 24, ncol = 6))
  #colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'b1.Lpc', 'alpha', 'Notes')

  for(j in t(1:ncol(resp_vials))){
    model <- rankLocReg(
  xall=resp$Time.Min.[40:100], yall=resp_vials[, j],
  alpha=0.2, method="pc", verbose=TRUE)

    sum.table<-summary(model)
    
    
    # name your table to call to in your summary table repeated from the model loop
    #resp.table <- data.frame(matrix(nrow = 1, ncol = 13))
    resp.table <- data.frame(matrix(nrow = 1, ncol = 6))
    #nrows<-nrow(resp.data[3,26])
    #colnames(resp.table )<-c('Date', 'ID','tank', 'SDR_position','number_indivs','b1.Lz',
    #'b1.Leq', 'b1.Lpc', 'ci.Lz', 'ci.Leq', 'ci.Lpc', 'alpha', 'Notes')
    colnames(resp.table)<-c('Date', 'RUN', 'SDR_position', 'b1.Lpc', 'alpha', 'Notes')
    
    # RESP TABLE ONE - populated for each and every loop
    resp.table$Date <- Date
    #resp.table$ID <- ID
    resp.table$RUN <- RUN
    #resp.table$SDR_position <- vial_pos
    #resp.table$tank <- tank_name
    resp.table$alpha <- alpha
    
    
    #take data from your model summary (example is the slope and range of confidence interval)
    #names(resp_vials)
    #extract b1
    #sum.table$Lcompare
    #resp.table$b1.Lz<-sum.table$Lcompare[1,6]
    #resp.table$b1.Leq<-sum.table$Lcompare[2,6]
    resp.table$b1.Lpc<-sum.table$Lcompare[3,6]
    #extract ci range
    #resp.table$ci.Lz<-sum.table$Lcompare[1,9]
    #resp.table$ci.Leq<-sum.table$Lcompare[2,9]
    #resp.table$ci.Lpc<-sum.table$Lcompare[3,9]
    resp.table$Notes <- Notes
    #Cumulative resp data
    #all.data <- read.csv("All_data_csv/Cumulative_Output/Cumulative_resp_Output2.csv", header=TRUE, sep=",")
    #update.data <- (all.data, resp.table)
# name dataframe for this single row
    df <- data.frame(resp.table)
# bind to a cumulative list dataframe
    df_total <- rbind(df_total,df)
    print(df_total)
    #update.data <- rbind(resp.table2 , resp.table)
    #write.table(update.data,"All_data_csv/Cumulative_Output/Cumulative_resp_Output2.csv",sep=",", row.names=FALSE)
    #write.table(update.data,"All_data_csv/Cumulative_Output/Cumulative_resp_Output.csv",sep=",", row.names=FALSE)
      #{
        #update.data$SDR_position <- names
        #update.data$Sample.ID <- paste(resp.table$Date, resp.table$SDR_position, resp.table$RUN, sep='_') #generate new row with concatenated sample id
        #update.data <- merge(update.data,size, by="Sample.ID", all = TRUE, sort = T) #merge seawater chemistry with total alkalinity
        #write.table(update.data,"All_data_csv/Cumulative_Output/Cumulative_resp_Output2.csv",sep=",", row.names=FALSE)
   }
}
df_total

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
    
    
    # name the SDR position in the cumulative dataframe to merge with size
    df_total$SDR_position <- names
    # create a column in the cumulative dataframe that matches the size dataframe
    df_total$Sample.ID <- paste(df_total$Date, df_total$SDR_position, df_total$RUN, sep='_') #generate new row with concatenated sample id
    
    # merge the cumulative dataframe from the loop with the size dataframe
    update.data <- merge(df_total,size, by="Sample.ID", all = TRUE, sort = T) #merge seawater chemistry with total alkalinity }
    
    # write out to the path names outputNAME
    write.table(update.data,ouputNAME,sep=",", row.names=FALSE)
    
    
    

  





