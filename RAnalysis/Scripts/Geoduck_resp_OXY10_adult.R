rm(list=ls())

#set working directory---------------------------------------------------------------------------------------------

setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/Oxy10_data")
main<-getwd()

#CHANGE THESE VALUES EVERY DAY----------------------------------------------------------------------
path<-"All_data_csv" #the location of all your titration files
respfile<-"20180804_ch_4.csv" # name of your file with resp data

# output file (FILL THIS IN) -----------------------------------
Date <- 20180804
ID <- "blank"
OXY10_channel <- 4
Biovolume <-  #ml
# length = 1
l <- #2.15
shell_length <- #(((l)*0.0254)*1000) # inches on measuring tape converted to mm
# width = w
w <- #(4+(11/16))
shell_width <- #(((w)*0.0254)*1000) # inches on measuring tape converted to mm
treatment <- "ambient"
tank <- "spawn/broodstock"
  
#load Data------------------------------------------------------------------------------------------
resp<-read.csv(file.path(path,respfile), header=T, sep=",", na.string="NA", as.is=T) 
# take a look at the file
resp

# assign headers----------------------------------------------------------------------------------
resp.data <- resp[,c(2,6,9)] #select columns = 26 total (Date + Time + 24 vials on SDR plate)
resp.data
#SDR plate organization - name columns accordingly as TANK_vial (Ex: H1_T_B1 where H1_T = Tank name; B1 = vial position)
############################-------------------------------------------------------------------------

colnames(resp.data) <- c('Time' , 'sensor', 'oxygen')

#### Time is a character - convert to numeric - convert to decimal form - convert to minutes

resp.data$time <- sapply(strsplit(resp.data$Time,":"),
                         function(x) {
                           x <- as.numeric(x)
                           x[1]*60+x[2]+x[3]/60
                         }
)
# convert to minutes--------------------------------------------------
resp.data$minutes <- (resp.data$time - resp.data$time[1])

# take alook at your dataset--------------------------------------------------
resp.data

# convert dataset to every 15 values to speed up LoLin analysis--------------------------------------------------
resp.data.2 <- resp.data[seq(1, nrow(resp.data), 15), ]

# take alook at your dataset--------------------------------------------------
resp.data.2

# here is a test of the commands for LoLinR---------------------------------------
#read about it here
#?rankLocReg
library(LoLinR)

# make a model for one of your data sets---------------------------------------
model<-rankLocReg(
  xall=resp.data.2$minutes,yall=resp.data.2$oxygen,
  alpha=0.2, method="pc", verbose=TRUE)

# Which alpha value gets Leq or Lpc at center of density peak? --------------
alpha <- 0.2

#plot the model----------------------------------------------------------------
plot(model, rank=1)
#if you want data from this model, name the summary for extraction--------------
sum.table<-summary(model)

#take data from your model summary (example is the slope and range of confidence interval)
resp.table <- data.frame(matrix(nrow = 1, ncol = 16))
nrows<-nrow(resp.data.2[3,26])
colnames(resp.table)<-c('Date', 'ID','OXY10_channel', 'tank', 'treatment', 'Biovolume', 'length', 'width','b1.Lz',
                         'b1.Leq', 'b1.Lpc', 'ci.Lz', 'ci.Leq', 'ci.Lpc', 'alpha', 'Notes')

#assign variables in output table
resp.table$Date <- Date
resp.table$ID <- ID
resp.table$OXY10_channel <- OXY10_channel
resp.table$Biovolume <- Biovolume
resp.table$length <- shell_length
resp.table$width <- shell_width
resp.table$treatment <- treatment
resp.table$tank <- tank

#extract b1
sum.table$Lcompare
resp.table$b1.Lz<-sum.table$Lcompare[1,6]
resp.table$b1.Leq<-sum.table$Lcompare[2,6]
resp.table$b1.Lpc<-sum.table$Lcompare[3,6]

#extract ci range
resp.table$ci.Lz<-sum.table$Lcompare[1,9]
resp.table$ci.Leq<-sum.table$Lcompare[2,9]
resp.table$ci.Lpc<-sum.table$Lcompare[3,9]

resp.table$alpha <- alpha

#Cumulative resp data
all.data <- read.csv("All_data_csv/Cumulative_Output/Cumulative_resp_Output.csv", header=TRUE, sep=",")
update.data <- rbind(all.data , resp.table)

write.table(update.data,"All_data_csv/Cumulative_Output/Cumulative_resp_Output.csv",sep=",", row.names=FALSE)

