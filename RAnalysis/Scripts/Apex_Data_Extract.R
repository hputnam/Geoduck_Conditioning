#http://www.informit.com/articles/article.aspx?p=2215520



library("XML")
library("plyr")

xmlfile <- xmlParse("http://192.168.1.100:80/cgi-bin/datalog.xml?sdate=180624&days=3") #read in the date plus x days of Apex data

Apex.Data <- ldply(xmlToList(xmlfile), data.frame) #convert xml to dataframe

Apex.Data2 <- Apex.Data[4:nrow(Apex.Data),] #remove extra metadata from top
Apex.Data2 <- head(Apex.Data2,-2) #remove extra metadata from bottom

# view data and names to ID the raw probe.name or probe.type or probe.value
Apex.Data2
names(Apex.Data2)

# Column names modified as of 20180628
# Date.Time = column 2
# TMP_T2 = column 6
# pH_T2= column 9
# ORP = column 12
# salinity = column 15
# TMP_T4 = column 69
# pH_T4 = column 72
# TMP_T5 = column 75
# pH_T5 = column 78
# TMP_T3 = column 81
# pH_T3 = column 84
# NOTE: 11 in total above

#keep columnes with data of interest. This needs to be changed as it will be specific to the Apex configuration
Probe.Data <- Apex.Data2[,c(3,6,9,12, 15, 69, 72, 75, 78, 81, 84)] #select columns
colnames(Probe.Data ) <- c("Date.Time", "TMP_T2", "pH_T2", "ORP", "Sal", "TMP_T4", "pH_T4","Temp_T5",
                           "pH_T5", "TMP_T3", "pH_T3")  #rename columns
Probe.Data$Date.Time <- as.POSIXct(Probe.Data$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz="HST") #convert date to HI time
write.csv(Probe.Data, "C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/Apex_data/Output/180627_Apex_Data_Output.data.csv") #write file to save data

#plot Temp and pH and save to output

pdf("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/Apex_data/Output/180627_Apex_Output.pdf")
par(mfrow=c(2,1))
plot(as.numeric(as.character(TMP_T2)) ~ Date.Time, Probe.Data, col = "grey", type="l", ylim=c(12, 20),  xlab="Time", ylab="Temperature °C")
lines(as.numeric(as.character(TMP_T3)) ~ Date.Time, Probe.Data, col = "red")
lines(as.numeric(as.character(TMP_T4)) ~ Date.Time, Probe.Data, col = "blue")
lines(as.numeric(as.character(Temp_T5)) ~ Date.Time, Probe.Data, col = "black")
axis.POSIXct(side=1, Probe.Data$Date.Time)

plot(as.numeric(as.character(pH_T2)) ~ Date.Time, Probe.Data, col = "grey", type="l", ylim=c(7.1, 8.1),  xlab="Time", ylab="pH NBS")
lines(as.numeric(as.character(pH_T3)) ~ Date.Time, Probe.Data, col = "red")
lines(as.numeric(as.character(pH_T4)) ~ Date.Time, Probe.Data, col = "blue")
lines(as.numeric(as.character(pH_T5)) ~ Date.Time, Probe.Data, col = "black")
axis.POSIXct(side=1, Probe.Data$Date.Time)

# plot(as.numeric(as.character(Salt_XL)) ~ Date.Time, Probe.Data, col = "grey", type="l", ylim=c(20, 35),  xlab="Time", ylab="Salinity psu")
# lines(as.numeric(as.character(Salt_L)) ~ Date.Time, Probe.Data, col = "red")
# lines(as.numeric(as.character(Salt_A)) ~ Date.Time, Probe.Data, col = "blue")
# axis.POSIXct(side=1, Probe.Data$Date.Time)
dev.off()