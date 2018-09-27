###################################################################################################
rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(Rmisc)
#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/SDR_data/All_data_csv/Cumulative_output/")
main<-getwd()
# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Summary" #the locationv of all your respiration files

########################
# make the dataframe
########################
dat<-read.csv(file.path(path, ("All_resp_calc_and_standardized.csv")), header=T, sep=",", na.string="NA", as.is=T) 

########################
# seperate into trial 1 and trial 2
########################
# Trial 1
data <-dat
# add a column for row names to identify scatterplot rows
data[,17] <- tibble::rownames_to_column(data, "ROWS")


# Trial 1
TRIAL1 <-dat[1:96,]
tail(TRIAL1, 34)
names(TRIAL1)
# add a column for row names to identify scatterplot rows
TRIAL1[,17] <- tibble::rownames_to_column(TRIAL1, "ROWS")

# Trial 2
TRIAL2 <-dat[97:192,]
tail(TRIAL2, 34)
nrow(TRIAL2)
# add a column for row names to identify scatterplot rows
TRIAL2[,17] <- tibble::rownames_to_column(TRIAL2, "ROWS")

########################
# plotted all automated ouputs ([,2-10]) to the output by hand ([,1])
########################
par(mfrow=c(1,1))


####################################################################################
# ------------------------ALL Data --------------------------------------------------
####################################################################################



# by hand output based on density criteria = "Ref" or [,1]

plot(data[,1],data[,2], main= "Ref vs. alpha0.2_all")#Adjusted R-squared:  0.4987 
summary(lm(data[,1]~data[,2]))
text(data[,1],data[,2], labels=data$ROWS, cex= 0.7, offset=1)
abline(lm(data[,1]~data[,2]))
ggplot(data, aes(data[,1],data[,2])) +
  geom_point() +
  geom_text(aes(label = data$ROWS), position = position_nudge(y = -0.01))

ggplot(data, aes(x = data[,1], y = data[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = data$ROWS), position = position_nudge(y = -0.01))


plot(data[,1],data[,3], main= "Ref vs. alpha0.2_min10.20")#Adjusted R-squared:  0.2892
summary(lm(data[,1]~data[,3]))

plot(data[,1],data[,4], main= "Ref vs. alpha0.2_min10.25")#Adjusted R-squared:  0.1674
summary(lm(data[,1]~data[,4]))
ggplot(data, aes(x = data[,1], y = data[,4])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = data$ROWS), position = position_nudge(y = -0.01))



plot(data[,1],data[,5], main= "Ref vs. alpha0.4_all")#Adjusted R-squared:  0.3905
summary(lm(data[,1]~data[,5]))
library(ggplot2)
ggplot(data, aes(x = data[,1], y = data[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = data$ROWS), position = position_nudge( y = -0.01)) 

# outside of loess curve are 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94,

plot(data[,1],data[,6], main= "Ref vs. alpha0.4_min10.20")#Adjusted R-squared:  0.2589 
summary(lm(data[,1]~data[,6]))
ggplot(data, aes(x = data[,1], y = data[,6])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = data$ROWS), position = position_nudge(y = -0.01))


plot(data[,1],data[,7], main= "Ref vs. alpha0.4_min10.25")#Adjusted R-squared:  0.315 
summary(lm(data[,1]~data[,7]))
ggplot(data, aes(x = data[,1], y = data[,7])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = data$ROWS), position = position_nudge(y = -0.01))

plot(data[,1],data[,8], main= "Ref vs. alpha0.6_all")#Adjusted R-squared:  0.4947 
summary(lm(data[,1]~data[,8]))
ggplot(data, aes(x = data[,1], y = data[,8])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = data$ROWS), position = position_nudge(y = -0.01))

plot(data[,1],data[,9], main= "Ref vs. alpha0.6_min10.20")#Adjusted R-squared:  0.4037 
summary(lm(data[,1]~data[,9]))

plot(data[,1],data[,10], main= "Ref vs. alpha0.6_min10.25")#Adjusted R-squared:  0.4409 
summary(lm(data[,1]~data[,10]))

















####################################################################################
# ------------------------TRIAL1 --------------------------------------------------
####################################################################################


# by hand output based on density criteria = "Ref" or [,1]

plot(TRIAL1[,1],TRIAL1[,2], main= "Ref vs. alpha0.2_all")#Adjusted R-squared:  0.4987 
summary(lm(TRIAL1[,1]~TRIAL1[,2]))
text(TRIAL1[,1],TRIAL1[,2], labels=TRIAL1$ROWS, cex= 0.7, offset=1)
abline(lm(TRIAL1[,1]~TRIAL1[,2]))
ggplot(TRIAL1, aes(TRIAL1[,1],TRIAL1[,2])) +
  geom_point() +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))

ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
 geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))


plot(TRIAL1[,1],TRIAL1[,3], main= "Ref vs. alpha0.2_min10.20")#Adjusted R-squared:  0.2892
summary(lm(TRIAL1[,1]~TRIAL1[,3]))

plot(TRIAL1[,1],TRIAL1[,4], main= "Ref vs. alpha0.2_min10.25")#Adjusted R-squared:  0.1674
summary(lm(TRIAL1[,1]~TRIAL1[,4]))
ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,4])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))



plot(TRIAL1[,1],TRIAL1[,5], main= "Ref vs. alpha0.4_all")#Adjusted R-squared:  0.3905
summary(lm(TRIAL1[,1]~TRIAL1[,5]))
library(ggplot2)
ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))

# outside of loess curve are 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94,
shapiro.test(TRIAL1[,5]) # not normal



plot(TRIAL1[,1],TRIAL1[,6], main= "Ref vs. alpha0.4_min10.20")#Adjusted R-squared:  0.2589 
summary(lm(TRIAL1[,1]~TRIAL1[,6]))
ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,6])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))


plot(TRIAL1[,1],TRIAL1[,7], main= "Ref vs. alpha0.4_min10.25")#Adjusted R-squared:  0.315 
summary(lm(TRIAL1[,1]~TRIAL1[,7]))
ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,7])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))

plot(TRIAL1[,1],TRIAL1[,8], main= "Ref vs. alpha0.6_all")#Adjusted R-squared:  0.4947 
summary(lm(TRIAL1[,1]~TRIAL1[,8]))
ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,8])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))

plot(TRIAL1[,1],TRIAL1[,9], main= "Ref vs. alpha0.6_min10.20")#Adjusted R-squared:  0.4037 
summary(lm(TRIAL1[,1]~TRIAL1[,9]))

plot(TRIAL1[,1],TRIAL1[,10], main= "Ref vs. alpha0.6_min10.25")#Adjusted R-squared:  0.4409 
summary(lm(TRIAL1[,1]~TRIAL1[,10]))


####################################
#-------------TRIAL 1 diagnostics of REF vs. automated outputs
####################################

# alpha 0.2 and all has the closest relationship to the measurements by hand (0.4987)
############
# (1) look for ouliers in the data
##############
# -----Reference
OutVals = boxplot(TRIAL1[,1])$out
which(TRIAL1[,1] %in% OutVals)
# 17 and 29 are ouliers from TRIAL1 by hand reference ouput
# -----alpha = 0.2 all 
OutVals = boxplot(TRIAL1[,2])$out
which(TRIAL1[,2] %in% OutVals)

# rows 17 and 57 are ouliers in alpha = 0.4 all
OutVals = boxplot(TRIAL1[,5])$out
which(TRIAL1[,5] %in% OutVals)

############
# (2) asses the regression and residuals for points that are furtherest from regression
############
# call the reference data and the automated data with strongest correlative strength (alpha=0.2_all)
#trial1_data <- data.frame(TRIAL1$Date , TRIAL1$Resp_individually_all.csv, 
#                TRIAL1$LpcResp_alpha0.2_all.csv)
#regression_trial1 <- lm(TRIAL1$Resp_individually_all.csv~TRIAL1$LpcResp_alpha0.2_all.csv)
#plot(regression_trial1)
#summary(regression_trial1)
#ggqqplot(TRIAL1$LpcResp_alpha0.2_all.csv)
# print and regression_trial1 residuals
#print(regression_trial1)
#plot(residuals(regression_trial1))
# Choose a threshhold
#outlier_threshold <- 0.1 # appears that 0.1 is fitting to narrow the data
# make a new column on the residuals
#trial1_data$residuals <- residuals(regression_trial1)
# Print out the row names of the outliers
#outliers_trial1 <- trial1_data[ abs(trial1_data$residuals) > outlier_threshold, ]
#print(outliers_trial1)


# numbers above residuals above 0.1 are in row 3,6,17,29,34,50,53,55,58,65,71,96 (12 rows)

# 53 and 55 are similar between the two datasheet but ARE NOT outliers!



# in summary - the residuals and outliers show that there are 12 rows that 
# differ most between the reference output (visual criteria) to the automated output
# with highest correlative strength (in this case alpha =0.2 and all data)
# Next step for Trial 1 is to replace these rows with data from a different automated output
# create a new dataset and regress with the reference. Test this new dataset for 
# homogeneity and then run a two way anova



# make a new data frame of just the REF ouput and alpha =0.2 all 
#newdata_TRIAL1 <- data.frame(TRIAL1$Date, TRIAL1$Resp_individually_all.csv, 
#                          TRIAL1$LpcResp_alpha0.2_all.csv, TRIAL1$Treat1_Treat2, TRIAL1$Day_trial)
# omit the rows that had greatest residuals 
#newdata_TRIAL1.OMIT <- newdata_TRIAL1[-c(3,6,17,29,34,50,53,55,58,65,71,96), ]

# omit data based on visual 
#newdata_TRIAL1.OMIT_00 <- newdata_TRIAL1[-c(2,3,6,7,17,29,34,49,50,58,65,70,71,72,92,96), ]
# test the new automated output for homogeneity at column three [,3]
#shapiro.test(newdata_TRIAL1.OMIT_00[,3])
#ggqqplot(newdata_TRIAL1.OMIT_00[,3])

#ggplot(newdata_TRIAL1.OMIT_00, aes(newdata_TRIAL1.OMIT_00[,2],newdata_TRIAL1.OMIT_00[,3])) +
# geom_point() 
# place values of by hand ref to the alpha ref (without the outliers included)
#newdata_TRIAL1[c(2,3,6,7,34,49,50,58,65,70,71,72,92,96),3] <- newdata_TRIAL1[c(2,3,6,7,34,49,50,58,65,70,71,72,92,96), 2]




# test whether removal of outliers ONLY makes the data normally distributed
#x <- newdata_TRIAL1[-c(17,29), ]
# test the new automated output for homogeneity at column three [,3]
#shapiro.test(x[,3])
#ggqqplot(x[,3])


# make a new data frame of just the REF ouput and alpha =0.2 all 
#newdata_TRIAL1_ALL <- data.frame(TRIAL1$Date , TRIAL1[,c(1:10)])
# test for correlative strength in dataset of JUST rows with weak residuals
# newdata_TRIAL1_ALL <-newdata_TRIAL1_ALL[c(3,6,17,29,34,50, 58,65,71,96), ] # 53 and 55 left out becasue these were similar between datasets
# omit the rows that had greatest residuals 
# newdata_TRIAL1_ALL <- newdata_TRIAL1_ALL[-c(3,4), ] # for rows labeled 17 and 29


#make a new dataset based on the number of points ouside of loess curve for alpha =0.4_all
# 19 total numbers are 57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94
newdata_TRIAL1_ALL <- data.frame(TRIAL1$Date, TRIAL1[,c(1:10)])
newdata_TRIAL1_ALL_1 <-newdata_TRIAL1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ]
newdata_TRIAL1_ALL_om <- newdata_TRIAL1_ALL[c(52,3,6,2,1,76,50,96,31,5,70,72,68,56,94), ] #17 29 and 57 omitted

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,3])
summary(lm(newdata_TRIAL1_ALL_1[,3]~newdata_TRIAL1_ALL_1[,2]))#Adjusted R-squared: 0.5145 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,4])
summary(lm(newdata_TRIAL1_ALL_1[,4]~newdata_TRIAL1_ALL_1[,2]))#Adjusted R-squared:  0.6196 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,5])
summary(lm(newdata_TRIAL1_ALL_1[,5]~newdata_TRIAL1_ALL_1[,2]))#  0.07991

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,6])
summary(lm(newdata_TRIAL1_ALL_1[,6]~newdata_TRIAL1_ALL_1[,2]))# 0.0618 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,7])
summary(lm(newdata_TRIAL1_ALL_1[,7]~newdata_TRIAL1_ALL_1[,2]))#0.3223 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,8])
summary(lm(newdata_TRIAL1_ALL_1[,8]~newdata_TRIAL1_ALL_1[,2]))#0.205 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,9])
summary(lm(newdata_TRIAL1_ALL_1[,9]~newdata_TRIAL1_ALL_1[,2]))#0.2356 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,10]) # alpha = 0.6 10-20 min
summary(lm(newdata_TRIAL1_ALL_1[,10]~newdata_TRIAL1_ALL_1[,2]))# 0.7235 

plot(newdata_TRIAL1_ALL_1[,2],newdata_TRIAL1_ALL_1[,11])
summary(lm(newdata_TRIAL1_ALL_1[,11]~newdata_TRIAL1_ALL_1[,2]))#  0.3334 

# insert values for rows (3,6,17,29,34,50, 58,65,71,96) from newdata_TRIAL1_ALL[,11]
# to alpha = 0.2 all - this data set is from the automated output alpha 0.6 at 10-25 mins
# and omit rows 17 and 29

# replace the rows (3,6, 34,50, 58,65,71,96)in alpha 0.2 all with corresponding rows from 
# alpha 0.6 10-25min 
# rows to replace
#newdata_TRIAL1[c(3,6, 34,50, 58,65,71,96), 3]
# rows to integrate
#newdata_TRIAL1_ALL[c(1:8),11]
#newdata_TRIAL1_ALL[c(1:8),2] # compare with the original reference data
# asigned values from 0.6 10-25 to  new rows within 0.2 all
#newdata_TRIAL1[c(3,6, 34,50, 58,65,71,96), 3] <- newdata_TRIAL1_ALL[c(1:8),11]

# objective now is to replace the values in the 18 rows from alpha 0.6 10-20mins
# with the rows in alpha 0.4_all
# WHY?? Alpha0.4 all yielded the leaset number of points outside of loess curve in relation
#with the resp values completed by hand, the values outside of the loess curve had 
#strongest correlative strength with the by hand values at the 0.6 10-20min resp output

# TRIAL1[,5] =  alpha 0.4_all
# TRIAL1[,9] = alpha 0.6_10-20mins
#assgin new  "FINAL" column base as the alpha 0.4 all
TRIAL1$FINALresp <- TRIAL1[,5]
TRIAL1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94),18] <- TRIAL1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), 9]


# normality test 
shapiro.test(TRIAL1[,18]) # not normal
hist(TRIAL1[,18]) # positive or right skewed - need to transform the data

# transform via squareroot gets a normal distrubtion via shapro wilk test
TRIAL1$sqrt_FINALresp <- sqrt(TRIAL1[,18])
shapiro.test(TRIAL1[,19]) # not normal
hist(TRIAL1[,19])

# regress the new data column FINALresp with the column of resp by hand ("REF") TRIAL1[,1]
# lets look at a plot of our raw untransformed FINALresp data TABLE1[,18]
plot(TRIAL1[,1],TRIAL1[,18], main= "Ref vs. FINALresp")
summary(lm(TRIAL1[,1]~TRIAL1[,18])) # Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 
library(ggplot2)
ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,18])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))

# get a table of means and deviations
sumTRIAL1 <- summarySE(TRIAL1, 
                       measurevar="FINALresp", 
                       groupvars=c("Date","Treat1_Treat2"))
sumTRIAL1


# get percent difference
sumTRIAL1_means <- summarySE(sumTRIAL1, 
                             measurevar="FINALresp", 
                             groupvars=c("Treat1_Treat2"))
sumTRIAL1_means

percentdiff <- ((sumTRIAL1_means[1,3] - sumTRIAL1_means[2,3])/sumTRIAL1_means[1,3])*100
percentdiff

##################################################
# two way anova test FINALresp transformed (MUST BE NORMALLY DISTRIBUTED)
##################################################

# --------------------------treatment as a fixed effect and time as a random effect
resp.aov_trial1 <- aov(TRIAL1$sqrt_FINALresp ~ 
                         TRIAL1$Treat1_Treat2+ 
                         TRIAL1$Date, data = TRIAL1)

summary(resp.aov_trial1) # significant effect of treatment
"""Df Sum Sq Mean Sq F value Pr(>F)  
TRIAL1$Treat1_Treat2  1 0.1121 0.11208   6.913  0.010 *
TRIAL1$Date           1 0.0051 0.00507   0.313  0.577  
Residuals            93 1.5079 0.01621                 
"""

# ---------------------------treatment as a fixed effect and time as a fixed effect
resp.aov_trial1_2 <- aov(TRIAL1$sqrt_FINALresp ~ 
                         TRIAL1$Treat1_Treat2* 
                         TRIAL1$Date, data = TRIAL1)

summary(resp.aov_trial1_2) # significant effect of treatment
""" Df Sum Sq Mean Sq F value Pr(>F)  
TRIAL1$Treat1_Treat2              1 0.1121 0.11208   6.839 0.0104 *
TRIAL1$Date                       1 0.0051 0.00507   0.309 0.5794  
TRIAL1$Treat1_Treat2:TRIAL1$Date  1 0.0002 0.00020   0.012 0.9127  
Residuals                        92 1.5077 0.01639                 
"""

# ---------------------------treatment as a fixed effect and time as a fixed effect, tank as a random effect
resp.aov_trial1_3 <- aov(TRIAL1$sqrt_FINALresp ~ 
                           TRIAL1$Treat1_Treat2* 
                           TRIAL1$Date+TRIAL1$tank, data = TRIAL1)

summary(resp.aov_trial1_3) # significant effect of treatment and time
"""Df Sum Sq Mean Sq F value  Pr(>F)   
TRIAL1$Treat1_Treat2              1 0.1121 0.11208   7.931 0.00602 **
TRIAL1$Date                       1 0.0051 0.00507   0.359 0.55078   
TRIAL1$tank                       6 0.2924 0.04873   3.449 0.00423 **
TRIAL1$Treat1_Treat2:TRIAL1$Date  1 0.0002 0.00020   0.014 0.90603   
Residuals                        86 1.2153 0.01413  """

##################################
# plots
##################################

#plot treatments and time
TRIAL1_plot <- ggplot(TRIAL1, aes(x = factor(TRIAL1$Date), y = TRIAL1$FINALresp, fill = TRIAL1$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = TRIAL1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(TRIAL1_plot + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                         x = "Date") + 
  ggtitle("Juvenile geoduck respirometry \nin OA treatments TRIAL1"))

# plot just treatment 
TRIAL1_plot_2 <- ggplot(TRIAL1, aes(x = factor(TRIAL1$Treat1_Treat2), y = TRIAL1$FINALresp, fill = TRIAL1$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = TRIAL1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(TRIAL1_plot_2 + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                         x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments TRIAL1"))



#####################################################################################
# ------------------------TRIAL2 --------------------------------------------------
#####################################################################################


# in reference to the individually completely TRIAL2aset
plot(TRIAL2[,1],TRIAL2[,2], main= "Ref vs. alpha0.2_all") #Adjusted R-squared:  0.4402 
summary(lm(TRIAL2[,1]~TRIAL2[,2]))
text(TRIAL2[,1],TRIAL2[,2], labels=TRIAL1$ROWS, cex= 0.7, offset=1)
ggplot(TRIAL1, aes(TRIAL2[,1],TRIAL2[,2])) +
  geom_point() +
  geom_text(aes(label = TRIAL2$ROWS), position = position_nudge(y = -0.01))
# appears that rows 114,146,155,186,109,123,182 have greatest residuals or are outiers
ggplot(TRIAL2, aes(x = TRIAL2[,1], y = TRIAL2[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL2$ROWS), position = position_nudge(y = -0.01))

plot(TRIAL2[,1],TRIAL2[,3], main= "Ref vs. alpha0.2_min10.20") #Adjusted R-squared:  0.4612 
summary(lm(TRIAL2[,1]~TRIAL2[,3]))

plot(TRIAL2[,1],TRIAL2[,4], main= "Ref vs. alpha0.2_min10.25")#Adjusted R-squared:  0.5053 
summary(lm(TRIAL2[,1]~TRIAL2[,4]))

plot(TRIAL2[,1],TRIAL2[,5], main= "Ref vs. alpha0.4_all")#Adjusted R-squared:  0.8002 
summary(lm(TRIAL2[,1]~TRIAL2[,5]))
ggplot(TRIAL2, aes(x = TRIAL2[,1], y = TRIAL2[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL2$ROWS), position = position_nudge(y = .01))

# numbers outside of loess curve are 114, 153,155,156,150,141,105,130,152,98,188 (take out 186)
numberrows <- (c(114,153,155,156,150,141,105,130,152,98,188)) - 96
# actuall number of rows are 18 57 59 60 54 45  9 34 56  2 92
#check values
TRIAL2[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), 5]

plot(TRIAL2[,1],TRIAL2[,6], main= "Ref vs. alpha0.4_min10.20")#Adjusted R-squared:  0.5743 
summary(lm(TRIAL2[,1]~TRIAL2[,6]))

plot(TRIAL2[,1],TRIAL2[,7], main= "Ref vs. alpha0.4_min10.25")#Adjusted R-squared:  0.4952 
summary(lm(TRIAL2[,1]~TRIAL2[,7]))

plot(TRIAL2[,1],TRIAL2[,8], main= "Ref vs. alpha0.6_all")#Adjusted R-squared:  0.4371 
summary(lm(TRIAL2[,1]~TRIAL2[,8]))

plot(TRIAL2[,1],TRIAL2[,9], main= "Ref vs. alpha0.6_min10.20")#Adjusted R-squared:  0.6022
summary(lm(TRIAL2[,1]~TRIAL2[,9]))

plot(TRIAL2[,1],TRIAL2[,10], main= "Ref vs. alpha0.6_min10.25")#Adjusted R-squared:  0.5896 
summary(lm(TRIAL2[,1]~TRIAL2[,10]))


####################################
#-------------TRIAL 2 diagnostics of REF vs. automated outputs
####################################

# alpha 0.2 and all has the closest relationship to the measurements by hand (0.4987)
############
# (1) look for ouliers in the data
##############
# -----Reference
# OutVals2 = boxplot(TRIAL2[,1])$out
# which(TRIAL2[,1] %in% OutVals2)
# ggqqplot(TRIAL2[,1])
# (c(83,90))+96 # take the outliers output and add 96 (rows from trial 1) to ID the row in the entire dataset 
# 179 and 186 are ouliers from TRIAL2 by hand reference ouput
# -----alpha = 0.2 all 
# OutVals2 = boxplot(TRIAL2[,2])$out
# which(TRIAL2[,2] %in% OutVals2)
# ggqqplot(TRIAL2[,2])
# (c(13, 27, 40, 83, 86))+96 # take the outliers output and add 96 (rows from trial 1) to ID the row in the entire dataset 
# ROWS 109 123 136 179 182 are ouliers from TRIAL2 alpha = 0.2 all


############
# (2) asses the regression and residuals for points that are furtherest from regression
############
# call the reference data and the automated data with strongest correlative strength (alpha=0.2_all)
# trial2_data <- data.frame(TRIAL2$Date , TRIAL2$Resp_individually_all.csv, 
#   TRIAL2$LpcResp_alpha0.2_all.csv)
# regression_trial2 <- lm(TRIAL2$Resp_individually_all.csv~TRIAL2$LpcResp_alpha0.2_all.csv)
# plot(regression_trial2)
#ggqqplot(TRIAL1$LpcResp_alpha0.2_all.csv)
# print and regression_trial1 residuals
# print(regression_trial2)
# plot(residuals(regression_trial2))
# Choose a threshhold
# outlier_threshold_2 <- 0.1 # appears that 0.1 is fitting to narrow the data
# make a new column on the residuals
# trial2_data$residuals <- residuals(regression_trial2)
# Print out the row names of the outliers
# outliers_trial2 <- trial2_data[ abs(trial2_data$residuals) > outlier_threshold_2, ]
# print(outliers_trial2)

# oulierrownumbers <- (c(11,18,27,50,59,86,90))+96
# oulierrownumbers
# numbers above residuals above 0.1 are in rows 107 114 123 146 155 182 186 (7 rows)


# in summary - the residuals and outliers show that there are 7 rows that 
# differ most between the reference output (visual criteria) to the automated output
# with highest correlative strength (in this case alpha =0.2 and all data)
# Next step for Trial 2 is to replace these rows with data from a different automated output
# create a new dataset and regress with the reference. Test this new dataset for 
# homogeneity and then run a two way anova





# make a new data frame of just the REF ouput and alpha =0.2 all 
newdata_TRIAL2_ALL <- data.frame(TRIAL2$Date , TRIAL2[,c(1:10)])
# test for correlative strength in dataset of JUST rows with weak residuals
newdata_TRIAL2_ALL_1 <-newdata_TRIAL2_ALL[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), ] 
# omit the rows that had greatest residuals 
#newdata_TRIAL2_ALL <- newdata_TRIAL2_ALL[-c(3,4), ] # for rows labeled 17 and 29

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,3])
summary(lm(newdata_TRIAL2_ALL_1[,3]~newdata_TRIAL2_ALL_1[,2]))#Adjusted R-squared: 0.4139 

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,4])
summary(lm(newdata_TRIAL2_ALL_1[,4]~newdata_TRIAL2_ALL_1[,2]))#Adjusted R-squared:  0.5674  

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,5])
summary(lm(newdata_TRIAL2_ALL_1[,5]~newdata_TRIAL2_ALL_1[,2]))#  0.425 

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,6])
summary(lm(newdata_TRIAL2_ALL_1[,6]~newdata_TRIAL2_ALL_1[,2]))# -0.02776  

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,7])
summary(lm(newdata_TRIAL2_ALL_1[,7]~newdata_TRIAL2_ALL_1[,2]))#0.5682 

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,8])
summary(lm(newdata_TRIAL2_ALL_1[,8]~newdata_TRIAL2_ALL_1[,2]))#00.4892 

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,9])
summary(lm(newdata_TRIAL2_ALL_1[,9]~newdata_TRIAL2_ALL_1[,2]))#-0.4071 

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,10])
summary(lm(newdata_TRIAL2_ALL_1[,10]~newdata_TRIAL2_ALL_1[,2]))#0.5931

plot(newdata_TRIAL2_ALL_1[,2],newdata_TRIAL2_ALL_1[,11]) # alpha = 0.6 10 - 25 mins
summary(lm(newdata_TRIAL2_ALL_1[,11]~newdata_TRIAL2_ALL_1[,2]))# 0.71699  !!!! use this!


# objective now is to replace the values in the 11 rows from alpha = 0.6 10 - 25 mins
# with the rows in alpha 0.4_all
# WHY?? Alpha0.4 all yielded the leaset number of points outside of loess curve in relation
#with the resp values completed by hand, the values outside of the loess curve had 
#strongest correlative strength with the by hand values at the 0.6 10-25min resp output

# TRIAL1[,5] =  alpha 0.4_all
# TRIAL1[,10] = alpha 0.6_10-25mins
#assgin new  "FINAL" column base as the alpha 0.4 all

#assgin new  "FINAL" column base as the alpha 0.4 all
# TRIAL2[,5] =0.4 all 
TRIAL2$FINALresp <- TRIAL2[,5]
# FINAresp is TRIAL2[,18]
# insert the rows to replace with 0.6 10-25mins
TRIAL2[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92),18] <- TRIAL2[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), 10]


# normality test 
shapiro.test(TRIAL2[,18]) # not normal - ommit the data from row 186
hist(TRIAL2[,18]) # positive or right skewed - need to transform the data

OutVals2 = boxplot(TRIAL2[,18])$out
which(TRIAL2[,18] %in% OutVals2) # 83 and 90 are outliers

TRIAL2_om <- TRIAL2[-c(83,90),]
shapiro.test(TRIAL2_om[,18]) # normally distributed
hist(TRIAL2_om[,18])

# regress the new data column FINALresp with the column of resp by hand ("REF") TRIAL1[,1]
# lets look at a plot of our raw untransformed FINALresp data TABLE1[,18]
plot(TRIAL2_om[,1],TRIAL2_om[,18], main= "Ref vs. FINALresp")
summary(lm(TRIAL2_om[,1]~TRIAL2_om[,18])) # Multiple R-squared:  0.9107,	Adjusted R-squared:  0.9097 
library(ggplot2)
ggplot(TRIAL2_om, aes(x = TRIAL2_om[,1], y = TRIAL2_om[,18])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL2_om$ROWS), position = position_nudge(y = -0.01))

# get a table of means and deviations
names(TRIAL2_om)
sumTRIAL2 <- summarySE(TRIAL2_om, 
                       measurevar="FINALresp", 
                       groupvars=c("Date","Treat1_Treat2"))
sumTRIAL2 # view the table

# get percent difference
sumTRIAL2_means <- summarySE(sumTRIAL2, 
                             measurevar="FINALresp", 
                             groupvars=c("Treat1_Treat2"))
sumTRIAL2_means


##################################################
# two way anova test FINALresp transformed (MUST BE NORMALLY DISTRIBUTED)
##################################################

# --------------------------treatment as a fixed effect and time as a random effect
resp.aov_TRIAL2 <- aov(FINALresp ~ 
                         Treat1_Treat2 + 
                         Date, data = TRIAL2_om)
summary(resp.aov_TRIAL2) # significant effect of treatment
"""Df Sum Sq Mean Sq F value Pr(>F)  
Treat1_Treat2  3 0.1020 0.03399   3.852 0.0121 *
Date           1 0.0059 0.00591   0.669 0.4154  
Residuals     89 0.7854 0.00882"""
# no significant effect of treatment and time but there is an effect of treatment
# unlike the previous trial, we must to a post hoc Tukey test to determine where this
# difference lies

# ---------------------------treatment as a fixed effect and time as a fixed effect
resp.aov_TRIAL2_2 <- aov(FINALresp ~ 
                           Treat1_Treat2*Date, data = TRIAL2_om)

summary(resp.aov_TRIAL2_2) # significant effect of treatment
"""  Df Sum Sq Mean Sq F value Pr(>F)  
Treat1_Treat2  3 0.1020 0.03399   3.852 0.0121 *
Date           1 0.0059 0.00591   0.669 0.4154  
Residuals     89 0.7854 0.00882                 
---                
"""

# ---------------------------treatment as a fixed effect and time as a fixed effect, tank as a random effect
resp.aov_TRIAL2_3 <- aov(FINALresp ~ 
                           Treat1_Treat2 * 
                           Date +
                           tank, data = TRIAL2_om)

summary(resp.aov_TRIAL2_3) # significant effect of treatment and time
"""  Df Sum Sq Mean Sq F value   Pr(>F)    
Treat1_Treat2  3 0.1020 0.03399   5.040  0.00291 ** 
Date           1 0.0059 0.00591   0.876  0.35196    
tank           4 0.2121 0.05303   7.862 1.93e-05 ***
Residuals     85 0.5733 0.00674                     
--- """

#

modelTRIAL2 <- aov(FINALresp ~ 
                     Treat1_Treat2, data = TRIAL2_om)
summary(modelTRIAL2)
TukeyHSD(modelTRIAL2)
""" diff         lwr          upr     p adj
Ambient_Low-Ambient_Ambient -0.006116942 -0.07773911  0.065505225 0.9960306
Low_Ambient-Ambient_Ambient -0.032556095 -0.10493619  0.039823999 0.6425986
Low_Low-Ambient_Ambient     -0.083028745 -0.15465091 -0.011406578 0.0163398 !!!!! diff
Low_Ambient-Ambient_Low     -0.026439153 -0.09806132  0.045183013 0.7688677
Low_Low-Ambient_Low         -0.076911803 -0.14776794 -0.006055671 0.0279431 !!!! diff
Low_Low-Low_Ambient         -0.050472649 -0.12209482  0.021149517 0.2594972"""

# check the percent change between low low and amb amb
sumTRIAL2_means
percent2_LL_AA <- ((sumTRIAL2_means[1,3] - sumTRIAL2_means[4,3])/sumTRIAL1_means[1,3])*100
percent2_LL_AA
# check the percent change between low low and amb low
percent2_LL_AL <- ((sumTRIAL2_means[2,3] - sumTRIAL2_means[4,3])/sumTRIAL1_means[2,3])*100
percent2_LL_AL

# as seen here the significant difference lies between the responses 
# of LOW-LOW with AMBIENT-AMBIENT (same as trial 1) and LOW-LOW
# with AMBIENT-LOW. This interestingly shows that the response to low pH
# upon second exposure is lower than taht of first exposure.
# this also supports recovery for animals exposed to AMBIENT after the initial LOW
# treatment. metabolism slows in acidified conditions WITHOUT a carry-over effect
# considering that all animals survived, this info can be explored further
# for resiliance effects


##################################
# plots
#################################

par(mfrow=c(2,1))
#plot treatments with time

TRIAL2_plot <- ggplot(TRIAL2_om, aes(x = factor(TRIAL2_om$Date), y = TRIAL2_om$FINALresp, fill = TRIAL2_om$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = TRIAL2_om$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(TRIAL2_plot + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                         x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments TRIAL2"))



# plot just treatment 
TRIAL2_plot_2 <- ggplot(TRIAL2_om, aes(x = factor(TRIAL2_om$Treat1_Treat2), y = TRIAL2_om$FINALresp, fill = TRIAL2_om$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = TRIAL2_om$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(TRIAL2_plot_2 + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                           x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments TRIAL2"))


