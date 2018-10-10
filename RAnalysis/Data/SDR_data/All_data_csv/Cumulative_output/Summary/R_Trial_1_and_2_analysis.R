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

####################################
#-------------TRIAL 1 REF vs. automated outputs
####################################

# by hand output based on density criteria = "Ref" or [,1]

plot(TRIAL1[,1],TRIAL1[,2], main= "Ref vs. alpha0.2_all")#Adjusted R-squared:  0.4987 
summary(lm(TRIAL1[,1]~TRIAL1[,2]))

ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
 geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))


plot(TRIAL1[,1],TRIAL1[,3], main= "Ref vs. alpha0.2_min10.20")#Adjusted R-squared:  0.2892
summary(lm(TRIAL1[,1]~TRIAL1[,3]))

plot(TRIAL1[,1],TRIAL1[,4], main= "Ref vs. alpha0.2_min10.25")#Adjusted R-squared:  0.1674
summary(lm(TRIAL1[,1]~TRIAL1[,4]))

plot(TRIAL1[,1],TRIAL1[,5], main= "Ref vs. alpha0.4_all")#Adjusted R-squared:  0.3905
summary(lm(TRIAL1[,1]~TRIAL1[,5]))

ggplot(TRIAL1, aes(x = TRIAL1[,1], y = TRIAL1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = TRIAL1$ROWS), position = position_nudge(y = -0.01))

# outside of loess curve are 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94,


plot(TRIAL1[,1],TRIAL1[,6], main= "Ref vs. alpha0.4_min10.20")#Adjusted R-squared:  0.2589 
summary(lm(TRIAL1[,1]~TRIAL1[,6]))

plot(TRIAL1[,1],TRIAL1[,7], main= "Ref vs. alpha0.4_min10.25")#Adjusted R-squared:  0.315 
summary(lm(TRIAL1[,1]~TRIAL1[,7]))

plot(TRIAL1[,1],TRIAL1[,8], main= "Ref vs. alpha0.6_all")#Adjusted R-squared:  0.4947 
summary(lm(TRIAL1[,1]~TRIAL1[,8]))

plot(TRIAL1[,1],TRIAL1[,9], main= "Ref vs. alpha0.6_min10.20")#Adjusted R-squared:  0.4037 
summary(lm(TRIAL1[,1]~TRIAL1[,9]))

plot(TRIAL1[,1],TRIAL1[,10], main= "Ref vs. alpha0.6_min10.25")#Adjusted R-squared:  0.4409 
summary(lm(TRIAL1[,1]~TRIAL1[,10]))




# rows 17 and 57 are ouliers in alpha = 0.4 all
OutVals = boxplot(TRIAL1[,5])$out
which(TRIAL1[,5] %in% OutVals)



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


#assgin new  "FINAL" column base as the alpha 0.4 all and inserted points from alpha = 0.6 10-20 min
TRIAL1$FINALresp <- TRIAL1[,5]
TRIAL1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94),18] <- TRIAL1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), 9]


# normality test 
shapiro.test(TRIAL1[,18]) # not normal
hist(TRIAL1[,18]) # positive or right skewed - need to transform the data

# transform via squareroot gets a normal distrubtion via shapro wilk test
TRIAL1$sqrt_FINALresp <- sqrt(TRIAL1[,18])
shapiro.test(TRIAL1[,19]) #  normal
hist(TRIAL1[,19])

# regress the new data column FINALresp with the column of resp by hand ("REF") TRIAL1[,1]
# lets look at a plot of our raw untransformed FINALresp data TABLE1[,18]
plot(TRIAL1[,1],TRIAL1[,18], main= "Ref vs. FINALresp")
summary(lm(TRIAL1[,1]~TRIAL1[,18])) # Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 

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
# get percent difference
percentdiff <- ((sumTRIAL1_means[1,3] - sumTRIAL1_means[2,3])/sumTRIAL1_means[1,3])*100
percentdiff

##################################################
# mixed effect model
##################################################
# lme---------------

#lme_resp_trial1 <- lmer(sqrt_FINALresp ~ Treat1_Treat2 + (1|Date/Treat1_Treat2/tank), data = TRIAL1)

lme_resp_trial1 <- lme(sqrt_FINALresp ~ Treat1_Treat2, random=~1|Date/Treat1_Treat2/tank, data = TRIAL1)

shapiro.test(residuals(lme_resp_trial1))
histogram(residuals(lme_resp_trial1))
summary(lme_resp_trial1)
anova(lme_resp_trial1)


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

####################################
#------------- REF vs. automated outputs
####################################

# in reference to the individually completely TRIAL2aset
plot(TRIAL2[,1],TRIAL2[,2], main= "Ref vs. alpha0.2_all") #Adjusted R-squared:  0.4402 
summary(lm(TRIAL2[,1]~TRIAL2[,2]))

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
# mixed effects models
##################################################
# lme---------------

#lme_resp_trial2 <- lmer(FINALresp ~ Treat1_Treat2 + (1|Date/Treat1_Treat2/tank), data = TRIAL2_om)

lme_resp_trial2 <- lme(FINALresp ~ Treat1_Treat2, random=~1|Date/Treat1_Treat2/tank, data = TRIAL2_om)

shapiro.test(residuals(lme_resp_trial2))
histogram(residuals(lme_resp_trial2))
summary(lme_resp_trial2)
anova(lme_resp_trial2)





#modelTRIAL2 <- aov(FINALresp ~ Treat1_Treat2, data = TRIAL2_om)
#summary(modelTRIAL2)
#TukeyHSD(modelTRIAL2)
""" diff         lwr          upr     p adj
Ambient_Low-Ambient_Ambient -0.006116942 -0.07773911  0.065505225 0.9960306
Low_Ambient-Ambient_Ambient -0.032556095 -0.10493619  0.039823999 0.6425986
Low_Low-Ambient_Ambient     -0.083028745 -0.15465091 -0.011406578 0.0163398 !!!!! diff
Low_Ambient-Ambient_Low     -0.026439153 -0.09806132  0.045183013 0.7688677
Low_Low-Ambient_Low         -0.076911803 -0.14776794 -0.006055671 0.0279431 !!!! diff
Low_Low-Low_Ambient         -0.050472649 -0.12209482  0.021149517 0.2594972"""

# check the percent change between low low and amb amb
#sumTRIAL2_means
#percent2_LL_AA <- ((sumTRIAL2_means[1,3] - sumTRIAL2_means[4,3])/sumTRIAL1_means[1,3])*100
#percent2_LL_AA
# check the percent change between low low and amb low
#percent2_LL_AL <- ((sumTRIAL2_means[2,3] - sumTRIAL2_means[4,3])/sumTRIAL1_means[2,3])*100
#percent2_LL_AL

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


