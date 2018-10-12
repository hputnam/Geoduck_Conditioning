###################################################################################################
rm(list=ls())
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(nlme)
library(lme4)
##Install and load packages
library(ggplot2) 
library(lme4)
library(ggpubr)
library(nlme)
library(plotrix)
library(lsmeans)
library(gridExtra)
#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/") #set working

#Load Size Data
resp<-read.csv("Data/All_resp_calc_and_standardized.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(resp)

########################
# seperate into trial 1 and trial 2
########################
resp_EXP1 <- subset(resp, EXP.numb=="EXP1")
resp_EXP2 <- subset(resp, EXP.numb=="EXP2")
resp_EXP2.0 <- subset(resp, EXP.numb=="EXP2")
resp_EXP2.0 <-subset(resp_EXP2.0, Day!=0)


#-------------TRIAL 1 REF vs. automated outputs
# determined that alpha 0.4 and all data yielded least number of points outside loess curve (reg with REF)
#################################### 


# by hand output based on density criteria = "Ref" or [,1]

plot(resp_EXP1[,1],resp_EXP1[,2], main= "Ref vs. alpha0.2_all")#Adjusted R-squared:  0.4987 
summary(lm(resp_EXP1[,1]~resp_EXP1[,2]))

ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
 geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))


plot(resp_EXP1[,1],resp_EXP1[,3], main= "Ref vs. alpha0.2_min10.20")#Adjusted R-squared:  0.2892
summary(lm(resp_EXP1[,1]~resp_EXP1[,3]))

plot(resp_EXP1[,1],resp_EXP1[,4], main= "Ref vs. alpha0.2_min10.25")#Adjusted R-squared:  0.1674
summary(lm(resp_EXP1[,1]~resp_EXP1[,4]))

plot(resp_EXP1[,1],resp_EXP1[,5], main= "Ref vs. alpha0.4_all")#Adjusted R-squared:  0.3905
summary(lm(resp_EXP1[,1]~resp_EXP1[,5]))

ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))

# outside of loess curve are 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94,


plot(resp_EXP1[,1],resp_EXP1[,6], main= "Ref vs. alpha0.4_min10.20")#Adjusted R-squared:  0.2589 
summary(lm(resp_EXP1[,1]~resp_EXP1[,6]))

plot(resp_EXP1[,1],resp_EXP1[,7], main= "Ref vs. alpha0.4_min10.25")#Adjusted R-squared:  0.315 
summary(lm(resp_EXP1[,1]~resp_EXP1[,7]))

plot(resp_EXP1[,1],resp_EXP1[,8], main= "Ref vs. alpha0.6_all")#Adjusted R-squared:  0.4947 
summary(lm(resp_EXP1[,1]~resp_EXP1[,8]))

plot(resp_EXP1[,1],resp_EXP1[,9], main= "Ref vs. alpha0.6_min10.20")#Adjusted R-squared:  0.4037 
summary(lm(resp_EXP1[,1]~resp_EXP1[,9]))

plot(resp_EXP1[,1],resp_EXP1[,10], main= "Ref vs. alpha0.6_min10.25")#Adjusted R-squared:  0.4409 
summary(lm(resp_EXP1[,1]~resp_EXP1[,10]))

# ID 17 and 57 are ouliers in alpha = 0.4 all
OutVals = boxplot(resp_EXP1[,5])$out
which(resp_EXP1[,5] %in% OutVals)

#make a new dataset based on the number of points ouside of loess curve for alpha =0.4_all
#####
# 19 total numbers are 57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94
newdata_resp_EXP1_ALL <- data.frame(resp_EXP1$Date, resp_EXP1[,c(1:10)])
newdata_resp_EXP1_ALL_1 <-newdata_resp_EXP1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ]
newdata_resp_EXP1_ALL_om <- newdata_resp_EXP1_ALL[c(52,3,6,2,1,76,50,96,31,5,70,72,68,56,94), ] #17 29 and 57 omitted

#####
# plot the new subsetted dataset 
plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,3])
summary(lm(newdata_resp_EXP1_ALL_1[,3]~newdata_resp_EXP1_ALL_1[,2]))#Adjusted R-squared: 0.5145 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,4])
summary(lm(newdata_resp_EXP1_ALL_1[,4]~newdata_resp_EXP1_ALL_1[,2]))#Adjusted R-squared:  0.6196 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,5])
summary(lm(newdata_resp_EXP1_ALL_1[,5]~newdata_resp_EXP1_ALL_1[,2]))#  0.07991

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,6])
summary(lm(newdata_resp_EXP1_ALL_1[,6]~newdata_resp_EXP1_ALL_1[,2]))# 0.0618 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,7])
summary(lm(newdata_resp_EXP1_ALL_1[,7]~newdata_resp_EXP1_ALL_1[,2]))#0.3223 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,8])
summary(lm(newdata_resp_EXP1_ALL_1[,8]~newdata_resp_EXP1_ALL_1[,2]))#0.205 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,9])
summary(lm(newdata_resp_EXP1_ALL_1[,9]~newdata_resp_EXP1_ALL_1[,2]))#0.2356 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,10]) # alpha = 0.6 10-20 min
summary(lm(newdata_resp_EXP1_ALL_1[,10]~newdata_resp_EXP1_ALL_1[,2]))# 0.7235 

plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,11])
summary(lm(newdata_resp_EXP1_ALL_1[,11]~newdata_resp_EXP1_ALL_1[,2]))#  0.3334 

#####
#assgin new  "FINAL" column base as the alpha 0.4 all and inserted points from alpha = 0.6 10-20 min
resp_EXP1$FINALresp <- resp_EXP1[,5]
resp_EXP1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94),22] <- resp_EXP1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), 9]

# normality test 
shapiro.test(resp_EXP1$FINALresp) # not normal
hist(resp_EXP1$FINALresp) # positive or right skewed - need to transform the data

# transform via squareroot gets a normal distrubtion via shapro wilk test
resp_EXP1$sqrt_FINALresp <- sqrt(resp_EXP1$FINALresp)
shapiro.test(resp_EXP1$sqrt_FINALres) #  not normal
hist(resp_EXP1$sqrt_FINALres)

#####
# regress the new data column FINALresp with the column of resp by hand ("REF") resp_EXP1[,1]
# lets look at a plot of our raw untransformed FINALresp data TABLE1[,18]
plot(resp_EXP1[,1],resp_EXP1$FINALresp, main= "Ref vs. FINALresp")
summary(lm(resp_EXP1[,1]~resp_EXP1$FINALresp)) # Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 

ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))



# mixed effect model
##################################################

# Visualize experiment 1
names(resp_EXP1)
Exp1.Fig.resp <- ggboxplot(resp_EXP1, x = "Day", y = "FINALresp", color = "Init.treat", ylab= "respiration",
                      palette = c("blue", "red"), main= "Initial Exposure")

Exp1.Fig.resp

x1 <- do.call(data.frame,aggregate(FINALresp ~ Day*Init.treat, data = resp_EXP1, function(x) c(mean = mean(x), se = std.error(x))))

# Test for respiration differences in experiment 1
Init.lme.resp <- lmer(FINALresp ~ Init.treat*Day + (1|Day), data = resp_EXP1) #use untransformed data for model - test residuals for normality
anova(Init.lme.resp)
summary(Init.lme.resp)

m1.resp <- lme(FINALresp~Init.treat*Day,random=~1|Day,data=resp_EXP1)
anova(m1.resp)


par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m1.resp)) #plot histogram of residuals
shapiro.test(residuals(m1.resp)) # residuals are normal
boxplot(residuals(m1.resp)) #plot boxplot of residuals
plot( fitted(m1.resp),residuals(m1.resp)) #display residuals versus fitter, normal QQ plot, leverage plot

# SIGNIFICANT DIFFERENCE FROM INITIAL EXPOSURE

# get a table of means and deviations to explore percent difference Low to Ambient in Experiment 1
sumresp_EXP1 <- summarySE(resp_EXP1, 
                       measurevar="FINALresp", 
                       groupvars=c("Date","Treat1_Treat2"))
sumresp_EXP1

# get percent difference
sumresp_EXP1_means <- summarySE(sumresp_EXP1, 
                             measurevar="FINALresp", 
                             groupvars=c("Treat1_Treat2"))
sumresp_EXP1_means
# get percent difference
percentdiff <- ((sumresp_EXP1_means[1,3] - sumresp_EXP1_means[2,3])/sumresp_EXP1_means[1,3])*100
percentdiff


# 25% lower respiration rates in low pH

# plots
##################################

#plot treatments and time
resp_EXP1_plot <- ggplot(resp_EXP1, aes(x = factor(resp_EXP1$Date), y = resp_EXP1$FINALresp, fill = resp_EXP1$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = resp_EXP1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_EXP1_plot + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                         x = "Date") + 
  ggtitle("Juvenile geoduck respirometry \nin OA treatments resp_EXP1"))

# plot just treatment 
resp_EXP1_plot_2 <- ggplot(resp_EXP1, aes(x = factor(resp_EXP1$Treat1_Treat2), y = resp_EXP1$FINALresp, fill = resp_EXP1$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = resp_EXP1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_EXP1_plot_2 + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                         x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments resp_EXP1"))



# ------------------------resp_EXP2 --------------------------------------------------
#####################################################################################


#------------- REF vs. automated outputs
####################################

# in reference to the individually completely resp_EXP2aset
plot(resp_EXP2[,1],resp_EXP2[,2], main= "Ref vs. alpha0.2_all") #Adjusted R-squared:  0.4402 
summary(lm(resp_EXP2[,1]~resp_EXP2[,2]))

# appears that ID 114,146,155,186,109,123,182 have greatest residuals or are outiers
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = -0.01))

plot(resp_EXP2[,1],resp_EXP2[,3], main= "Ref vs. alpha0.2_min10.20") #Adjusted R-squared:  0.4612 
summary(lm(resp_EXP2[,1]~resp_EXP2[,3]))

plot(resp_EXP2[,1],resp_EXP2[,4], main= "Ref vs. alpha0.2_min10.25")#Adjusted R-squared:  0.5053 
summary(lm(resp_EXP2[,1]~resp_EXP2[,4]))

plot(resp_EXP2[,1],resp_EXP2[,5], main= "Ref vs. alpha0.4_all")#Adjusted R-squared:  0.8002 
summary(lm(resp_EXP2[,1]~resp_EXP2[,5]))

ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01))

# numbers outside of loess curve are 114, 153,155,156,150,141,105,130,152,98,188 (take out 186)
numberID <- (c(114,153,155,156,150,141,105,130,152,98,188)) - 96
# actuall number of ID are 18 57 59 60 54 45  9 34 56  2 92
#check values
resp_EXP2[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), 20] # check what row numbers are outside loess curve

plot(resp_EXP2[,1],resp_EXP2[,6], main= "Ref vs. alpha0.4_min10.20")#Adjusted R-squared:  0.5743 
summary(lm(resp_EXP2[,1]~resp_EXP2[,6]))

plot(resp_EXP2[,1],resp_EXP2[,7], main= "Ref vs. alpha0.4_min10.25")#Adjusted R-squared:  0.4952 
summary(lm(resp_EXP2[,1]~resp_EXP2[,7]))

plot(resp_EXP2[,1],resp_EXP2[,8], main= "Ref vs. alpha0.6_all")#Adjusted R-squared:  0.4371 
summary(lm(resp_EXP2[,1]~resp_EXP2[,8]))

plot(resp_EXP2[,1],resp_EXP2[,9], main= "Ref vs. alpha0.6_min10.20")#Adjusted R-squared:  0.6022
summary(lm(resp_EXP2[,1]~resp_EXP2[,9]))

plot(resp_EXP2[,1],resp_EXP2[,10], main= "Ref vs. alpha0.6_min10.25")#Adjusted R-squared:  0.5896 
summary(lm(resp_EXP2[,1]~resp_EXP2[,10]))

# make a new data frame of just the REF ouput and alpha =0.2 all 
newdata_resp_EXP2_ALL <- data.frame(resp_EXP2$Date , resp_EXP2[,c(1:10)])
# test for correlative strength in dataset of JUST ID with weak residuals
newdata_resp_EXP2_ALL_1 <-newdata_resp_EXP2_ALL[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), ] 
# omit the ID that had greatest residuals 
#newdata_resp_EXP2_ALL <- newdata_resp_EXP2_ALL[-c(3,4), ] # for ID labeled 17 and 29

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,3])
summary(lm(newdata_resp_EXP2_ALL_1[,3]~newdata_resp_EXP2_ALL_1[,2]))#Adjusted R-squared: 0.4139 

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,4])
summary(lm(newdata_resp_EXP2_ALL_1[,4]~newdata_resp_EXP2_ALL_1[,2]))#Adjusted R-squared:  0.5674  

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,5])
summary(lm(newdata_resp_EXP2_ALL_1[,5]~newdata_resp_EXP2_ALL_1[,2]))#  0.425 

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,6])
summary(lm(newdata_resp_EXP2_ALL_1[,6]~newdata_resp_EXP2_ALL_1[,2]))# -0.02776  

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,7])
summary(lm(newdata_resp_EXP2_ALL_1[,7]~newdata_resp_EXP2_ALL_1[,2]))#0.5682 

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,8])
summary(lm(newdata_resp_EXP2_ALL_1[,8]~newdata_resp_EXP2_ALL_1[,2]))#00.4892 

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,9])
summary(lm(newdata_resp_EXP2_ALL_1[,9]~newdata_resp_EXP2_ALL_1[,2]))#-0.4071 

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,10])
summary(lm(newdata_resp_EXP2_ALL_1[,10]~newdata_resp_EXP2_ALL_1[,2]))#0.5931

plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,11]) # alpha = 0.6 10 - 25 mins
summary(lm(newdata_resp_EXP2_ALL_1[,11]~newdata_resp_EXP2_ALL_1[,2]))# 0.71699  !!!! use this!



#assgin new  "FINAL" column base as the alpha 0.4 all
# resp_EXP2[,5] =0.4 all 
resp_EXP2$FINALresp <- resp_EXP2[,5]
# FINAresp is resp_EXP2[,18]
# insert the ID to replace with 0.6 10-25mins
resp_EXP2[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92),21] <- resp_EXP2[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), 10]


# normality test 
shapiro.test(resp_EXP2$FINALresp) # not normal - ommit the data from row 186
hist(resp_EXP2$FINALresp) # positive or right skewed - need to transform the data

OutVals2 = boxplot(resp_EXP2$FINALresp)$out
which(resp_EXP2$FINALresp %in% OutVals2) #  90 is outliers

resp_EXP2_om <- resp_EXP2[-c(83,90),]
shapiro.test(resp_EXP2_om$FINALresp) # normally distributed
hist(resp_EXP2_om$FINALresp)

# regress the new data column FINALresp with the column of resp by hand ("REF") resp_EXP1[,1]
# lets look at a plot of our raw untransformed FINALresp data TABLE1[,18]
plot(resp_EXP2_om$FINALresp,resp_EXP2_om[,1], main= "Ref vs. FINALresp")
summary(lm(resp_EXP2_om[,1]~resp_EXP2_om$FINALresp)) # Multiple R-squared:  0.9107,	Adjusted R-squared:  0.9097 

ggplot(resp_EXP2_om, aes(x = resp_EXP2_om[,1], y = resp_EXP2_om$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2_om$ID), position = position_nudge(y = -0.01))

# get a table of means and deviations
names(resp_EXP2_om)
sumresp_EXP2 <- summarySE(resp_EXP2_om, 
                       measurevar="FINALresp", 
                       groupvars=c("Date","Treat1_Treat2"))
sumresp_EXP2 # view the table

# get percent difference
sumresp_EXP2_means <- summarySE(sumresp_EXP2, 
                             measurevar="FINALresp", 
                             groupvars=c("Treat1_Treat2"))
sumresp_EXP2_means

##################################################
# mixed effects models
##################################################
names(resp_EXP2_om)
#Exposure 2
Exp2.Fig.resp <- ggboxplot(resp_EXP2_om, x = "Day", y = "FINALresp", color = "Sec.treat", ylab= "respiration",
                      palette = c())
Exp2.Fig.resp

  
x2.resp <- do.call(data.frame,aggregate(FINALresp ~ Day*Init.treat*Sec.treat, data = resp_EXP2_om, function(x) c(mean = mean(x), se = std.error(x))))
x2.1.resp <- do.call(data.frame,aggregate(FINALresp ~ Init.treat*Sec.treat, data = resp_EXP2_om, function(x) c(mean = mean(x), se = std.error(x))))


m2.resp <- lme(FINALresp~Init.treat*Sec.treat,random=~1|Day/Init.treat/Sec.treat,data=resp_EXP2_om)

anova(m2.resp)
summary(m2.resp)


#Post hoc
exp2.ph.resp <- lsmeans(m2.resp, pairwise ~ Init.treat*Sec.treat)
exp2.ph.resp


par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(m2.resp)) #plot histogram of residuals
shapiro.test(residuals(m2.resp)) #  normal residuals
boxplot(residuals(m2.resp)) #plot boxplot of residuals
plot( fitted(m2.resp),residuals(m2.resp)) #display residuals versus fitter, normal QQ plot, leverage plot



#Exposure2 Plotting
#Day 0
Day0 <- subset(x2.resp, Day==0)

Fig.Exp2.D0.resp <- ggplot(Day0, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day0$FINALresp.mean -Day0$FINALresp.se, ymax=Day0$FINALresp.mean +Day0$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 0") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D0.resp

#Day 2
Day2 <- subset(x2.resp, Day==2)

Fig.Exp2.D2.resp <- ggplot(Day2, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day2$FINALresp.mean -Day2$FINALresp.se, ymax=Day2$FINALresp.mean +Day2$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 2") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D2.resp

#Day 4
Day4 <- subset(x2.resp, Day==4)

Fig.Exp2.D4.resp <- ggplot(Day4, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day4$FINALresp.mean -Day4$FINALresp.se, ymax=Day4$FINALresp.mean +Day4$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 4") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D4.resp

#Day 6
Day6 <- subset(x2.resp, Day==6)

Fig.Exp2.D6.resp <- ggplot(Day6, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=Day6$FINALresp.mean -Day6$FINALresp.se, ymax=Day6$FINALresp.mean +Day6$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure Day 6") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.D6.resp

#Averaged across Days

Fig.Exp2.All.resp <- ggplot(x2.1.resp, aes(x=Sec.treat, y=FINALresp.mean , group=Init.treat)) + 
  geom_errorbar(aes(ymin=x2.1.resp$FINALresp.mean -x2.1.resp$FINALresp.se, ymax=x2.1.resp$FINALresp.mean +x2.1.resp$FINALresp.se), colour="black", width=.1, position = position_dodge(width = 0.05)) +
  geom_line(aes(linetype=Init.treat), size = 0.5, position = position_dodge(width = 0.05)) +   
  geom_point(aes(shape=Init.treat), size = 3, position = position_dodge(width = 0.05)) +
  #annotate("text", x=0.85, y=1.3, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(0.85,0.85,2.2,2.2,2.25), y=c(0.9,0.95,0.88, 0.92,0.97), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  xlab("Secondary Treatment") +
  ylab("Respiration rate") +
  ylim(0,0.5) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(), #Set the plot background
        legend.position='none') + #remove legend background
  ggtitle(" Secondary Exposure All Days") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 12, 
                                  hjust = 0))

Fig.Exp2.All.resp

# Saving output plots

Figure1.Exp1.Resp <- arrangeGrob(Exp1.Fig.resp, ncol=1)
ggsave(file="Output/Geoduck_Resp_Exp1.pdf", Exp1.Fig.resp, width = 6.5, height = 8, units = c("in"))


Figure1.Exp2.Resp <- arrangeGrob(Exp2.Fig.resp,Fig.Exp2.D0.resp, Fig.Exp2.D2.resp,Fig.Exp2.D4.resp,Fig.Exp2.D6.resp, ncol=1)
ggsave(file="Output/Geoduck_Resp_Exp2.byDay.pdf", Figure1.Exp2.Resp, width = 6.5, height = 15, units = c("in"))

Figure1.Exp2.AllResp <- arrangeGrob(Fig.Exp2.All.resp, ncol=1)
ggsave(file="Output/Geoduck_Resp_Exp2.All.pdf", Figure1.Exp2.AllResp, width = 6.5, height = 8, units = c("in"))








































##################################
# plots
#################################

par(mfrow=c(2,1))
#plot treatments with time

resp_EXP2_plot <- ggplot(resp_EXP2_om, aes(x = factor(resp_EXP2_om$Date), y = resp_EXP2_om$FINALresp, fill = resp_EXP2_om$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = resp_EXP2_om$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_EXP2_plot + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                         x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments EXP2"))



# plot just treatment 
resp_EXP2_plot_2 <- ggplot(resp_EXP2_om, aes(x = factor(resp_EXP2_om$Treat1_Treat2), y = resp_EXP2_om$FINALresp, fill = resp_EXP2_om$Treat1_Treat2)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = resp_EXP2_om$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_EXP2_plot_2 + labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1", 
                           x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \nin OA treatments EXP2"))


