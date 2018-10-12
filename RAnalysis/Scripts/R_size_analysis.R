###################################################################################################
rm(list=ls())
library(dplyr)
library(tidyverse)
library(plyr)
library(ggplot2)
library(ggpubr)
library(devtools)
library(easyGgplot2)
library(reshape2)
library(lme4)
library(seacarb) 
library(reshape) 
library(plotrix) 
library(ggplot2) 
library(plyr)
library(gridExtra)
library(multcompView)
library(lsmeans)
#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/Notebook/data/Geoduck_Conditioning/RAnalysis/Data/Growth_data/")
main<-getwd()
# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Juvenile_SDR_resp_trials" #the locationv of all your respiration files

########################
# make the dataframe
########################
size<-read.csv(file.path(path, ("All_growth_data.csv")), header=T, sep=",", na.string="NA", as.is=T) 

size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals

# DIVIDE THE DATASET BETWEEN EXPERIMENTS 1 AND 2
size_EXP1 <- size[1:955,]
size_EXP2 <- size[956:1917,]

# make new columns to view size across tanks and treatment through time
# by tank as "Date_tank"
size$Date_tank <- paste(size$Date, size$tank, size$timeline_days, sep="_")
size_EXP1$Date_tank <- paste(size_EXP1$Date, size_EXP1$tank, size_EXP1$timeline_days, sep="_")
size_EXP2$Date_tank <- paste(size_EXP2$Date, size_EXP2$tank, size_EXP2$timeline_days, sep="_")

# by treatment as "Date_treat"
size$Date_treat <- paste(size$Date, size$treatment, sep="_")
size_EXP1$Date_treat <- paste(size_EXP1$Date, size_EXP1$treatment, size_EXP1$timeline_days, sep="_")
size_EXP2$Date_treat <- paste(size_EXP2$Date, size_EXP2$treatment, size_EXP2$timeline_days, sep="_")
##############################
# PLOT THE RAW DATA
##############################
#################  Exposure 1 Plotting
#points
Fig.Exp1.size <- ggplot(size_EXP1, aes(x=Day, y=shell_size, group=treatment)) +
  geom_point(aes(shape=treatment), size = 3, position = position_dodge(width = 0.6))

Fig.Exp1.size
#boxplots
Fig.Exp1.size_2 <- ggplot(size_EXP1, aes(x = factor(Day), y = shell_size, fill = tank)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = tank), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(Fig.Exp1.size_2 + labs(y="Shell size (mm)", 
                         x = "Day") + 
        ggtitle("Juvenile geoduck shell size \nin OA treatments EXP1"))

################# Exposure 2 Plotting
#points
Fig.Exp2.size <- ggplot(size_EXP2, aes(x=Day, y=shell_size, group=treatment)) +
  geom_point(aes(shape=treatment), size = 3, position = position_dodge(width = 0.6))

Fig.Exp2.size

#boxplots
Fig.Exp2.size_2 <- ggplot(size_EXP2, aes(x = factor(Day), y = shell_size, fill = tank)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = tank), size = 2, shape = 21, position = position_jitterdodge(0.05)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(Fig.Exp2.size_2 + labs(y="Shell size (mm)", 
                             x = "Day") + 
        ggtitle("Juvenile geoduck shell size \nin OA treatments EXP2"))

##############################
# run linear mixed effects model with treatment fixed, date random, and tank random and nested in treatment
##############################
#size_EXP1$Day <- as.character(size_EXP1$Day) # Day needs to be non-numerical
#size_EXP2$Day <- as.character(size_EXP2$Day) # Day needs to be non-numerical
#size_EXP1$ID <- as.character(size_EXP1$ID)

############################## Exposure 1 model############################## 
shapiro.test(size_EXP1$shell_size) # check for normality in the raw data
hist(size_EXP1$shell_size) # negative skew, reflect and square root to normalize
max(size_EXP1$shell_size) # max value is 6.859, subtract all values by 1+maxshellsize and square root
size_EXP1$shell_sizeTRANS <- sqrt((1+max(size_EXP1$shell_size)) - size_EXP1$shell_size) # reflect and square root
shapiro.test(size_EXP1$shell_sizeTRANS) # transformation is normally distributed
hist(size_EXP1$shell_sizeTRANS) # histogram shows the norm distribution

# run mixed effect model on transformed data; treat = fixed, Tank nested random within treatment and day

#Init.lme <- lmer(shell_sizeTRANS ~ treatment + (1|Day/treatment/tank/tank_ID), data = size_EXP1) # group factor > individual variance (num. observ)

Init.lme <- lme(shell_sizeTRANS ~ treatment, random=~1|Day/treatment/tank, data = size_EXP1) 
#Init.lme <- lmer(shell_sizeTRANS ~ treatment + (1|Day/treatment/tank), data = size_EXP1) 

hist(residuals(Init.lme)) # histogram
shapiro.test(residuals(Init.lme)) # normality test
summary(Init.lme) # summary
anova(Init.lme) # anova

############################## Exposure 2 model############################## 

Second.lme <- lme(shell_size ~ treatment, random=~1|Day/treatment/tank, data = size_EXP2) 

shapiro.test(residuals(Second.lme)) # check for normality of residuals
hist(residuals(Second.lme)) # strong negative skew


shapiro.test(size_EXP2$shell_size) # check for normality in the raw data
hist(size_EXP2$shell_size) # negative skew, reflect and square root to normalize
max(size_EXP2$shell_size) # max value is 6.859, subtract all values by 1+maxshellsize and square root
size_EXP2$shell_sizeTRANS <- sqrt((1+max(size_EXP2$shell_size)) - size_EXP2$shell_size) # reflect and square root
shapiro.test(size_EXP2$shell_sizeTRANS) # transformation is normally distributed
hist(size_EXP2$shell_sizeTRANS) # histogram shows the norm distribution


Second.lmeTRANS <- lme(shell_sizeTRANS ~ treatment, random=~1|Day/treatment/tank, data = size_EXP2) # use transformed data in the model

#Second.lmeTRANS <- lmer(shell_sizeTRANS ~ treatment + (1|Day/treatment/tank), data = size_EXP2)


shapiro.test(residuals(Second.lmeTRANS)) # check residuals
hist(residuals(Second.lmeTRANS)) # histogram
summary(Second.lmeTRANS) # summary
anova(Second.lmeTRANS) # anova

#EXP2.post.hoc <- aov(shell_sizeTRANS ~ treatment, data = size_EXP2) #post.hoc test treatment
#anova(EXP2.post.hoc)
#TukeyHSD(EXP2.post.hoc)


























#-------------------------------------------------------- EXPERIMENT 1 normality tests based on treatment 
#day 2 (first measurements of specific trays for EXP 1 were on day 2 of exposure)
list(size_EXP1$Date_treat)

low_exp1.d2 <-subset(size_EXP1, Date_treat=="20180716_Low_2")
shapiro.test(low_exp1.d2$shell_size) # EXP1 
hist(low_exp1.d2$shell_size) # EXP1

amb_exp1.d2 <-subset(size_EXP1, Date_treat=="20180716_Ambient_2")
shapiro.test(amb_exp1.d2$shell_size) # EXP1 
hist(amb_exp1.d2$shell_size) # EXP1

#------------------------------------------------------ EXPERIMENT 2 normality tests based on treatment 
#day 0 (first measurements of specific trays for EXP 1 were on day 2 of exposure)
list(size_EXP2$Date_treat)

low_exp2.d0 <-subset(size_EXP2, Date_treat==c("20180807_Ambient_Low_24", "20180807_Low_Low_24"))
shapiro.test(low_exp2.d0$shell_size) # EXP1 
hist(low_exp2.d0$shell_size)


amb_exp2.d0 <- subset(size_EXP2, Date_treat==c("20180807_Low_Ambient_24", "20180807_Ambient_Ambient_24"))
shapiro.test(amb_exp2.d0$shell_size) # EXP1 
hist(amb_exp2.d0$shell_size)



lowlow.d <-subset(size_EXP2, Date_treat=="20180807_Low_Low_24")
shapiro.test(lowlow.d$shell_size) # EXP1 
hist(lowlow.d$shell_size)


ambamb.d0 <-subset(size_EXP2, Date_treat=="20180807_Ambient_Ambient_24")
shapiro.test(ambamb.d0$shell_size) # EXP1 
hist(ambamb.d0$shell_size)


lowamb.d0 <-subset(size_EXP2, Date_treat=="20180807_Low_Ambient_24")
shapiro.test(lowamb.d0$shell_size) # EXP1 
hist(lowamb.d0$shell_size)


amblow.d0 <-subset(size_EXP2, Date_treat=="20180807_Ambient_Low_24")
shapiro.test(amblow.d0$shell_size) # EXP1 
hist(amblow.d0$shell_size)




########################
# summarize tables from dplyr as ddply(dataframe, "group name", summarise, mean = mean (Dep variable))
########################
# by treat
sizeMEANS_treat <- cdata <- ddply(size, c("Date_treat"), summarise,
                                 N    = length(shell_size),
                                 mean = mean(shell_size),
                                 sd   = sd(shell_size),
                                 se   = sd / sqrt(N))
sizeMEANS_treat # look at data
sizeMEANS_treat$day <- c(2,2,5,5,8,8,10,10,0,0,0,0,2,2,2,2,4,4,4,4,6,6,6,6)
sizeMEANS_treat$treat <- (substr(sizeMEANS_treat$Date_treat, 10, 20))


# take the three columns we need - mean, treat, day
EXP1.mean <- sizeMEANS_treat[1:8,(c(3,6,7))]
EXP2.mean <- sizeMEANS_treat[9:24,(c(3,6,7))]
# reshape the rows into columns
EXP1.mean.cols <- reshape(EXP1.mean, idvar="treat", timevar="day", direction="wide")
EXP2.mean.cols <- reshape(EXP2.mean, idvar="treat", timevar="day", direction="wide")


# EXP1 GROWTH DAY 10 - DAY 0
EXP1.mean.cols$growth <- EXP1.mean.cols[1:2,5] - EXP1.mean.cols[1:2,2]

growthperday.exp1 <- (EXP1.mean.cols$growth/8)*1000 #day 10 to day 2 = 8 days of growth (units =  um day-1)

# EXP2 GROWTH 
EXP2.mean.cols$growth.d0 <- c(1)
EXP2.mean.cols$growth.d2 <- EXP2.mean.cols[1:4,3] / EXP2.mean.cols[1:4,2] # EXP2 GROWTH DAY 2 - DAY 0
EXP2.mean.cols$growth.d4 <- EXP2.mean.cols[1:4,4] / EXP2.mean.cols[1:4,2] # EXP2 GROWTH DAY 4 - DAY 0
EXP2.mean.cols$growth.d6 <- EXP2.mean.cols[1:4,5] / EXP2.mean.cols[1:4,2] # EXP2 GROWTH DAY 6 - DAY 0
EXP2.mean.cols

growthperday.exp2 <- (EXP2.mean.cols$ growth.d6/6)*1000 #day 6 to day 0 = 6 days of growth (units =  um day-1)

# make columns back to rows for plotting
EXP2.mean.FINAL <- melt(EXP2.mean.cols, id=c("treat")) #reshape according to treatment
EXP2.mean.FINAL <- EXP2.mean.FINAL[17:32,] # only include normalized growth in new dataframe
EXP2.mean.FINAL$day <-  (substr(EXP2.mean.FINAL$variable, 9,9)) # name new column for "day"
# plot the data
ggplot(EXP2.mean.FINAL, aes(x=day, y=value, 
    shape=treat, color=treat , group=treat)) + 
    geom_point()+geom_line()





# means of data from first 10-day (trial 1) and second 6-day (EXP2)

# ALL data at day 10 normalized to day 2 growth
exp1_all_d2 <-subset(size_EXP1, timeline_days==c("2")) # get the mean data for each tank at day to normalize
exp1.mean.all.d2 <- cdata <- ddply(exp1_all_d2, c("tank"), summarise,
                                  N    = length(shell_size),
                                  mean = mean(shell_size),
                                  sd   = sd(shell_size),
                                  se   = sd / sqrt(N))
exp1.mean.all.d2# made a table for the mean growth data for each LOW tank

exp1_all_d10 <-subset(size_EXP1, timeline_days==c("10")) # subset data of just day 10

exp1.growth.d10 <- exp1_all_d10[c(6,8,9)] # take only the treatment tank and size
exp1.H0_T<- subset(exp1.growth.d10, tank=="H0_T")
exp1.H0_T$growth <- exp1.H0_T$shell_size / exp1.mean.all.d2[2,3] # substract by coinciding tank at day 2

exp1.H0_B<- subset(exp1.growth.d10, tank=="H0_B")
exp1.H0_B$growth <- exp1.H0_B$shell_size / exp1.mean.all.d2[1,3]# substract by coinciding tank at day 2

exp1.H1_T<- subset(exp1.growth.d10, tank=="H1_T")
exp1.H1_T$growth <- exp1.H1_T$shell_size / exp1.mean.all.d2[4,3]# substract by coinciding tank at day 2

exp1.H1_B<- subset(exp1.growth.d10, tank=="H1_B")
exp1.H1_B$growth <- exp1.H1_B$shell_size / exp1.mean.all.d2[3,3]# substract by coinciding tank at day 2

exp1.H2_T<- subset(exp1.growth.d10, tank=="H2_T")
exp1.H2_T$growth <- exp1.H2_T$shell_size / exp1.mean.all.d2[6,3]# substract by coinciding tank at day 2

exp1.H2_B<- subset(exp1.growth.d10, tank=="H2_B")
exp1.H2_B$growth <- exp1.H2_B$shell_size / exp1.mean.all.d2[5,3]# substract by coinciding tank at day 2

exp1.H3_T<- subset(exp1.growth.d10, tank=="H3_T")
exp1.H3_T$growth <- exp1.H3_T$shell_size / exp1.mean.all.d2[8,3]# substract by coinciding tank at day 2

exp1.H3_B<- subset(exp1.growth.d10, tank=="H3_B")
exp1.H3_B$growth <- exp1.H3_B$shell_size / exp1.mean.all.d2[7,3]# substract by coinciding tank at day 2



# bind all rows for each of the tanks
exp1.growth.FINAL<-rbind.fill(exp1.H0_T,exp1.H0_B,exp1.H1_T,exp1.H1_B,exp1.H2_T,exp1.H2_B,exp1.H3_T,exp1.H3_B)
names(exp1.growth.FINAL)


# plot based on treatment and tank

par(mfrow=c(1,2))
PLOT.exp1.growth.FINAL <- ggplot(exp1.growth.FINAL, aes(x = factor(treatment), y = growth, fill = tank)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = tank), size = 2, shape = 21, position = position_jitterdodge(0.05)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(PLOT.exp1.growth.FINAL + labs(y="growth day 10 - day 2 EXP1", 
                           x = "treatment") + 
        ggtitle("Juvenile geoduck growth standardized to day 2 EXP1"))



PLOT.exp1.growth.FINAL2 <- ggplot(exp1.growth.FINAL, aes(x = factor(treatment), y = growth, fill = treatment)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = treatment), size = 2, shape = 21, position = position_jitterdodge(0.05)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(PLOT.exp1.growth.FINAL2 + labs(y="growth day 10 - day 2 EXP1", 
                                    x = "treatment") + 
        ggtitle("Juvenile geoduck growth standardized to day 2 EXP1"))










#####################################################################################################3

######################################################################################################



# means of data from second 6-day (EXP2)

# ALL data at day 6 normalized to day 0 growth
exp2_all_d0 <-subset(size_EXP2, timeline_days==c("24")) # get the mean data for each tank at day to normalize
exp2.mean.all.d0 <- cdata <- ddply(exp2_all_d0, c("tank"), summarise,
                                   N    = length(shell_size),
                                   mean = mean(shell_size),
                                   sd   = sd(shell_size),
                                   se   = sd / sqrt(N))
exp2.mean.all.d0# made a table for the mean growth data for each LOW tank

exp2_all_d6 <-subset(size_EXP2, timeline_days==c("30")) # subset data of just day 10

exp2.growth.d6 <- exp2_all_d6[c(6,8,9)] # take only the treatment tank and size
exp2.H0_T<- subset(exp2.growth.d6, tank=="H0_T")
exp2.H0_T$growth <- exp2.H0_T$shell_size / exp2.mean.all.d0[2,3] # substract by coinciding tank at day 0

exp2.H0_B<- subset(exp2.growth.d6, tank=="H0_B")
exp2.H0_B$growth <- exp2.H0_B$shell_size / exp2.mean.all.d0[1,3]# substract by coinciding tank at day 0

exp2.H1_T<- subset(exp2.growth.d6, tank=="H1_T")
exp2.H1_T$growth <- exp2.H1_T$shell_size / exp2.mean.all.d0[4,3]# substract by coinciding tank at day 0

exp2.H1_B<- subset(exp2.growth.d6, tank=="H1_B")
exp2.H1_B$growth <- exp2.H1_B$shell_size / exp2.mean.all.d0[3,3]# substract by coinciding tank at day 0

exp2.H2_T<- subset(exp2.growth.d6, tank=="H2_T")
exp2.H2_T$growth <- exp2.H2_T$shell_size / exp2.mean.all.d0[6,3]# substract by coinciding tank at day 0

exp2.H2_B<- subset(exp2.growth.d6, tank=="H2_B")
exp2.H2_B$growth <- exp2.H2_B$shell_size / exp2.mean.all.d0[5,3]# substract by coinciding tank at day 0

exp2.H3_T<- subset(exp2.growth.d6, tank=="H3_T")
exp2.H3_T$growth <- exp2.H3_T$shell_size / exp2.mean.all.d0[8,3]# substract by coinciding tank at day 0

exp2.H3_B<- subset(exp2.growth.d6, tank=="H3_B")
exp2.H3_B$growth <- exp2.H3_B$shell_size / exp2.mean.all.d0[7,3]# substract by coinciding tank at day 0



# bind all rows for each of the tanks
exp2.growth.FINAL<-rbind.fill(exp2.H0_T,exp2.H0_B,exp2.H1_T,exp2.H1_B,exp2.H2_T,exp2.H2_B,exp2.H3_T,exp2.H3_B)
names(exp2.growth.FINAL)


# plot based on treatment and tank

par(mfrow=c(1,2))
PLOT.exp2.growth.FINAL <- ggplot(exp2.growth.FINAL, aes(x = factor(treatment), y = growth, fill = tank)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = tank), size = 2, shape = 21, position = position_jitterdodge(0.05)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(PLOT.exp2.growth.FINAL + labs(y="growth day 6 - day 0 exp2", 
                                    x = "treatment") + 
        ggtitle("Juvenile geoduck growth standardized to day 0 exp2"))



PLOT.exp2.growth.FINAL2 <- ggplot(exp2.growth.FINAL, aes(x = factor(treatment), y = growth, fill = treatment)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(aes(fill = treatment), size = 2, shape = 21, position = position_jitterdodge(0.05)) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(PLOT.exp2.growth.FINAL2 + labs(y="growth day 6 - day 0 exp2", 
                                     x = "treatment") + 
        ggtitle("Juvenile geoduck growth standardized to day 0 exp2"))

############################## Exposure 2 model############################## 
MERGED_EXP2 <- merge.data.frame(exp2.growth.FINAL, size_EXP2, all.x=TRUE) # merge orgiinal exp2 with standardized growth data
Second.lme.merged <- lme(growth ~ treatment, random=~1|Day/treatment/tank, data = MERGED_EXP2) 

shapiro.test(residuals(Second.lme.merged)) # check for normality of residuals
hist(residuals(Second.lme.merged)) # norm

summary(Second.lme.merged)
anova(Second.lme.merged)







