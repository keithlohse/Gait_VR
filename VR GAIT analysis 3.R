library("ggplot2"); library("lme4"); library("car"); library("dplyr"); library("lmerTest"); 
library("tidyverse"); library("RColorBrewer");library("plyr")

###set directory
setwd("C:/Users/u6021023/Box/Gait_VR DATA/Analysis")

list.files()


###Load in data frames for gait and create tibble

DATA_Gait <- read.csv("VR GAIT2.csv", header = TRUE, sep=",",  
              na.strings=c("NA","NaN"," ",""))
head(DATA_Gait)

DATA_Gait$Subject<-factor(DATA_Gait$Subject)
head(DATA_Gait, 10)

DATA_Gait$Height.c<-(as.numeric(DATA_Gait$Height)-1.5)*(-2)
DATA_Gait$Fast.c<-(as.numeric(DATA_Gait$Speed)-1.5)*(-2)
DATA_Gait$Trial.c<-DATA_Gait$Trial-1

#####


# Lumbar Velocity
mod1 <- lmer(Lumbar.Vel~
               #Fixed Effects:
               Trial.c*Height.c*Fast.c+
               #Random Effects:
               (1|Subject)+(1|Height:Subject)+(1|Speed:Subject),
             REML=FALSE, data=DATA_Gait)
summary(mod1)




##post hoc decomposition
mod1_High <- lmer(Lumbar.Vel~
                    #Fixed Effects:
                    Trial.c*Fast.c+
                    #Random Effects:
                    (1|Subject),
                  REML=FALSE, data=DATA_Gait[DATA_Gait$Height=="H",])

summary(mod1_High)

###breaking down Speed by Trial interaction at Height

mod1_HighT <- lmer(Lumbar.Vel~
                    #Fixed Effects:
                    Trial.c +
                    #Random Effects:
                    (1|Subject),
                  REML=FALSE, data=DATA_Gait[DATA_Gait$Height=="H"&DATA_Gait$Speed=="SS",])

summary(mod1_HighT)


mod1_HighT2 <- lmer(Lumbar.Vel~
                     #Fixed Effects:
                     Trial.c +
                     #Random Effects:
                     (1|Subject),
                   REML=FALSE, data=DATA_Gait[DATA_Gait$Height=="H"&DATA_Gait$Speed=="FT",])

summary(mod1_HighT2)

###############

mod1_Low <- lmer(Lumbar.Vel~
                   #Fixed Effects:
                   Trial.c * Fast.c+
                   #Random Effects:
                   (1|Subject),
                 REML=FALSE, data=DATA_Gait[DATA_Gait$Height=="L",])

summary(mod1_Low)

mod0_Fast <- lmer(Lumbar.Vel~
                    #Fixed Effects:
                    Trial.c * Height.c+
                    #Random Effects:
                    (1|Subject),
                  REML=FALSE, data=DATA_Gait[DATA_Gait$Speed=="FT",])

summary(mod0_Fast)


mod0_Slow <- lmer(Lumbar.Vel~
                    #Fixed Effects:
                    Trial.c * Height.c+
                    #Random Effects:
                    (1|Subject),
                  REML=FALSE, data=DATA_Gait[DATA_Gait$Speed=="SS",])

summary(mod0_Slow)


###-------------Likert rating analysis------############

###Load in data frames for ratings and create tibble
MRFRatings<-read.csv("Copy of Final Ratings Long Format2.csv", header=TRUE, sep=",",  
                     na.strings=c("NA","NaN"," ",""))
as_tibble(MRFRatings)
head(MRFRatings)

##transforming data as per first model
MRFRatings$Height.c<-(as.numeric(MRFRatings$Elevation)-1.5)*(-2)
MRFRatings$Fast.c<-(as.numeric(MRFRatings$Speed)-1.5)*(-2)
MRFRatings$SUBJ<-factor(MRFRatings$SUBJ)
MRFRatings$Confidence2 <- 12-MRFRatings$Confidence




##cog.anxiety 
mod3 <- lmer(Cog.Anxiety~
               #Fixed Effects:
               Height.c*Fast.c+
               #Random Effects:
               (1|SUBJ)+(1|Elevation:SUBJ)+(1|Speed:SUBJ),
             REML=FALSE, data=MRFRatings)
summary(mod3)


###somatic anxiety

mod4 <- lmer(Som.Anxiety~
               #Fixed Effects:
               Height.c*Fast.c+
               #Random Effects:
               (1|SUBJ)+(1|Elevation:SUBJ)+(1|Speed:SUBJ),
             REML=FALSE, data=MRFRatings)
summary(mod4)



###Confidence
mod5 <- lmer(Confidence2~
               #Fixed Effects:
               Height.c*Fast.c+
               #Random Effects:
               (1|SUBJ)+(1|Elevation:SUBJ)+(1|Speed:SUBJ),
             REML=FALSE, data=MRFRatings)
summary(mod5)


###Effort
mod6 <- lmer(Effort~
               #Fixed Effects:
               Height.c*Fast.c+
               #Random Effects:
               (1|SUBJ)+(1|Elevation:SUBJ)+(1|Speed:SUBJ),
             REML=FALSE, data=MRFRatings)
summary(mod6)

####Correlation Analysis




##read in change score data sheet
Data_Corr <- read.csv("Change Scores.csv", header = TRUE, sep=",",  
                      na.strings=c("NA","NaN"," ",""))
head(Data_Corr)

##Subset correlation data by SS speed
Data_CorrSS <- subset(Data_Corr, Data_Corr$Speed =='SS', Select = c(Subject:lumb_change))

view(Data_CorrSS)
summary(Data_CorrSS$Cog_change)
summary(Data_CorrSS$Som_change)
summary(Data_CorrSS$Conf_change)
summary(Data_CorrSS$Eff_change)
summary(Data_CorrSS$lumb_change)

##Subset correlation data by FT speed

Data_CorrFT <- subset(Data_Corr, Data_Corr$Speed =='FT', Select = c(Subject:lumb_change))

view(Data_CorrFT)

summary(Data_CorrFT$Cog_change)
summary(Data_CorrFT$Som_change)
summary(Data_CorrFT$Conf_change)
summary(Data_CorrFT$Eff_change)
summary(Data_CorrFT$lumb_change)


# Cog anxiety and lumbar at SS
corr1 <- cor.test(x=Data_CorrSS$Cog_change, y=Data_CorrSS$lumb_change, method = 'spearman')
(corr1)

# Cog anxiety and lumbar at FT
corr2 <- cor.test(x=Data_CorrFT$Cog_change, y=Data_CorrFT$lumb_change, method = 'spearman')
(corr2)

# Somatic anxiety and lumbar at SS
corr3 <- cor.test(x=Data_CorrSS$Som_change, y=Data_CorrSS$lumb_change, method = 'spearman')
(corr3)


# Somatic anxiety and lumbar at FT
corr4 <- cor.test(x=Data_CorrFT$Som_change, y=Data_CorrFT$lumb_change, method = 'spearman')
(corr4)

#Confidence and lumbar at SS

corr5 <- cor.test(x=Data_CorrSS$Conf_change, y=Data_CorrSS$lumb_change, method = 'spearman')
(corr5)

#Confidence and lumbar at FT
corr6 <- cor.test(x=Data_CorrFT$Conf_change, y=Data_CorrFT$lumb_change, method = 'spearman')
(corr6)

#Effort and lumbar at SS

corr7 <- cor.test(x=Data_CorrSS$Eff_change, y=Data_CorrSS$lumb_change, method = 'spearman')
(corr7)

#Effort and lumbar at FT
corr8 <- cor.test(x=Data_CorrFT$Eff_change, y=Data_CorrFT$lumb_change, method = 'spearman')
(corr8)

#Cognitive Anxiety and Effort at SS
corr9 <- cor.test(x=Data_CorrSS$Cog_change, y=Data_CorrSS$Eff_change, method = 'spearman')
(corr9)

#Cognitive Anxiety and Effort at FT
corr10 <- cor.test(x=Data_CorrFT$Cog_change, y=Data_CorrFT$Eff_change, method = 'spearman')
(corr10)


#Somatic Anxiety and Effort at SS
corr11 <- cor.test(x=Data_CorrSS$Som_change, y=Data_CorrSS$Eff_change, method = 'spearman')
(corr11)

#Somatic Anxiety and Effort at FT
corr12 <- cor.test(x=Data_CorrFT$Som_change, y=Data_CorrFT$Eff_change, method = 'spearman')
(corr12)

#Confidence and Effort at SS
corr13 <- cor.test(x=Data_CorrSS$Conf_change, y=Data_CorrSS$Eff_change, method = 'spearman')
(corr13)

#Confidence and Effort at FT
corr14 <- cor.test(x=Data_CorrFT$Conf_change, y=Data_CorrFT$Eff_change, method = 'spearman')
(corr14)