###---written by T Raffegeau, last updated 4/29/20
# Loading the essential libraries. 
library("ggplot2"); library("lme4"); library("car"); library("dplyr"); library("lmerTest"); 
library("tidyverse"); library("RColorBrewer"); 

###---package needed to install and use Times font in final figures 
###--causes a lot of errors, do not use until creating final images
install.packages("extrafont")
library(extrafont)
font_import()
y
loadfonts(device="win") 


###set directory
setwd("YOUR DIRECTORY HERE")
list.files()

####-----READ DATA----####

DATA<-read.csv("Standing HR YA 3.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

head(DATA)
str(DATA)


####---TIDY DATA----####

DATA$Trial<-as.factor(DATA$Trial)

DATA$Height <-substr(DATA$Trial, start=1, stop=4)
DATA$Height<-gsub("Low ", "Low", DATA$Height)
DATA$Height <- as.factor(DATA$Height)
DATA$Height<-relevel(DATA$Height, "Low")
summary(DATA$Height)

DATA$Trial<-gsub("Low AP", "Parallel", DATA$Trial)
DATA$Trial<-gsub("High AP", "Parallel", DATA$Trial)
DATA$Trial<-gsub("Low ML", "Perpendicular", DATA$Trial)
DATA$Trial<-gsub("High ML", "Perpendicular", DATA$Trial)
DATA$Trial <- factor(DATA$Trial, levels = c("Parallel", "Perpendicular"))

str(DATA)




###-----POSTURE DATA ANALYSES AND VISUALIZATION----####

####---Ellipse Sway Area Acceleration Based ---####

###----calculations for Cohens D worksheet-----####
DATA_MEAN <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(Ellipse.Sway.Area.1 = mean(Ellipse.Sway.Area), Ellipse.Sway.SD= sd(Ellipse.Sway.Area))
view(DATA_MEAN)
DATA_MEAN.T <-DATA %>%
  group_by (Trial) %>%
  summarize(Ellipse.Sway.Area.T = mean(Ellipse.Sway.Area), Ellipse.Sway.SD.T = sd(Ellipse.Sway.Area))
view(DATA_MEAN.T)
DATA_MEAN.H <-DATA %>%
  group_by (Height) %>%
  summarize(Ellipse.Sway.Area.H = mean(Ellipse.Sway.Area), Ellipse.Sway.SD.H = sd(Ellipse.Sway.Area))
view(DATA_MEAN.H)
###------Ellipse Graph-----####

DATA_MEAN$Height <-as.factor(DATA_MEAN$Height)

str(DATA)

ggplot(data = DATA, mapping = aes (x = Height, y = Ellipse.Sway.Area)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="95% Ellipse Sway Area (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none") +
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN, mapping = aes(x = Height, y = Ellipse.Sway.Area.1), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, stroke=1.25)+
  geom_line(data = DATA_MEAN, mapping = aes(x = Height, y = Ellipse.Sway.Area.1, group=1), col = "black", lty=2, lwd=2)


###------Ellipse Stats-----####


mod1 <- lmer(Ellipse.Sway.Area~
               #Fixed Effects:
               Height*Trial+
               #Random Effects:
               (1|Subject)+(1|Height:Subject) + (1|Trial:Subject),
             REML = FALSE, data=DATA)
summary(mod1)

##----decomposing Trial x Height interaction ---## 
##---look at the effect of height within each trial to match figures---##

mod1a <- lmer(Ellipse.Sway.Area~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Parallel",])
summary(mod1a)

mod1b <- lmer(Ellipse.Sway.Area~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Perpendicular",])
summary(mod1b)


#assumptions tests#
plot(fitted(mod1),resid(mod1)) #plots to show linearity - want no clear trend in residuals#
plot(fitted(mod1),resid(mod1)/sd(resid(mod1)))
plot(fitted(mod1),(abs(resid(mod1)/sd(resid(mod1)))))

summary(lm((abs(resid(mod1)/sd(resid(mod1))))~fitted(mod1)))


plot(density(resid(mod1))) 
qqnorm(resid(mod1)/sd(resid(mod1)))#QQ plots to show normality - want fit close as possible to line#
abline(a=0, b=1)
res <- length(resid(mod1))
ks.test(x=resid(mod1)/sd(resid(mod1)),
        y=rnorm(res, 0, 1))

Anova(mod1, type = "3") #effect sizes (i.e., eta squared)#
boxplot(residuals(mod1) ~ DATA$Height)
leveneTest(residuals(mod1) ~ DATA$Height) #tests heteroscedasticity - want > .05#

boxplot(residuals(mod1) ~ DATA$Trial)
leveneTest(residuals(mod1) ~ DATA$Trial) #tests heteroscedasticity - want > .05#

###-------Ellipse Rotation Acceleration Based-----###

###----Rotation Calculations for Cohens D worksheet-----####
DATA_MEAN.2 <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(Ellipse.Acc.Rotation.R = mean(Ellipse.Acc.Rotation.Rad), Ellipse.Acc.Rotation.SD = sd(Ellipse.Acc.Rotation.Rad))
view(DATA_MEAN.2)
DATA_MEAN.2T <-DATA %>%
  group_by (Trial) %>%
  summarize(Ellipse.Acc.Rotation.R.T = mean(Ellipse.Acc.Rotation.Rad), Ellipse.Acc.Rotation.SD.T = sd(Ellipse.Acc.Rotation.Rad))
view(DATA_MEAN.2T)
DATA_MEAN.2H <-DATA %>%
  group_by (Height) %>%
  summarize(Ellipse.Acc.Rotation.R.H = mean(Ellipse.Acc.Rotation.Rad), Ellipse.Acc.Rotation.SD.H = sd(Ellipse.Acc.Rotation.Rad))
view(DATA_MEAN.2H)

###-------------Rotation Visualization ----###
##--not in paper so not in final format
DATA.Rotation <-DATA %>%
  group_by (Height, Trial) %>%
  summarize(Ellipse.Acc.Rotation = mean(Ellipse.Acc.Rotation.Rad))

ggplot(data = DATA, mapping = aes (x = Height, y = Ellipse.Acc.Rotation.Rad)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="Ellipse Axis 1 Rotation (rad)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 14),
        legend.position="none")+ 
  geom_point(data = DATA.Rotation, mapping = aes(x = Height, y = Ellipse.Acc.Rotation), 
             col = "black", fill = "black", alpha=0.7, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA.Rotation, mapping = aes(x = Height, y = Ellipse.Acc.Rotation, group=1), col = "black", lty=2, lwd=2)

####---Ellipse Sway Area Axis 1 -Acceleration Based ---####

##---Ellipse Axis 1 Cohens D Calculations----###
DATA_MEAN.3 <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(Ellipse.Acc.Axis = mean(Ellipse.Acc.Axis.1), Ellipse.Acc.Axis.SD = sd(Ellipse.Acc.Axis.1))
view(DATA_MEAN.3)
DATA_MEAN.3T <-DATA %>%
  group_by (Trial) %>%
  summarize(Ellipse.Acc.Axis.T = mean(Ellipse.Acc.Axis.1), Ellipse.Acc.Axis.SD.T = sd(Ellipse.Acc.Axis.1))
view(DATA_MEAN.3T)
DATA_MEAN.3H <-DATA %>%
  group_by (Height) %>%
  summarize(Ellipse.Acc.Axis.H = mean(Ellipse.Acc.Axis.1), Ellipse.Acc.Axis.SD.H = sd(Ellipse.Acc.Axis.1))
view(DATA_MEAN.3H)


###----Ellipse Axis 1 graph-----###


DATA_MEAN.3$Height <-as.factor(DATA_MEAN.3$Height)

str(DATA)

ggplot(data = DATA, mapping = aes (x = Height, y = Ellipse.Acc.Axis.1)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="95% Ellipse Minor Axis (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+  
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN.3, mapping = aes(x = Height, y = Ellipse.Acc.Axis), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN.3, mapping = aes(x = Height, y = Ellipse.Acc.Axis, group=1), col = "black", lty=2, lwd=2)


###----Ellipse Axis 1 stats-----###

mod2 <- lmer(Ellipse.Acc.Axis.1~
                #Fixed Effects:
                Height*Trial+
                #Random Effects:
                (1|Subject)+(1|Height:Subject) + (1|Trial:Subject),
              REML = FALSE, data=DATA)
summary(mod2)

##----decomposing Trial x Height interaction ---## 
##---look at the effect of height within each trial to match figures---##

mod2a <- lmer(Ellipse.Acc.Axis.1~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Parallel",])
summary(mod2a)

mod2b <- lmer(Ellipse.Acc.Axis.1~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Perpendicular",])
summary(mod2b)

#assumptions tests#
plot(fitted(mod2),resid(mod2)) #plots to show linearity - want no clear trend in residuals#
plot(fitted(mod2),resid(mod2)/sd(resid(mod2)))
plot(fitted(mod2),(abs(resid(mod2)/sd(resid(mod2)))))

summary(lm((abs(resid(mod2)/sd(resid(mod2))))~fitted(mod2)))


plot(density(resid(mod2)))
qqnorm(resid(mod2)/sd(resid(mod2))) #QQ plots to show normality - want fit close as possible to line#
abline(a=0, b=1)
res <- length(resid(mod2))
ks.test(x=resid(mod2)/sd(resid(mod2)),
        y=rnorm(res, 0, 1))

Anova(mod2, type = "3") #effect sizes (i.e., eta squared)#
boxplot(residuals(mod2) ~ DATA$Height)
leveneTest(residuals(mod2) ~ DATA$Height) #tests heteroscedasticity - want > .05#

boxplot(residuals(mod2) ~ DATA$Trial)
leveneTest(residuals(mod2) ~ DATA$Trial) #tests heteroscedasticity - want > .05#

####---Ellipse Sway Area Axis 2 -Acceleration Based---####

###----Ellipse Axis 2 calculations for Cohens D worksheet-----####
DATA_MEAN.4 <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(Ellipse.Acc.Major = mean(Ellipse.Acc.Axis.2), Ellipse.Acc.Major.SD = sd(Ellipse.Acc.Axis.2))
view(DATA_MEAN.4)
DATA_MEAN.4T <-DATA %>%
  group_by (Trial) %>%
  summarize(Ellipse.Acc.Major.T = mean(Ellipse.Acc.Axis.2), Ellipse.Acc.Major.SD.T = sd(Ellipse.Acc.Axis.2))
view(DATA_MEAN.4T)
DATA_MEAN.4H <-DATA %>%
  group_by (Height) %>%
  summarize(Ellipse.Acc.Major.H = mean(Ellipse.Acc.Axis.2), Ellipse.Acc.Axis.Major.SD.H = sd(Ellipse.Acc.Axis.2))
view(DATA_MEAN.4H)

###-----Ellipse Axis 2 Graph---###

DATA_MEAN.4$Height <-as.factor(DATA_MEAN.4$Height)

str(DATA)


ggplot(data = DATA, mapping = aes (x = Height, y = Ellipse.Acc.Axis.2)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="95% Ellipse Major Axis (m/s^2)", labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"), 
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN.4, mapping = aes(x = Height, y = Ellipse.Acc.Major), 
        col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
        stroke=1.25)+
  geom_line(data = DATA_MEAN.4, mapping = aes(x = Height, y = Ellipse.Acc.Major, group=1), col = "black", lty=2, lwd=2)


###-----Ellipse Axis 2 Stats---###

mod3 <- lmer(Ellipse.Acc.Axis.2~
                #Fixed Effects:
                Height*Trial+
                #Random Effects:
                (1|Subject)+(1|Height:Subject) + (1|Trial:Subject),
              REML = FALSE, data=DATA)
summary(mod3)

##breaking down interaction

mod3a <- lmer(Ellipse.Acc.Axis.2~
                 #Fixed Effects:
                 Height+
                 #Random Effects:
                 (1|Subject),
               REML = FALSE, data=DATA[DATA$Trial=="Parallel",])
summary(mod3a)

mod3b <- lmer(Ellipse.Acc.Axis.2~
                 #Fixed Effects:
                 Height+
                 #Random Effects:
                 (1|Subject),
               REML = FALSE, data=DATA[DATA$Trial=="Perpendicular",])
summary(mod3b)


#assumptions tests#
plot(fitted(mod3),resid(mod3)) #plots to show linearity - want no clear trend in residuals#
plot(fitted(mod3),resid(mod3)/sd(resid(mod3)))
plot(fitted(mod3),(abs(resid(mod3)/sd(resid(mod3)))))

summary(lm((abs(resid(mod3)/sd(resid(mod3))))~fitted(mod3)))


plot(density(resid(mod3))) 
qqnorm(resid(mod3)/sd(resid(mod3)))#QQ plots to show normality - want fit close as possible to line#
abline(a=0, b=1)
res <- length(resid(mod3))
ks.test(x=resid(mod3)/sd(resid(mod3)),
        y=rnorm(res, 0, 1))

Anova(mod3, type = "3") #effect sizes (i.e., eta squared)#
boxplot(residuals(mod3) ~ DATA$Height)
leveneTest(residuals(mod3) ~ DATA$Height) #tests heteroscedasticity - want > .05#

boxplot(residuals(mod3) ~ DATA$Trial)
leveneTest(residuals(mod3) ~ DATA$Trial) #tests heteroscedasticity - want > .05#


####---RMS SWAY  Acceleration Based---####

###----RMS calculations for Cohens D worksheet-----####
DATA_MEAN.5 <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(RMS.Sway = mean(RMS.Sway.Acc), RMS.Sway.SD = sd(RMS.Sway.Acc))
view(DATA_MEAN.5)
DATA_MEAN.5T <-DATA %>%
  group_by (Trial) %>%
  summarize(RMS.Sway.T = mean(RMS.Sway.Acc), RMS.Sway.SD.T = sd(RMS.Sway.Acc))
view(DATA_MEAN.5T)
DATA_MEAN.5H <-DATA %>%
  group_by (Height) %>%
  summarize(RMS.Sway.H = mean(RMS.Sway.Acc), RMS.Sway.SD.H = sd(RMS.Sway.Acc))
view(DATA_MEAN.5H)

###----------------RMS Sway Graph----####


DATA_MEAN.5$Height <-as.factor(DATA_MEAN.5$Height)

str(DATA)

ggplot(data = DATA, mapping = aes (x = Height, y = RMS.Sway.Acc)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  #geom_point(data = DATA_MEAN6, col = "red", fill = "yellow", alpha=0.7, pch=21, size=3.5, stroke=1.25, inherit.aes = TRUE) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="RMS Sway (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN.5, mapping = aes(x = Height, y = RMS.Sway), 
        col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
        stroke=1.25)+
  geom_line(data = DATA_MEAN.5, mapping = aes(x = Height, y = RMS.Sway, group=1), col = "black", lty=2, lwd=2) 


###----------------RMS Sway Stats----####

mod4 <- lmer(RMS.Sway.Acc~
               #Fixed Effects:
              Height * Trial+
             #Random Effects:
             (1|Subject)+(1|Height:Subject)+(1|Trial:Subject),
             REML = FALSE, data=DATA)
summary(mod4)

##-breaking down interaction effect-#

mod4a <- lmer(RMS.Sway.Acc~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Parallel",])
summary(mod4a)

mod4b <- lmer(RMS.Sway.Acc~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Perpendicular",])
summary(mod4b)

#assumptions tests#
plot(fitted(mod4),resid(mod4)) #plots to show linearity - want no clear trend in residuals#
plot(fitted(mod4),resid(mod4)/sd(resid(mod4)))
plot(fitted(mod4),(abs(resid(mod4)/sd(resid(mod4)))))

summary(lm((abs(resid(mod4)/sd(resid(mod4))))~fitted(mod4)))


plot(density(resid(mod4))) 
qqnorm(resid(mod4)/sd(resid(mod4)))#QQ plots to show normality - want fit close as possible to line#
abline(a=0, b=1)
res <- length(resid(mod4))
ks.test(x=resid(mod4)/sd(resid(mod4)),
        y=rnorm(res, 0, 1))
Anova(mod4, type = "3") #effect sizes (i.e., eta squared)#
boxplot(residuals(mod4) ~ DATA$Height)
leveneTest(residuals(mod4) ~ DATA$Height) #tests heteroscedasticity - want > .05#

boxplot(residuals(mod4) ~ DATA$Trial)
leveneTest(residuals(mod4) ~ DATA$Trial) #tests heteroscedasticity - want > .05#

####---RMS SWAY Frontal ---####

###----RMS Sway Frontal Cohens D calculations
DATA_MEAN.6 <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(RMS.Sway.Coronal = mean(RMS.Sway.Acc.Coronal), RMS.Sway.Coronal.SD = sd(RMS.Sway.Acc.Coronal))
view(DATA_MEAN.6)
DATA_MEAN.6T <-DATA %>%
  group_by (Trial) %>%
  summarize(RMS.Sway.Coronal.T = mean(RMS.Sway.Acc.Coronal), RMS.Sway.Coronal.SD.T = sd(RMS.Sway.Acc.Coronal))
view(DATA_MEAN.6T)
DATA_MEAN.6H <-DATA %>%
  group_by (Height) %>%
  summarize(RMS.Sway.Coronal.H = mean(RMS.Sway.Acc.Coronal), RMS.Sway.Coronal.SD.H = sd(RMS.Sway.Acc.Coronal))
view(DATA_MEAN.6H)

###---RMS Sway Frontal Graph ---###

DATA_MEAN.6$Height <-as.factor(DATA_MEAN.6$Height)

str(DATA)

ggplot(data = DATA, mapping = aes (x = Height, y = RMS.Sway.Acc.Coronal)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  #geom_point(data = DATA_MEAN7, col = "red", fill = "yellow", alpha=0.7, pch=21, size=3.5, stroke=1.25, inherit.aes = TRUE) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="RMS M/L Sway (m/s^2) ")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN.6, mapping = aes(x = Height, y = RMS.Sway.Coronal), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN.6, mapping = aes(x = Height, y = RMS.Sway.Coronal, group=1), col = "black", lty=2, lwd=2)

###----Coronal RMS Sway Stats---###
mod5 <- lmer(RMS.Sway.Acc.Coronal~
               #Fixed Effects:
               Height * Trial+
             #Random Effects:
             (1|Subject)+(1|Height:Subject) + (1|Trial:Subject),
             REML = FALSE, data=DATA)
summary(mod5)

###--interaction effects ---##
mod5a <- lmer(RMS.Sway.Acc.Coronal~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Parallel",])
summary(mod5a)

mod5b <- lmer(RMS.Sway.Acc.Coronal~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Perpendicular",])
summary(mod5b)

#assumptions tests#
plot(fitted(mod5),resid(mod5)) #plots to show linearity - want no clear trend in residuals#
plot(fitted(mod5),resid(mod5)/sd(resid(mod5)))
plot(fitted(mod5),(abs(resid(mod5)/sd(resid(mod5)))))

summary(lm((abs(resid(mod5)/sd(resid(mod5))))~fitted(mod5)))


plot(density(resid(mod5))) 
qqnorm(resid(mod5)/sd(resid(mod5)))#QQ plots to show normality - want fit close as possible to line#
abline(a=0, b=1)
res <- length(resid(mod5))
ks.test(x=resid(mod5)/sd(resid(mod5)),
        y=rnorm(res, 0, 1))

Anova(mod5, type = "3")  #effect sizes (i.e., eta squared)#
boxplot(residuals(mod5) ~ DATA$Height)
leveneTest(residuals(mod5) ~ DATA$Height) #tests heteroscedasticity - want > .05#

boxplot(residuals(mod5) ~ DATA$Trial)
leveneTest(residuals(mod5) ~ DATA$Trial) #tests heteroscedasticity - want > .05#

###---RMS SWAY Sagittal ---####

###----RMS Sway Sagittal Cohens D calculations--##
DATA_MEAN.7 <-DATA %>%
  group_by (Trial, Height) %>%
  summarize(RMS.Sway.Sagittal = mean(RMS.Sway.Acc.Sagittal), RMS.Sway.Sagittal.SD = sd(RMS.Sway.Acc.Sagittal))
view(DATA_MEAN.7)
DATA_MEAN.7T <-DATA %>%
  group_by (Trial) %>%
  summarize(RMS.Sway.Sagittal.T = mean(RMS.Sway.Acc.Sagittal), RMS.Sway.Acc.Sagittal = sd(RMS.Sway.Acc.Sagittal))
view(DATA_MEAN.7T)
DATA_MEAN.7H <-DATA %>%
  group_by (Height) %>%
  summarize(RMS.Sway.Sagittal.H = mean(RMS.Sway.Acc.Sagittal), RMS.Sway.Sagittal.SD.H = sd(RMS.Sway.Acc.Sagittal))
view(DATA_MEAN.7H)


####---RMS Sway Sagittal Graph----###
DATA_MEAN.7$Height <-as.factor(DATA_MEAN.7$Height)

str(DATA)

ggplot(data = DATA, mapping = aes (x = Height, y = RMS.Sway.Acc.Sagittal)) +
  geom_line(aes(group= Subject), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject)), pch=21, size=2, stroke=1.25) +
  #geom_point(data = DATA_MEAN8, col = "red", fill = "yellow", alpha=0.7, pch=21, size=3.5, stroke=1.25, inherit.aes = TRUE) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="RMS A/P Sway (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN.7, mapping = aes(x = Height, y = RMS.Sway.Sagittal), 
        col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
        stroke=1.25)+
  geom_line(data = DATA_MEAN.7, mapping = aes(x = Height, y = RMS.Sway.Sagittal, group=1), col = "black", lty=2, lwd=2)

####---RMS Sway Sagittal Stats----###

mod8 <- lmer(RMS.Sway.Acc.Sagittal~
               #Fixed Effects:
               Height * Trial+
             #Random Effects:
             (1|Subject)+(1|Height:Subject) + (1|Trial:Subject),
             REML = FALSE, data=DATA)
summary(mod8)

##breaking down interaction terms

mod8a <- lmer(RMS.Sway.Acc.Sagittal~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Parallel",])
summary(mod8a)

mod8b <- lmer(RMS.Sway.Acc.Sagittal~
                #Fixed Effects:
                Height+
                #Random Effects:
                (1|Subject),
              REML = FALSE, data=DATA[DATA$Trial=="Perpendicular",])
summary(mod8b)

#assumptions tests#
plot(fitted(mod8),resid(mod8)) #plots to show linearity - want no clear trend in residuals#
plot(fitted(mod8),resid(mod8)/sd(resid(mod8)))
plot(fitted(mod8),(abs(resid(mod8)/sd(resid(mod8)))))

summary(lm((abs(resid(mod8)/sd(resid(mod8))))~fitted(mod8)))


plot(density(resid(mod8))) 
qqnorm(resid(mod8)/sd(resid(mod8)))#QQ plots to show normality - want fit close as possible to line#
abline(a=0, b=1)
res <- length(resid(mod8))
ks.test(x=resid(mod8)/sd(resid(mod8)),
        y=rnorm(res, 0, 1))

Anova(mod8, type = "3")  #effect sizes (i.e., eta squared)#
boxplot(residuals(mod8) ~ DATA$Height)
leveneTest(residuals(mod8) ~ DATA$Height) #tests heteroscedasticity - want > .05#

boxplot(residuals(mod8) ~ DATA$Trial)
leveneTest(residuals(mod8) ~ DATA$Trial) #tests heteroscedasticity - want > .05#



####-------Sub analysis data visualization ------#####

DATAsubset<-read.csv("Standing Subset.csv", header = TRUE, sep=",",  
               na.strings=c("NA","NaN"," ",""))

#DATAsubset$Trial<-as.factor(DATA$Trial)
DATAsubset$Trial<-gsub("1", "Parallel", DATAsubset$Trial)
DATAsubset$Trial<-gsub("3", "Parallel", DATAsubset$Trial)
DATAsubset$Trial<-gsub("2", "Perpendicular", DATAsubset$Trial)
DATAsubset$Trial<-gsub("4", "Perpendicular", DATAsubset$Trial)
DATAsubset$Height<-gsub("L", "Low", DATAsubset$Height)
DATAsubset$Height<-gsub("H", "High", DATAsubset$Height)
DATAsubset$Trial<-as.factor(DATAsubset$Trial)
DATAsubset$Trial <- factor(DATAsubset$Trial, levels = c("Parallel", "Perpendicular"))
DATAsubset$Height <- as.factor(DATAsubset$Height)
DATAsubset$Height<-relevel(DATAsubset$Height, "Low")



DATA_MEAN18 <-DATAsubset %>%
  group_by (Height, Trial) %>%
  summarize(RMS.Sway = mean(RMS.Sway))

DATA_MEAN18$Height <-as.factor(DATA_MEAN18$Height)

str(DATAsubset)

ggplot(data = DATAsubset, mapping = aes (x = Height, y = RMS.Sway)) +
  geom_line(aes(group= Subject.Public.ID), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject.Public.ID)), pch=21, size=2, stroke=1.25) +
  #geom_point(data = DATA_MEAN18, col = "red", fill = "yellow", alpha=0.7, pch=21, size=3.5, stroke=1.25, inherit.aes = TRUE) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="RMS Sway (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_point(data = DATA_MEAN18, mapping = aes(x = Height, y = RMS.Sway), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN18, mapping = aes(x = Height, y = RMS.Sway, group=1), col = "black", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN.5, mapping = aes(x = Height, y = RMS.Sway), 
             col = "grey", fill = "grey", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
   geom_line(data = DATA_MEAN.5, mapping = aes(x = Height, y = RMS.Sway, group=1), col = "grey", lty=2, lwd=2)



DATA_MEAN19 <-DATAsubset %>%
  group_by (Height, Trial) %>%
  summarize(RMS.Sway.Coronal = mean(RMS.Sway.Coronal))

DATA_MEAN19$Height <-as.factor(DATA_MEAN19$Height)

str(DATA)

ggplot(data = DATAsubset, mapping = aes (x = Height, y = RMS.Sway.Coronal)) +
  geom_line(aes(group= Subject.Public.ID), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject.Public.ID)), pch=21, size=2, stroke=1.25) +
  #geom_point(data = DATA_MEAN19, col = "red", fill = "yellow", alpha=0.7, pch=21, size=3.5, stroke=1.25, inherit.aes = TRUE) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="RMS M/L Sway (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_line(data = DATA_MEAN19, mapping = aes(x = Height, y = RMS.Sway.Coronal, group=1), col = "black", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN19, mapping = aes(x = Height, y = RMS.Sway.Coronal), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN.6, mapping = aes(x = Height, y = RMS.Sway.Coronal, group=1), col = "grey", lty=2, lwd=2)+
  geom_point(data = DATA_MEAN.6, mapping = aes(x = Height, y = RMS.Sway.Coronal), 
             col = "grey", fill = "grey", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)



DATA_MEAN20 <-DATAsubset %>%
  group_by (Height, Trial) %>%
  summarize(RMS.Sway.Sagittal = mean(RMS.Sway.Sagittal))

DATA_MEAN20$Height <-as.factor(DATA_MEAN20$Height)

str(DATA)

ggplot(data = DATAsubset, mapping = aes (x = Height, y = RMS.Sway.Sagittal)) +
  geom_line(aes(group= Subject.Public.ID), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject.Public.ID)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="RMS A/P Sway (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_line(data = DATA_MEAN20, mapping = aes(x = Height, y = RMS.Sway.Sagittal, group=1), col = "black", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN20, mapping = aes(x = Height, y = RMS.Sway.Sagittal), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN.7, mapping = aes(x = Height, y = RMS.Sway.Sagittal, group=1), col = "grey", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN.7, mapping = aes(x = Height, y = RMS.Sway.Sagittal), 
             col = "grey", fill = "grey", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)

DATA_MEAN21 <-DATAsubset %>%
  group_by (Height, Trial) %>%
  summarize(Ellipse.Sway.Area = mean(Ellipse.Sway.Area))

DATA_MEAN21$Height <-as.factor(DATA_MEAN21$Height)

str(DATA)

ggplot(data = DATAsubset, mapping = aes (x = Height, y = Ellipse.Sway.Area)) +
  geom_line(aes(group= Subject.Public.ID), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject.Public.ID)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="95% Ellipse Sway Area (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
    geom_line(data = DATA_MEAN21, mapping = aes(x = Height, y = Ellipse.Sway.Area, group=1), col = "black", lty=2, lwd=2) +
    geom_point(data = DATA_MEAN21, mapping = aes(x = Height, y = Ellipse.Sway.Area), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
    geom_line(data = DATA_MEAN, mapping = aes(x = Height, y = Ellipse.Sway.Area.1, group=1), col = "grey", lty=2, lwd=2) +
    geom_point(data = DATA_MEAN, mapping = aes(x = Height, y = Ellipse.Sway.Area.1), 
             col = "grey", fill = "grey", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)

DATA_MEAN22 <-DATAsubset %>%
  group_by (Height, Trial) %>%
  summarize(Ellipse.Axis.1.Radius = mean(Ellipse.Axis.1.Radius))

DATA_MEAN22$Height <-as.factor(DATA_MEAN22$Height)

str(DATA)

ggplot(data = DATAsubset, mapping = aes (x = Height, y = Ellipse.Axis.1.Radius)) +
  geom_line(aes(group= Subject.Public.ID), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject.Public.ID)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="95% Ellipse Minor Axis (m/s^2)", labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_line(data = DATA_MEAN22, mapping = aes(x = Height, y = Ellipse.Axis.1.Radius, group=1), col = "black", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN22, mapping = aes(x = Height, y = Ellipse.Axis.1.Radius), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN.3, mapping = aes(x = Height, y = Ellipse.Acc.Axis, group=1), col = "grey", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN.3, mapping = aes(x = Height, y = Ellipse.Acc.Axis), 
             col = "grey", fill = "grey", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)


DATA_MEAN23 <-DATAsubset %>%
  group_by (Height, Trial) %>%
  summarize(Ellipse.Axis.2.Radius = mean(Ellipse.Axis.2.Radius))

DATA_MEAN23$Height <-as.factor(DATA_MEAN23$Height)

str(DATA)

ggplot(data = DATAsubset, mapping = aes (x = Height, y = Ellipse.Axis.2.Radius)) +
  geom_line(aes(group= Subject.Public.ID), col = "grey") + 
  geom_point(aes(fill=as.factor(Subject.Public.ID)), pch=21, size=2, stroke=1.25) +
  facet_wrap(~Trial) +
  scale_y_continuous(name="95% Ellipse Major Axis (m/s^2)")+
  scale_x_discrete(name="")+
  theme_bw()+ theme(legend.position = "none")+
  theme(axis.text=element_text(size=12, colour="black"),
        axis.title=element_text(size=16,face="bold"), 
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text.x = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.position="none")+
  theme(text=element_text(family="Times New Roman", face="bold")) +
  geom_line(data = DATA_MEAN23, mapping = aes(x = Height, y = Ellipse.Axis.2.Radius, group=1), col = "black", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN23, mapping = aes(x = Height, y = Ellipse.Axis.2.Radius), 
             col = "black", fill = "black", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)+
  geom_line(data = DATA_MEAN.4, mapping = aes(x = Height, y = Ellipse.Acc.Major, group=1), col = "grey", lty=2, lwd=2) +
  geom_point(data = DATA_MEAN.4, mapping = aes(x = Height, y = Ellipse.Acc.Major), 
             col = "grey", fill = "grey", alpha=0.9, pch=21, size=3.5, 
             stroke=1.25)

