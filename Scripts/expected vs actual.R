library(tidyverse)
library(ggplot2)

pHData <- read.csv("Data/pH_temp/ph_temp_final.csv")

##ph plots
pHData$discrete <- cut(pHData$expected_temp, breaks = 2, labels = c("Low", "High"))
ggplot(data=pHData, aes(x=expected_pH,y=pH, group = interaction(expected_pH,discrete), fill = discrete, color = discrete))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  scale_y_continuous(breaks = seq(7.0, 8.1, by=0.1), limits=c(7.0,8.1))+
  scale_x_continuous(breaks = seq(7.1, 8.1, by=0.1), limits=c(7.1,8.1))+
  scale_fill_manual(values=c("Low" = "#0072B2", "High" = "tomato"))+
  ##geom_jitter(width=0.25, pch=21, color="white", size=2) +
  scale_color_manual(values=c("Low" = "#0072B2", "High" = "tomato"))+
  labs(x="Expected pH", y= "Measured pH")+
  theme_bw()

##daily data
todayspH <- subset(pHData, date %in% c("20240404"))
todayspH$discrete <- cut(todayspH$expected_temp, breaks = 2, labels = c("Low", "High"))
ggplot(data=todayspH, aes(x=expected_pH,y=pH, group = interaction(expected_pH,discrete), fill = discrete, color = discrete))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  scale_y_continuous(breaks = seq(7.0, 8.1, by=0.1), limits=c(7.0,8.1))+
  scale_x_continuous(breaks = seq(7.1, 8.1, by=0.1), limits=c(7.1,8.1))+
  scale_fill_manual(values=c("Low" = "#0072B2", "High" = "tomato"))+
  ##geom_jitter(width=0.25, pch=21, color="white", size=2) +
  scale_color_manual(values=c("Low" = "#0072B2", "High" = "tomato"))+
  labs(x="Expected pH", y= "Measured pH")+
  theme_bw()

##temp plots

#expected vs actual
pHData$discrete <- cut(pHData$expected_temp, breaks = 2, labels = c("Low", "High"))
ggplot(data=pHData, aes(x=expected_temp,y=TempInSitu, group=expected_temp, fill=discrete))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  scale_fill_manual(values=c("Low" ="#0072B2", "High"="tomato"))+
  geom_jitter(width=0.25, pch=21, size=2) +
  labs(x="Expected temp", y= "Measured temp")+
  theme_bw()

#with pH included
ggplot(data=na.omit(pHData), aes(x=pH,y=TempInSitu, group = interaction(expected_temp,pH), fill = pH, color = pH))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  ##geom_jitter(width=0.25, pch=21, size=2) +
  labs(x="pH", y= "Measured temp")+
  theme_bw()

##time series
ggplot(data=pHData, aes(x=date,y=TempInSitu, group=expected_temp, fill=discrete))+
  geom_smooth(method="lm") + 
  scale_fill_manual(values=c("Low" ="lightblue", "High"="lightpink"))+
  geom_jitter(width=0.25, pch=21, size=2) +
  labs(x="Date", y= "Temperature")+
  theme_bw()