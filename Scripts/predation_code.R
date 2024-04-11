library(tidyverse)
predation <- read.csv("Data/growth/weight_diff.csv")

##by temperature
ggplot(data=predation, aes(x=temperature,y=weight, group = interaction(predator,temperature), fill = predator, color = predator))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  labs(x="Temperature", y= "Difference in rockweed weight (g)")+
  theme_bw()

##by pH
ggplot(data=predation, aes(x=pH,y=weight, group = interaction(predator,pH), fill = predator, color = predator))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  scale_x_continuous(breaks = seq(7.1, 8.1, by=0.1), limits=c(7.1,8.1))+
  labs(x="pH", y= "Difference in rockweed weight (g)")+
  theme_bw()

##by temperature and pH
ggplot(data=predation, aes(x=tank,y=weight, group = interaction(predator,tank), fill = predator, color = predator))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  ##scale_x_continuous(breaks = seq(7.1, 8.1, by=0.1), limits=c(7.1,8.1))+
  labs(x="tank", y= "Difference in rockweed weight (g)")+
  theme_bw()
