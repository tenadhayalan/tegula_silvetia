library(tidyverse)

tegula <- read.csv("Data/growth/20240329_tegula_initial.csv")

tegula$pH= as.character(tegula$pH)

ggplot(data=na.omit(tegula), aes(x=pH,y=blotted_wet_weight_g, group=interaction(pH,temp),fill = temp, color = temp))+
  geom_boxplot(alpha=0.5, outlier.alpha=0) + 
  geom_jitter(width=0.25, pch=21, size=2) +
  labs(x="pH", y= "Wet weight (g)")+
  theme_bw()
