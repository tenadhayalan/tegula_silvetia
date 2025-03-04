# for rockweed predation 
# by: tena 
# updated: 04/16/2024

####-----libraries-----####
library(tidyverse)
library(here)
library(janitor)

####-----load data------####
predation <- read_csv(here("Data","growth","rockweed_weights_may.csv"))


####-----calculating percent weight change------####
predation_wide <- predation %>%
  pivot_wider(values_from = c(weight_g, full_weight), names_from=date) %>% #combine data by date
  clean_names()%>%
  mutate(percent_change_week1=100*(full_weight_20240525-weight_g_20240518)/weight_g_20240518)%>% #calculate diff between weeks
  mutate(percent_change_week2=100*(full_weight_20240601-weight_g_20240525)/weight_g_20240525)%>%
  mutate(percent_change_week3=100*(full_weight_20240608-weight_g_20240601)/weight_g_20240601)%>%
  mutate(percent_change_week4=100*(full_weight_20240613-weight_g_20240608)/weight_g_20240608)%>%
  mutate(percent_change=100*(full_weight_20240613-weight_g_20240518)/weight_g_20240518)

####-----creating plots------####
##plot by predation
predation_wide %>%
  ggplot(aes(x=factor(p_h),y=percent_change, color=factor(temperature)))+
  geom_boxplot()+
  geom_jitter(position=position_dodge())+
  facet_wrap(~predator)

##plot percent change by temperature
predation_wide %>%
  ggplot(aes(x=factor(p_h),y=percent_change, color=predator))+
  geom_boxplot()+
  geom_jitter(position=position_dodge())+
  facet_wrap(~temperature)+
  labs(color = "Treatment", x= "pH", y= "Change in rockweed weight (%)")+
  scale_color_manual(labels = c("Control", "Predator"), values = c("cyan3", "red1"))
  

####-----stats-----####
predation_wide$predator <- as.factor(predation_wide$predator)
predation_wide$temperature <- as.factor(predation_wide$temperature)
predation_wide$p_h <- as.factor(predation_wide$p_h)

predationmodel <- lm(percent_change ~ predator * temperature * p_h + (1|tank), data=predation_wide)

plot(predationmodel)
anova(predationmodel)
