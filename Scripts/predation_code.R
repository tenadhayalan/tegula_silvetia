# for rockweed predation 
# by: tena 
# updated: 04/16/2024

####-----libraries-----####
library(tidyverse)
library(here)
library(janitor)

####-----load data------####
predation <- read_csv(here("Data","growth","rockweed_weights.csv"))
predationNew <- predation
predationNew$date[predationNew$date == 20240428] <- 20240429


####-----calculating percent weight change------####

predation_wide <- predationNew %>%
  pivot_wider(values_from = weight_g, names_from=date) %>% #combine data by date
  clean_names() %>%
  mutate(percent_change=100*(x20240429-x20240403)/x20240403) #calculate diff between weeks

####-----calculating absolute weight change------####
predation_wide <- predation %>%
  pivot_wider(values_from = weight_g, names_from=date) %>% #combine data by date
  clean_names() %>%
  mutate(absolute_change=(x20240417-x20240403)) #calculate diff between weeks

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
  

##plot absolute change by temperature
predation_wide %>%
  ggplot(aes(x=factor(p_h),y=absolute_change, color=predator))+
  geom_boxplot()+
  geom_jitter(position=position_dodge())+
  facet_wrap(~temperature)

####-----stats-----####
predation_wide$predator <- as.factor(predation_wide$predator)
predation_wide$temperature <- as.factor(predation_wide$temperature)
predation_wide$p_h <- as.factor(predation_wide$p_h)

predationmodel <- lm(percent_change ~ predator * temperature * p_h + (1|tank), data=predation_wide)

plot(predationmodel)
anova(predationmodel)
