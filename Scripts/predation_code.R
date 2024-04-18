# for rockweed predation 
# by: tena 
# updated: 04/16/2024

####-----libraries-----####
library(tidyverse)
library(here)
library(janitor)

####-----load data------####
predation <- read_csv(here("Data","growth","rockweed_weights.csv"))

####-----calculating weight change------####
predation_wide <- predation %>%
  pivot_wider(values_from = weight_g, names_from=date) %>% #combine data by date
  clean_names() %>%
  mutate(percent_change=100*(x20240417-x20240410)/x20240410) #calculate diff between weeks

####-----creating plots------####
##plot by predation
predation_wide %>%
  ggplot(aes(x=factor(p_h),y=percent_change, color=factor(temperature)))+
  geom_boxplot()+
  geom_jitter(position=position_dodge())+
  facet_wrap(~predator)

##plot by temperature
predation_wide %>%
  ggplot(aes(x=factor(p_h),y=percent_change, color=predator))+
  geom_boxplot()+
  geom_jitter(position=position_dodge())+
  facet_wrap(~temperature)
