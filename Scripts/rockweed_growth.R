# for rockweed growth 
# by: tena 
# updated: 04/26/2024

####-----libraries-----####
library(tidyverse)
library(here)
library(janitor)

####-----load data------####
rw <- read_csv(here("Data","growth","rockweed_weights_may.csv"))

####-----make plot-----####
rw %>%
  ggplot(aes(x=factor(date),y=weight_g, color=predator, shape=factor(temperature)))+
  geom_line(aes(group=factor(rockweed_ID)))+
  geom_jitter(position=position_dodge())+
  facet_wrap(~tank)+
  theme(axis.text.x = element_blank())
