#load libraries
library(tidyverse)

#read data
respo <- read_csv(here("Data","PR_2024","Metadata_may.csv"))
respo <- respo %>%
  filter(Species == "Tegula")
calc <- read_csv(here("Data", "TA", "May_calcification.csv"))

#get initial data
rows.initial <- which(calc$sample_type == "Initial") #tells you all the rows that you have with initial
initial <- calc[rows.initial,]

#take means of initial data
initial_mean <- initial %>%
  group_by(tank) %>%
  summarise(TA_initial = mean(TA))

#bring initials into data frame by tank
calc <- calc %>%
  left_join(initial_mean, by= "tank")

#separate blanks
rows.blank <- which(calc$sample_type == "Blank") #tells you all the rows that you have with blank
blanks <- calc[rows.blank,]

#figure out delta TA, initial-final
blanks$delta.TA.blank <- blanks$TA_initial - blanks$TA

#add blank column
mean.blanks <- blanks %>% 
  group_by(tank) %>%
  summarise(mean.blanks=mean(delta.TA.blank)) 

#bring in mean blanks to calc.data
calc <- left_join(calc, mean.blanks) 

ggplot(aes(x=factor(pH_treatment),y=blanks, color=temp_treatment))+
  geom_jitter(position=position_dodge())+
  theme(axis.text.x = element_blank())
