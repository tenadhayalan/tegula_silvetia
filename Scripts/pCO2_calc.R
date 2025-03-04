## Calculate pCO2 using pH and TA
# Created by Tena Dhayalan
# Edited on 02/22/2025
library(tidyverse)
library(seacarb)
library(broom)
library(here)
library(lubridate)
library(ggridges)
library(jtools)
library(interactions)
library(sandwich)
library(patchwork)

## bring in pH and TA data

april <- read.csv("Data/TA/April_background_TA.csv")
may <- read.csv("Data/TA/May_background_TA.csv")

# april
aprilcarb <- carb(8, var1 = april$pH, var2 = april$TA, S = april$Salinity_lab, T = april$TempInSitu, pHscale = "T")

april <- april %>%
  left_join(aprilcarb)

april %>%
  ggplot(aes(x = SampleID, y = pCO2, group = SampleID)) +
  geom_boxplot()

write.csv(april, "Data/pH_temp/april_carb.csv")

# may
maycarb <- carb(8, var1 = may$pH, var2 = may$TA, S = may$Salinity_lab, T = may$TempInSitu, pHscale = "T")

may <- may %>%
  left_join(maycarb)

may %>%
  ggplot(aes(x = SampleID, y = pCO2, group = SampleID)) +
  geom_boxplot()

write.csv(may, "Data/pH_temp/may_carb.csv")
  
## take the mV calibration files by each date and use them to calculate pH
pHSlope<-pHcalib %>%
  # extract only the orion data
  nest_by(TrisCalDate)%>%
  mutate(fitpH = list(lm(mVTris~TTris, data = data))) %>% # linear regression of mV and temp of the tris
  summarise(broom::tidy(fitpH)) %>% # make the output tidy
  dplyr::select(TrisCalDate, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%# put slope and intercept in their own column
  right_join(.,pHData) %>% # join with the pH sample data
  mutate(mVTris = TempInSitu*TTris + `(Intercept)`) %>% # calculate the mV of the tris at temperature in which the pH of samples were measured
  #  drop_na(TempInSitu)%>%
  drop_na(mV) %>%
  mutate(pH = pH(Ex=mV,Etris=mVTris,S=Salinity_lab,T=TempInSitu))  # calculate pH of the samples using the pH seacarb function

view(pHSlope)

write.csv(pHSlope, "Data/pH_temp/ph_temp_may_final.csv")
