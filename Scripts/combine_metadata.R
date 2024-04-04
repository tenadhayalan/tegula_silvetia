### Combining experimental design and PR metadata ############################
# Created by Jamie Kerlin
# Created on 2021-08-13
# Updated on 2022_03_21 to have surface area measurements
##############################################################################

### Load libraries ###########################################################
library(tidyverse)
library(here)

### Load data ################################################################
PRmetadata <- read_csv(here("Short_term", 
                            "Data_Raw", 
                            "PR",
                            "PR_final",
                            "Metadata",
                            "PR_final_metadata.csv"))

PRmetadata_initial <- read_csv("Short_term/Data_Raw/PR/PR_initial/Metadata/PR_initial_metadata_combined.csv")

PRvolume <- read_csv(here("Short_term",
                          "Data_Raw",
                          "PR",
                          "PR_final",
                          "Metadata",
                          "PR_final_volume_09132021.csv"))

PRsurfacearea <- read_csv(here("Short_term/Data/Surface_area/surface_area_post_exp.csv"))

PRsurfacearea_initial <- read_csv(here("Short_term/Data/Surface_area/surface_area_pr_initial.csv"))


#weight <- read_csv(here("Short_term",
                        #"Data_Raw",
                        #"PR",
                        #"PR_final",
                        #"Metadata",
                        #"buoyant_weight_post.csv")) #not using anymore


### Fix & Combine #############################################################
#PRmetadata <- PRmetadata %>%
 # mutate(Donor_Colony = FileName) %>% #copy file name into new column
  #separate(Donor_Colony, sep = "_", #separate the file name into new columns by underscore
   #        into = c("BLOCK", "CHANNEL", "TREATMENT", "Donor_Colony",
    #                "DATE", "O2")) %>%
  #select(!c("BLOCK", "CHANNEL", "TREATMENT", "DATE", "O2")) #deselect columns I don't need

PRvolume <- PRvolume %>%
  filter(notes == "good") #removing first trials of fragments that were run 2 times

PRmetadata <- PRmetadata %>%
  filter(notes == "good") 

combined <- left_join(PRmetadata, PRvolume)

# combined$BLANK <- ifelse(combined$Donor_Colony == "BLANK", 1, 0) #automatically assign binary to channels
#didn't use this for final PR, just initial
 
PRsurfacearea1 <- PRsurfacearea %>%
  select(FragmentID, totalSA_cm2)

combined2 <- left_join(combined, PRsurfacearea1) 

combined2 %>%
  write_csv(here("Short_term",
                 "Data_Raw",
                 "PR",
                 "PR_final",
                 "Metadata",
                 "PR_metadata_combined_2022.csv"))


### Same for initial metadata ##########################################
PRsurfacearea_initial1 <- PRsurfacearea_initial %>%
  rename(Donor_Colony = FragmentID) %>%
  select(Donor_Colony, Surface_area_cm2)


combined_initial <- left_join(PRmetadata_initial, PRsurfacearea_initial1) 

combined_initial %>%
  write_csv(here("Short_term",
                 "Data_Raw",
                 "PR",
                 "PR_initial",
                 "Metadata",
                 "PR_initial_metadata_combined_2022.csv"))


