### PI curve #####################################################
# Created by Jamie Kerlin, edited by Tena Dhayalan
##################################################################

#Install Packages 

if ("github" %in% rownames(installed.packages()) == 'FALSE') install.packages('github') 
if ("segmented" %in% rownames(installed.packages()) == 'FALSE') install.packages('segmented') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("LoLinR" %in% rownames(installed.packages()) == 'FALSE') devtools::install_github('colin-olito/LoLinR') 
if ("lubridate" %in% rownames(installed.packages()) == 'FALSE') install.packages('lubridate') 
if ("chron" %in% rownames(installed.packages()) == 'FALSE') install.packages('chron') 
if ("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 
if ("here" %in% rownames(installed.packages()) == 'FALSE') install.packages('here') 
if ("patchwork" %in% rownames(installed.packages()) == 'FALSE') install.packages('patchwork') 


#Read in required libraries
##### Include Versions of libraries
library("segmented")
library("plotrix")
library("gridExtra")
library("LoLinR")
library("lubridate")
library("chron")
library('tidyverse')
library('here')
library("patchwork")

# get the file path


#set the path to all of the raw oxygen datasheets
path.p<-here("Data",
             "PR_2024",
             "RawO2",
             "PI_curve") #the location of all your respirometry files

# bring in all of the individual files
file.names<-basename(list.files(path = path.p, pattern = "csv$", recursive = TRUE)) #list all csv file names in the folder and subfolders

#basename above removes the subdirectory name from the file, re-nale as file.names.full
file.names.full<-list.files(path = path.p, pattern = "csv$", recursive = TRUE) 

#generate a 3 column dataframe with specific column names
# data is in umol.L.sec
Respo.R<- data.frame(matrix(NA, nrow=length(file.names), ncol=5))
Respo.R <- tibble(
  FileName = character(),
  run = integer(),
  run_id = character(),
  umol.L.sec = numeric(),
  Intercept = numeric(),
  Temp.C = numeric()
)
#View(Respo.R)

#Load your respiration data file, with all the times, water volumes(mL), snail weight (dry weight) (g)
Sample.Info <- read_csv(file = here("Data",
                                    "PR_2024",
                                    "Metadata_PI.csv"))
#View(Sample.Info)

##### Make sure times are consistent ####

# make start and stop times real times, so that we can join the data frames
Sample.Info$Date <- as.Date(as.character(Sample.Info$Date), format = "%Y%m%d")  # make this into a real date
Sample.Info$start.time <- as.POSIXct(paste(Sample.Info$Date, Sample.Info$start.time),format="%Y-%m-%d %H:%M:%S", tz = "") #convert time with date
Sample.Info$stop.time <- as.POSIXct(paste(Sample.Info$Date, Sample.Info$stop.time),"%Y-%m-%d %H:%M:%S", tz = "") #convert time with date

#view(Sample.Info)

for (i in 1:length(file.names.full)) {
  
  # Extract base file name
  rename <- sub(".csv", "", file.names[i])
  
  # Filter Sample.Info to get all runs for this file
  runs <- Sample.Info %>% filter(FileName == rename)
  
  # Load the full data file
  Respo.Data1 <- read_csv(file.path(path.p, file.names.full[i]), skip = 1) %>%
    dplyr::select(Date, Time, Value, Temp) %>%
    mutate(Date = as.Date(as.character(Date), format = "%m/%d/%Y")) %>%
    mutate(Time = as.POSIXct(paste(Date, Time),"%Y-%m-%d %H:%M:%S", tz = "")) %>%
    drop_na()
  
  
  # Start after 2 minutes (120 seconds)
  Respo.Data1 <- Respo.Data1[-c(1:120),] %>%
    mutate(sec_total = 1:n())  # total seconds since beginning of file
  
  # Loop through runs within this file
  for (j in 1:nrow(runs)) {
    
    # Extract this run’s start and stop times
    run_start <- as.POSIXct(runs$start.time[j], format = "%H:%M:%S", tz = "")
    run_stop <- as.POSIXct(runs$stop.time[j], format = "%H:%M:%S", tz = "")
    
    # Subset to data for this run
    run_data <- Respo.Data1 %>%
      filter(Time >= run_start & Time <= run_stop) %>%
      mutate(sec = row_number()) # sets sec as only within this run
    
    # thin this run's data
    thin_val <- thinData(run_data[, c("sec", "Value")], by = 20)$newData1
    colnames(thin_val) <- c("sec", "Value")
    
    thin_temp <- thinData(run_data[, c("sec", "Temp")], by = 20)$newData1
    colnames(thin_temp) <- c("sec", "Temp")
    
    # Combine
    thinned <- thin_val %>%
      mutate(Temp = thin_temp$Temp)
    
    # Apply rank-local-regression
    Regs <- rankLocReg(
      xall = thinned$sec,
      yall = thinned$Value,
      alpha = 0.5,
      method = "pc",
      verbose = TRUE
    )
    
    # Store result
    Respo.R <- add_row(Respo.R,
                       FileName = rename,
                       run = runs$run[j],  # uses run from Sample.Info
                       run_id = paste0(tools::file_path_sans_ext(rename), "_", runs$run[j]), # unique run x file name
                       umol.L.sec = Regs$allRegs[1, 5],
                       Intercept = Regs$allRegs[1, 4],
                       Temp.C = mean(thinned$Temp, na.rm = TRUE)
    )
  }
}

#export raw data and read back in as a failsafe 
#this allows me to not have to run the for loop again 
write_csv(Respo.R, here("Data",
      "PR_2024",
      "PI_respo.csv"))  


####----after loop----####
Sample.Info <- read_csv(file = here("Data",
              "PR_2024",
              "Metadata_PI.csv"))
Respo.R <- read_csv(here("Data",
        "PR_2024",
        "PI_respo.csv"))  

# Calculate Respiration rate

Sample.Info <- Sample.Info %>%
  mutate(BLANK = as.factor(BLANK)) %>%
  mutate(run_id = paste0(FileName, "_", run))

Respo.R <- Respo.R %>%
  left_join(Sample.Info) %>% # Join the raw respo calculations with the metadata
  mutate(umol.sec = umol.L.sec*(volume/1000)) %>% #Account for chamber volume to convert from umol L-1 s-1 to umol s-1. This standardizes across water volumes and removes per Liter
  mutate_if(sapply(., is.character), as.factor) %>% #convert character columns to factors
  mutate(BLANK = as.factor(BLANK)) #make the blank column a factor

#View(Respo.R)

#Account for blank rate by light/Dark and Block (if we do one blank per block)

#View(Respo.R)

# Step 1: Normalize respiration data
Respo.R.Normalized <- Respo.R %>%
  group_by(run, pH_treatment, Light_Dark, Species, BLANK) %>%
  summarise(umol.sec = mean(umol.sec, na.rm = TRUE), .groups = 'drop') %>%
  filter(BLANK == 1) %>%
  dplyr::select(run, pH_treatment, Light_Dark, Species, blank.rate = umol.sec) %>%
  right_join(Respo.R, by = c("run", "pH_treatment", "Light_Dark", "Species")) %>%
  mutate(
    AFDW = as.numeric(as.character(AFDW)), # Convert to numeric
    umol.sec.corr = umol.sec - blank.rate,
    umol.gram.hr = ((umol.sec.corr * 3600) / AFDW)) %>%
  filter(BLANK == 0)


#PI curve fit

#renaming variables
PAR = as.numeric(Respo.R.Normalized$run)
Pc = as.numeric(Respo.R.Normalized$umol.gram.hr)

summary(PAR)
summary(Pc)
plot(PAR, Pc)

#fit a model using a Nonlinear Least Squares regression of a non-rectangular hyperbola (Marshall & Biscoe, 1980)
curve.nlslrc= nlsLM(Pc ~ (1/(2*theta))*(AQY*PAR+Pmax-sqrt((AQY*PAR+Pmax)^2-4*AQY*theta*Pmax*PAR))-Rd,
                  start=list(
                    Pmax=(max(Pc)-min(Pc)),
                    AQY= 0.001,
                    Rd=abs(min(Pc)),
                    theta=0.6
                    ),
                  control = nls.lm.control(maxiter = 200)
)

#summary of model fit
my.fit <- summary(curve.nlslrc)

#define model fit (hyperbolic tangent, more common way to fit) 

model_fun <- function(x) {
  (1/(2*summary(curve.nlslrc)$coef[4,1]))*
    (summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1]-sqrt((summary(curve.nlslrc)$coef[2,1]*x+summary(curve.nlslrc)$coef[1,1])^2-4*summary(curve.nlslrc)$coef[2,1]*summary(curve.nlslrc)$coef[4,1]*summary(curve.nlslrc)$coef[1,1]*x))-summary(curve.nlslrc)$coef[3,1]
}


#extract parameters
#Amax (max gross photosynthetic rate)
Pmax.gross <- my.fit$parameters[1]

#AQY (apparent quantum yield) alpha
AQY <- my.fit$parameters[2]

#Rd (dark respiration)
Rd <- my.fit$parameters[3]

# Ik light saturation point
Ik <- Pmax.gross/AQY


# plot PI curve
Respo.R.Normalized %>%
  ggplot(aes(x = run, y = umol.gram.hr, color = as.factor(temp_treatment)))+
  geom_point(alpha = 0.3)+
  theme_bw()+
  stat_function(fun = model_fun, color = "#0EAD69", size = 1.2) +
  scale_color_manual(values = c("skyblue2", "firebrick1")) +
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 20)) +
  labs(x = expression("Photon Flux Density ("*mu*"mol photons "*m^-2*s^-1*")"),
       y = expression(Net~Photosynthesis* " ("*mu*"mol "*O[2]*" "*g^-2*h^-1*")"),
       color = expression(paste("Temperature (",degree,"C)")))+
  geom_vline(xintercept = Ik, col = "black", lty = 3, lwd = 1)+ 
  annotate("text", x = 425, y = 1.0, label = expression(I[k]))
  

ggsave("PI curve.png", path=here("Output"), width = 10, height = 6)

# Ic light compensation point

Ic <- Rd/AQY

# Net photosynthetic rates

Pmax.net <- Pmax.gross-Rd


#output parameters into a table

PI.curve <- as.data.frame(cbind(Pmax.gross, Pmax.net, -Rd, AQY,Ik,Ic)) %>%
  write_csv(here("Data", "PR_2024", "PIcurve_values.csv"))
