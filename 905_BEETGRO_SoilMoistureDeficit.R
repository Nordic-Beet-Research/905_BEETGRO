############################################
############################################
##
## BBRO BEETGRO MODEL - SOIL MOISTURE DEFICIT (SMD)
##
## v1.0 - TRANSPOSING OPENMODEL VERSION TO R
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-01
##
## Current status:
## - 
## - have added in (a slightly modified version of) BBCH as the record of growth stage
## -- 00: seed prior to sowing, 01: sown seed, 09: emergence, 31-9: ground coverage 10-90%, 49: harvestable size, 99: harvested.
## 
############################################
############################################

# Setup

{
  # -------------------------------------------
  snapshot_date = "2021-11-01"
  options("repos" = paste0("https://mran.revolutionanalytics.com/snapshot/", snapshot_date))
  # -------------------------------------------
  
  # -------------------------------------------
  # sink options
  options(width = 150)
  # rJava memory option
  options(java.parameters = "-Xmx8000m")
  # -------------------------------------------
  
  # R packages
  # -------------------------------------------
  Rpackages_version = c(
    "ggplot2_3.3.5",
    "dplyr_1.0.7",
    "tidyr_1.1.4",
    "readxl_1.3.1",
    "writexl_1.4.0",
    "lubridate_1.8.0"
  )
  
  path_Rpackages = "C:/R packages_412"
  # -------------------------------------------
  
  # version check and load packages
  # -------------------------------------------
  # R version check
  if(sessionInfo()$R.version$version.string != "R version 4.1.2 (2021-11-01)") stop("R.version must be 4.1.2 (2021-11-01)")
  
  # install packages
  Rpack = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[1])
  Rpack_version = sapply(strsplit(Rpackages_version, "_", fixed = T), FUN = function(x) x[2])
  if(!all(Rpack %in% list.files(path_Rpackages))){
    loadRpackages <- Rpack[!Rpack %in% list.files(path_Rpackages)]
    for(i in loadRpackages) install.packages(i, lib = path_Rpackages, repos = options("repos"), dependencies = T)
  }
  
  # load packages
  for(i in Rpack) eval(parse(text = paste0("library(", i, ", lib.loc = '", path_Rpackages, "')")))
  
  rm(list = ls())
}

############################################
# IMPORT SOME EXAMPLE DATA
# read in model parameters
dat_in_param <- pivot_wider(read_xlsx("parameters.xlsx", sheet = "parameters"),names_from = parameter)
# read in trial data
dat_in_trial <- read_xlsx("TrialData.xlsx", sheet = "TrialData")
# read in weather data
dat_in_weath <- read_xlsx("dat_weather_2021_40141.xlsx", sheet = "Sheet1")
# read in weather data
dat_in_canopy <- read_xlsx("dat_canopy_2021.xlsx", sheet = "Sheet1")

############################################
# CREATE FOUNDATION DATAFRAME

i=7L

dat_in_trial_i <- dat_in_trial[i,]

## Create base data frame for each day of the year
smd_df <- data.frame(doy = seq(1:365),
                     smd = 20) %>%

## Join the weather data (NEED TO ADD IN )
  left_join(
    select(dat_in_weath, c(doy, et)), 
    by = "doy") %>% 
  left_join(dat_in_canopy, by = "doy") %>% 

############################################
# SOIL MOISTURE DEFICIT
## Update data frame with SMD = 1 at sowing and all dates thereafter
  mutate(smd = replace(smd, bbch >= 01, 1),
         
         ### Daily soil surface evaporation (SSE)         
         dSSE = ifelse(f < 1, min(1.5, et)*(1-f), 0))

### Ea Actual Crop Evapotranspiration
smd_df <- smd_df %>%
  rowwise() %>% 
  mutate(smd = replace(smd, smd < 0, 0),
         Eatmos = 1.2*f*et,
         Eatmos = replace(Eatmos, Eatmos <= 0, 0.001),
         q = max(dat_in_trial_i$qfc - lag(smd)/(root_depth*1000), 0.01))
,
         
         Esoil = 2,
         Ea = min(Eatmos, Esoil),
         dSMD = dSSE + Ea - neda,
         smd = lag(smd) + dsmd)
