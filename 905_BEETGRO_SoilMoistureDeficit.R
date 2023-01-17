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

############################################
# CREATE FOUNDATION DATAFRAME

## Create base data frame for each day of the year
smd_df <- data.frame(doy = seq(1:365),
                     smd = 0,
                     bbch = 00) # browseURL("https://en.wikipedia.org/wiki/BBCH-scale_(beet)")


## Join the weather data (NEED TO ADD IN )
smd_df <- smd_df %>% 
  left_join(
    select(dat_in_weath, c(doy, et)), 
    by = "doy")

i=7L

sow_date <- date(as.POSIXct(unlist(dat_in_trial[i,"SowDate"]), origin = '1970-01-01'))
emerg_date <- date(as.POSIXct(unlist(dat_in_trial[i,"EmDate"]), origin = '1970-01-01'))
harvest_date <- date(as.POSIXct(unlist(dat_in_trial[i,"HarvestDate"]), origin = '1970-01-01'))
sow_doy <-yday(sow_date)
emerg_doy <-yday(emerg_date)
harvest_doy <-yday(harvest_date)

## Update data frame with SMD = 1 at sowing and BBCH = 01 for all dates fr.o.m sowing
smd_df <- smd_df %>% 
  mutate(smd = replace(smd, doy >= sow_doy, 1),
         bbch = replace(bbch, doy >= sow_doy, 01))

############################################
# SOIL MOISTURE DEFICIT

## Calculate daily change
### Daily soil surface evaporation (SSE)
smd_df <- smd_df %>%
  mutate(dSSE = ifelse(f < 1, min(1.5, et)*(1-f), 0))

#mutate
dSMD <- dSSE + Ea - rain
SMD <- lag(SMD) + dSMD