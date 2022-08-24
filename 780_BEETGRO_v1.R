############################################
############################################
##
## BBRO BEETGRO MODEL
##
## v1.0 - TRANSPOSING OPENMODEL VERSION TO R
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-01
##
## 
## Aim is to build a system that: 
## - reads in all the trial data from a single spreadsheet. Each TrialID is a unique trial site x year combination.
## - reads in all the site weather data from site specific spreadsheets. Multiple years are currently housed in single site files. 
## -- This set-up suits most weather data systems, thus lends itself to automation. 
## -- It also means that with careful management of weather data files, .csv files for each site can be saved locally.
## - fills in any blanks in the data where possible.
## - runs through the analysis for each TrialID and stores the results as appropriate.
## - makes the results viewable via one or two dashboards
##
## Current status:
## - Ready to add in the soil moisture deficit (SMD) and canopy cover models.
## - DON'T FORGET TO UPDATE THE README AS YOU GO!
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
    "writexl_1.4.0"
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
}

############################################
# READ IN DATA

# read in model parameters
param <- pivot_wider(read_xlsx("parameters.xlsx", sheet = "parameters"),names_from = parameter)
# read in trial data
TrialData <- read_xlsx("TrialData.xlsx", sheet = "TrialData")
# read which sites are in trail data, and read in appropriate weather data
SiteIDs <- unique(TrialData$SiteID)
Sites <- data.frame("year" = numeric(),
                    "doy" = numeric(),
                    "Tmax" = numeric(),
                    "Tmin" = numeric(),
                    "Rain" = numeric(),
                    "Solar" = numeric(),
                    "Epenman" = numeric(),
                    "Windspeed" = numeric())
file_names <- list.files()
missing_files <- character()
for(i in length(SiteIDs)){
  SiteID_i <- paste0("Site",sprintf("%03d",SiteIDs[i]),".xlsx")
  if(!(SiteID_i %in% file_names)) missing_files <- c(missing_files, SiteID_i)
}
if(length(missing_files) == 0){
  for(i in length(SiteIDs)){
    SiteID_i <- paste0("Site",sprintf("%03d",SiteIDs[i]))
    Site_i <- read_xlsx(paste0(SiteID_i,".xlsx"))
    Sites <- Sites %>%
      add_row(Site_i)
    rm(Site_i)
  }
} else {
  stop(paste0("Missing weather data files: ", missing_files, 
              "\n", "Maybe it/ they just need to be renamed?",
              "\n", "The name should be in the format Site###.xlsx"))
}

############################################
# INITIATE A SINGLE TRIAL "i"
i=1L

## START LOOP HERE!

TrialData_i <- TrialData[i,]

latitude <- TrialData_i$Latitude*2*pi/360   # Converts latitude from degrees-decimal to radians
b <- ifelse(TrialData_i$SoilB < 1, 2.1, TrialData_i$SoilB)
pop1 <- ifelse(TrialData_i$POP1 < 90000, -0.0003*(TrialData_i$POP1/1000)^2+0.0456*(TrialData_i$POP1/1000)-1.0246, 1)
pop2 <- ifelse(TrialData_i$POP2 < 90000, -0.0003*(TrialData_i$POP2/1000)^2+0.0456*(TrialData_i$POP2/1000)-1.0246, 1)
pop3 <- ifelse(TrialData_i$POP3 < 90000, -0.0003*(TrialData_i$POP3/1000)^2+0.0456*(TrialData_i$POP3/1000)-1.0246, 1)
poploss <- ifelse(TrialData_i$PlantPop == 0, 1, mean(pop1, pop2, pop3))

## Soil parameters, function of b
TrialData_i <- TrialData_i %>%
  mutate(
    qfc = param$a2 * (param$a1/5)^(1/b),
    qpwp = param$a2 * (param$a1/1500)^(1/b),
    q = qfc,                                      # ISSUE: THIS MEANS THAT QREL (NEXT LINE) ALWAYS = 1.
    qrel = (q - qpwp) / (qfc - qpwp)
  )

## Soil parameters, grouped by b
if(b > 20){
  TrialData_i <- TrialData_i %>%
    mutate(
      kappa = 0.0008,
      gamma_i = 0.00002701,
      WstressA = 5,
      WstressB = 0.8,
      WstressC = 200,
      RUEZero1 = 2.1
  )
} else {
  TrialData_i <- TrialData_i %>%
    mutate(
      kappa = 0.0027,
      gamma_i = 0.00014,
      WstressA = 2,
      WstressB = 0.6,
      WstressC = 300,
      RUEZero1 = param$RUEZero
  )
}

############################################
# TABLE OF EACH PARAMETER FOR EACH DAY OF YEAR FOR TRIAL "i".

Trial_i <- Sites %>%
  filter(year == TrialData_i$Year)

# Crop status
Trial_i <- Trial_i %>%
  mutate(
    Sown = 0,
    Sown = replace(Sown, doy >= TrialData_i$SowDOY, 1),
    Emerged = 0,
    Emerged = replace(Emerged, TrialData_i$EmDOY > TrialData_i$SowDOY & doy >= TrialData_i$EmDOY, 1), # NB: when EmDOY not given, see #Temperature cluster
    Harvested = 0,
    Harvested = replace(Harvested, doy >= TrialData_i$HarvestDOY, 1)
  )

# Temperature
Trial_i <- Trial_i %>%
  mutate(
    Tave = (Tmax + Tmin)/2, # Called Temp in OpenModel
    dT = Tave - param$Tbase,
    dT = replace(dT, dT < 0, 0)
    ) %>%
  group_by(Sown) %>%
  mutate(
    Cd_sow = cumsum(dT),
    Cd_sow = replace(Cd_sow, Sown == 0, 0),
    Emerged = replace(Emerged, TrialData_i$EmDOY <= TrialData_i$SowDOY & Cd_sow >= param$Tzero, 1)
  ) %>%
  group_by(Emerged) %>%
  mutate(
    Cd_em = cumsum(dT) + 90,
    Cd_em = replace(Cd_em, Emerged == 0, 0)
  ) %>%
  ungroup

# Soil
Trial_i <- Trial_i %>%
  mutate(
    SMD = 0)
  
# Canopy
Trial_i <- Trial_i %>%
  mutate(
    f = param$fZero,
    WStress = 1,
    WStress = replace(WStress, TrialData_i$qrel < TrialData_i$WstressB & Tave > TrialData_i$WstressC , 1),
    T_Stress = (Tave-3) * WStress, # Called DailyTemp in OpenModel
    T_Stress = replace(T_Stress, Tave < 3 | Tave > 25 | Emerged == 1, 0)
    )


## END LOOP HERE

