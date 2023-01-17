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
# READ IN DATA

# read in model parameters
param <- pivot_wider(read_xlsx("parameters.xlsx", sheet = "parameters"),names_from = parameter)
# read in trial data
dat_in_trial <- read_xlsx("TrialData.xlsx", sheet = "TrialData")
# read in weather data
dat_in_weath <- read_xlsx("dat_weather_2021_40141.xlsx", sheet = "Sheet1")
# read which sites are in trail data, and read in appropriate weather data
SiteIDs <- unique(dat_in_trial$SiteID)
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
i=7L

## START LOOP HERE!

dat_in_trial_i <- dat_in_trial[i,]

b <- ifelse(dat_in_trial_i$SoilB < 1, 2.1, dat_in_trial_i$SoilB)
pop1 <- ifelse(dat_in_trial_i$POP1 < 90000, -0.0003*(dat_in_trial_i$POP1/1000)^2+0.0456*(dat_in_trial_i$POP1/1000)-1.0246, 1)
pop2 <- ifelse(dat_in_trial_i$POP2 < 90000, -0.0003*(dat_in_trial_i$POP2/1000)^2+0.0456*(dat_in_trial_i$POP2/1000)-1.0246, 1)
pop3 <- ifelse(dat_in_trial_i$POP3 < 90000, -0.0003*(dat_in_trial_i$POP3/1000)^2+0.0456*(dat_in_trial_i$POP3/1000)-1.0246, 1)
poploss <- ifelse(dat_in_trial_i$PlantPop == 0, 1, mean(pop1, pop2, pop3))

## Soil parameters, function of b
qfc <- param$a2 * (param$a1/5)^(1/b)
qpwp <- param$a2 * (param$a1/1500)^(1/b)

dat_in_trial_i <- dat_in_trial_i %>%
  mutate(
    qfc = param$a2 * (param$a1/5)^(1/b),
    qpwp = param$a2 * (param$a1/1500)^(1/b),
    q = qfc,                                      # ISSUE: THIS MEANS THAT QREL (NEXT LINE) ALWAYS = 1.
    
  )

## Soil parameters, grouped by b
if(b > 20){
  dat_in_trial_i <- dat_in_trial_i %>%
    mutate(
      kappa = 0.0008,
      gamma_i = 0.00002701,
      WstressA = 5,
      WstressB = 0.8,
      WstressC = 200,
      RUEZero1 = 2.1
  )
} else {
  dat_in_trial_i <- dat_in_trial_i %>%
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

sow_date <- date(as.POSIXct(unlist(dat_in_trial_i["SowDate"]), origin = '1970-01-01'))
emerg_date <- date(as.POSIXct(unlist(dat_in_trial[i,"EmDate"]), origin = '1970-01-01'))
harvest_date <- date(as.POSIXct(unlist(dat_in_trial[i,"HarvestDate"]), origin = '1970-01-01'))
sow_doy <-yday(sow_date)
emerg_doy <-yday(emerg_date)
harvest_doy <-yday(harvest_date)

can_i <- dat_in_weath %>%
  select(doy, xtemp, neda, et
         ) %>%

# Crop status
  mutate(bbch = 00,
         bbch = replace(bbch, doy >= sow_doy, 01),
         bbch = replace(bbch, doy >= emerg_doy, 09),
         bbch = replace(bbch, doy >= harvest_doy, 99)
         ) %>% 

# Temperature
  mutate(
    #xtemp = (Tmax + Tmin)/2, # Called Temp in OpenModel
    dT = xtemp - param$Tbase,
    dT = replace(dT, dT < 0, 0)
    ) %>%
  group_by(sown = bbch >= 01) %>%
  mutate(
    Cd_sow = cumsum(dT),
    Cd_sow = replace(Cd_sow, bbch == 00, 0),
    bbch = replace(bbch, dat_in_trial_i$EmDOY <= dat_in_trial_i$SowDOY & Cd_sow >= param$Tzero, 01)
  ) %>%
  group_by(emerged = bbch >= 09) %>%
  mutate(
    Cd_em = cumsum(dT) + 90,
    Cd_em = replace(Cd_em, bbch < 09, 0)
  ) %>%
  ungroup

# GOING ROW WISE(). BUT THE PROBLEM IS THAT YOU CAN'T LAG IN ROWWISE(), SO HAVE TO GO TO LOOP
for(j in 1:nrow(can_i)){
  can_ij <- can_i[j,] %>%
    mutate(
      # INITIAL CONDITIONS
      
      # CANOPY
      f = param$fZero,
      WStress = 1,
      WStress = replace(WStress, qrel < dat_in_trial_i$WstressB & xtemp > dat_in_trial_i$WstressC, 1),
      T_Stress = (xtemp-3) * WStress, # Called DailyTemp in OpenModel
      T_Stress = replace(T_Stress, xtemp < 3 | xtemp > 25 | bbch < 09, 0),
      TotalTemp = cumsum(T_Stress),
      TotalTemp = replace(TotalTemp, TotalTemp > 990, 990),
      fTemp = TotalTemp * 0.001,
      fTemp = replace(fTemp, fTemp < 0.00001, 0.00001),
      f = 0.0015 + (0.99-0.0015)/(1+exp(-4*log((fTemp/(1-fTemp))))),
      
      # ROOT DEPTH
      root_depth = param$Dsowing + param$length0*exp(param$beta0/param$delta*(1-exp(-param$delta*(Cd_em-param$Tzero)))),
      root_depth = replace(root_depth, bbch < 09, param$Dsowing),
      
      # WATER RELATIONS
      smd = 20,
      smd = replace(smd, doy == sow_doy, 0),
      smd = replace(smd, smd < 0, 0),
      dSSE = ifelse(f < 1, min(1.5, et)*(1-f), 0),
      eatmos = 1.2*f*et,
      eatmos = replace(eatmos, eatmos <= 0, 0.001),
      q = max(qfc - smd/(root_depth*1000), 0.01),
      qrel = q/qfc,
      psiSoil = -5*(qrel)^(-b),
      rsum = param$c1 + param$c2/root_depth*((qrel)^(-(2*b+3))-1)
      )
  
  if(j==1) can_j <- can_ij
  if(j!=1) can_j <- bind_rows(can_j, can_ij)
  }

can_i_names <- names(can_i)
can_i_names <- can_i_names[-1]
can_j <- can_j[,-which(names(can_j) %in% can_i_names)]

can_i <- can_i %>% 
  left_join(can_j, by ="doy")

# # Canopy, with adjustments from user input (Pretty sure this isn't working properly in OpenModel)
# if(dat_in_trial_i$Canopy1 == 1 | dat_in_trial_i$Can1aCOV > 0){
#   Trial_i <- Trial_i %>%
#     mutate(
#       f1 = param$fZero,
#       TotalTemp = cumsum(T_Stress),
#       TotalTemp = replace(TotalTemp, TotalTemp > 990, 990),
#       fTemp = TotalTemp * 0.001,
#       fTemp = replace(fTemp, fTemp < 0.00001, 0.00001),
#       f = 0.0015 + (0.99-0.0015)/(1+exp(-4*log((fTemp/(1-fTemp)))))
#     )
# }

## END LOOP HERE

write_xlsx(can_i, paste0("dat_canopy_2021.xlsx"))
