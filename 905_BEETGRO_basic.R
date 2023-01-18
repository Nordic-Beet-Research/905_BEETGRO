############################################
############################################
##
## BBRO BEETGRO MODEL - TRANSPOSING OPENMODEL VERSION TO R
##
## v2023-01-18
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-01
##
## 
## Aim is to build a system that: 
## - reads in all the Trial data from a single spreadsheet. Each Trial is a unique Site x Year combination. Each Site is a single grower.
## - reads in all the Site weather data either through an API or from grower submitted Site specific spreadsheets. 
## -- Multiple years are currently housed in single site files (named Site###.xlsx). 
## -- This set-up suits most weather data systems, thus lends itself to automation. 
## -- It also means that with careful management of weather data files, .csv files for each site can be saved locally.
## - fills in any blanks in the data where possible.
## - runs through the analysis for each Trial and stores the results as appropriate.
## - makes the results viewable via one or two dashboards.
##
## Current status:
## - Weather data is run through the 905_BEETGRO_weather script. Can import from Lantmet. Saves as Site###_year.xlsx
## - The soil moisture, canopy, and yield model seems to work at the Trial level. 
## -- It's not yet set up to run through all the Trials in the TrialData.xlsx sheet, but not far from that.
## -- Have not confirmed the results with a comparison to the OpenModel version
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
trial <- read_xlsx("TrialData.xlsx", sheet = "TrialData")

############################################
# INITIATE A SINGLE TRIAL "i" (TRIAL = SITE x YEAR)

## START LOOP HERE!

i=1L
trial_i <- trial[i,]

# read in weather data FOR DEVELOPMENT. SYSTEM BELOW IS FOR NORMAL OPERATION.
weath <- read_xlsx(paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),"_",trial_i$Year,".xlsx"), sheet = "Sheet1")

b <- ifelse(trial_i$SoilB < 1, 2.1, trial_i$SoilB)
pop1 <- ifelse(trial_i$POP1 < 90000, -0.0003*(trial_i$POP1/1000)^2+0.0456*(trial_i$POP1/1000)-1.0246, 1)
pop2 <- ifelse(trial_i$POP2 < 90000, -0.0003*(trial_i$POP2/1000)^2+0.0456*(trial_i$POP2/1000)-1.0246, 1)
pop3 <- ifelse(trial_i$POP3 < 90000, -0.0003*(trial_i$POP3/1000)^2+0.0456*(trial_i$POP3/1000)-1.0246, 1)
pop_loss <- ifelse(trial_i$PlantPop == 0, 1, mean(pop1, pop2, pop3))

## Soil parameters, function of b
qfc <- param$a2 * (param$a1/5)^(1/b)
qpwp <- param$a2 * (param$a1/1500)^(1/b)

## Soil parameters, grouped by b
if(b > 20){
  trial_i <- trial_i %>%
    mutate(
      kappa = 0.0008,
      gamma_i = 0.00002701,
      w_stressA = 5,
      w_stressB = 0.8,
      w_stressC = 200,
      RUEZero1 = 2.1
  )
} else {
  trial_i <- trial_i %>%
    mutate(
      kappa = 0.0027,
      gamma_i = 0.00014,
      w_stressA = 2,
      w_stressB = 0.6,
      w_stressC = 300,
      RUEZero1 = param$RUEZero
  )
}

############################################
# TABLE OF EACH PARAMETER FOR EACH DAY OF YEAR FOR TRIAL "i".

sow_date <- date(as.POSIXct(unlist(trial_i["SowDate"]), origin = '1970-01-01'))
emerg_date <- date(as.POSIXct(unlist(trial[i,"EmDate"]), origin = '1970-01-01'))
harvest_date <- date(as.POSIXct(unlist(trial[i,"HarvestDate"]), origin = '1970-01-01'))
sow_doy <-yday(sow_date)
emerg_doy <-yday(emerg_date)
harvest_doy <-yday(harvest_date)

BG_i <- weath %>%
  select(doy, xtemp, precip, solin, et
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
    bbch = replace(bbch, emerg_doy <= sow_doy & Cd_sow >= param$Tzero, 01)
  ) %>%
  group_by(emerged = bbch >= 09) %>%
  mutate(
    Cd_em = cumsum(dT) + 90,
    Cd_em = replace(Cd_em, bbch < 09, 0)
  ) %>%
  ungroup

# GOING ROW WISE(). BUT THE PROBLEM IS THAT YOU CAN'T LAG IN ROWWISE(), SO HAVE TO GO TO LOOP
for(j in 1:(sow_doy-1)){
  BG_ij <- BG_i[j,] %>%
    mutate(SMD = 20,
           SMD0 = 20,
           qrel = 1,
           f = param$fZero,
           Cd_stress = 0,
           SSE = 0,
           biomass = 0,
           yield = 0)
  
  if(j==1) BG_j <- BG_ij
  if(j!=1) BG_j <- bind_rows(BG_j, BG_ij)
}
for(j in sow_doy:nrow(BG_i)){
  BG_ij <- BG_i[j,] %>%
    mutate(
      # VALUES WITH LAGS, AND REMOVING NEGATIVES
      qrel = BG_j$qrel[j-1],
      SMD = BG_j$SMD[j-1], 
      SMD0 = case_when(doy == sow_doy ~ 0, SMD < 0 ~ 0, T ~ SMD), # This is SMD from previous period, with min = 0
      SSE = BG_j$SSE[j-1],
      biomass = BG_j$biomass[j-1],
      solin = replace(solin, solin < 0, 0),
      yield = BG_j$yield[j-1],
      
      # CANOPY (f)
      w_stress = ifelse(qrel < trial_i$w_stressB & xtemp > trial_i$w_stressC, (qrel/trial_i$w_stressB)*trial_i$Wstresc, 1),
      xt_stress = ifelse(xtemp > 3 & xtemp < 25 & bbch >= 09, (xtemp-3) * w_stress, 0), # xtemp Called Weather.DailyTemp in OpenModel, xt_stress called DailyTemp in OpenModel
      Cd_stress = unlist(BG_j[j-1,"Cd_stress"]) + xt_stress, # Cd_stress called TotalTemp in OpenModel
      fTemp = Cd_stress * 0.001,
      fTemp = replace(fTemp, fTemp < 0.00001, 0.00001),
      Cd_stress = replace(Cd_stress, Cd_stress > 950, 950),
      f = 0.0015 + (0.99-0.0015)/(1+exp(-4*log(fTemp/(1-fTemp)))),

      # ROOT DEPTH (is sowing depth until emergence.)
      root_depth = param$Dsowing + param$length0*exp(param$beta0/param$delta*(1-exp(-param$delta*(Cd_em-param$Tzero)))),
      root_depth = replace(root_depth, bbch < 09, param$Dsowing),

      # WATER RELATIONS
      # SSE SOIL SURFACE EVAPORATION
      dSSE = ifelse(f < 1, min(1.5, et)*(1-f), 0),
      dSSE = replace(dSSE, SSE > 20, 0),
      SSE = SSE + dSSE - precip,
      SSE = replace(SSE, SSE < 0, 0),
      # ATMOSPHERE LIMITED CROP TRANSPIRATION
      eatmos = 1.2*f*et,
      eatmos = replace(eatmos, eatmos <= 0, 0.001),
      # PARAMETERS
      q = max(qfc - SMD0/(root_depth*1000), 0.01), # depth is always greater than 0 post sowing, so if statement for q removed.
      qrel = q/qfc,
      psi_soil = -5*(qrel)^(-b),
      rsum = param$c1 + param$c2/root_depth*(qrel^(-2*b+3)-1),
      hc_soil = qrel^(2*b+3),
      # SOIL LIMITED MAXIMUM CROP TRANSPIRATION
      esoil = (psi_soil - param$psiCrop)/rsum,
      rsum = replace(rsum, qrel == 0, param$c1),
      esoil = replace(esoil, qrel == 0, 0),
      # ACTUAL CROP EVAPOTRANSPIRATION
      ea = min(eatmos, esoil),
      # SOIL MOISTURE DEFICIT NB: IF THIS GIVE A NEGATIVE RESULT, 0 IS USED WHEN SMD IS USED TO CALC q IN THE NEXT DAY.
      dSMD = dSSE + ea - precip,
      SMD = SMD0 + dSMD,
      
      
      # RADIATION USE EFFICIENCY (RUE)
      RUE = 0.6*trial_i$RUEZero1*exp(-trial_i$gamma_i*biomass)+0.4*trial_i$RUEZero1*(ea/eatmos)*exp(-trial_i$gamma_i*biomass),
      
      # YIELDS
      dbiomass = RUE*f*solin, #There is a calculation for radiation in OpenModel, if solin is negative.
      biomass = biomass + dbiomass,
      yield = (yield + dbiomass*(trial_i$kappa*biomass/(1+trial_i$kappa*biomass))),
      fsugar = (trial_i$kappa*biomass/(1+trial_i$kappa*biomass)),
      yield_pop = pop_loss*yield
      )
  
  BG_j <- bind_rows(BG_j, BG_ij)
}

BG_i_names <- names(BG_i)
BG_i_names <- BG_i_names[-1]
BG_j <- BG_j[,-which(names(BG_j) %in% BG_i_names)]

BG_i <- BG_i %>% 
  left_join(BG_j, by ="doy")

# SAVE OUTPUT FILE BY TRIAL (SITE x YEAR)
write_xlsx(BG_i, paste0("./results/BG_Site",sprintf("%03d", trial_i$SiteID),"_",trial_i$Year,".xlsx"))

## END LOOP HERE



