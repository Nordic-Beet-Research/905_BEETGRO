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
    "lubridate_1.8.0",
    "RPostgreSQL_0.7-3",
    "RPostgres_1.4.1"
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
param <- pivot_wider(read_xlsx("parameters.xlsx", sheet = "parameters"), names_from = parameter)
# read in trial data
trial <- read_xlsx("TrialData.xlsx", sheet = "TrialData")
## CONNECT TO DATABASE

# con <- dbConnect(RPostgres::Postgres(),
#                  user="postgres", 
#                  password="easy",
#                  host="localhost", 
#                  port=5433,
#                  dbname="nbyc")
# 
# trial <- dbGetQuery(con, "SELECT * FROM trials")

#dbDisconnect(con)
#rm(con)

############################################
# INITIATE A SINGLE TRIAL "i" (TRIAL = SITE x YEAR)

## START LOOP HERE!

i=1L
trial_i <- trial[i,]

# read in weather data FOR DEVELOPMENT. THIS CURRENTLY ASSUMES WEATHER DATA BY SITE X YEAR, WITH EVAPOTRANSPIRATION SORTED.
weath <- read_xlsx(paste0("./weather/Site",sprintf("%03d", trial_i$site),"_",trial_i$year,".xlsx"), sheet = "Sheet1")

soil_b <- ifelse(trial_i$soil_b < 1, 2.1, trial_i$soil_b)
pop1 <- ifelse(trial_i$pop1 < 90000, -0.0003*(trial_i$pop1/1000)^2+0.0456*(trial_i$pop1/1000)-1.0246, 1)
pop2 <- ifelse(trial_i$pop2 < 90000, -0.0003*(trial_i$pop2/1000)^2+0.0456*(trial_i$pop2/1000)-1.0246, 1)
pop3 <- ifelse(trial_i$pop3 < 90000, -0.0003*(trial_i$pop3/1000)^2+0.0456*(trial_i$pop3/1000)-1.0246, 1)
pop_loss <- ifelse(trial_i$plant_pop == 0, 1, mean(pop1, pop2, pop3))

## Soil parameters, function of soil_b
soil_qfc <- param$a2 * (param$a1/5)^(1/soil_b)
soil_qpwp <- param$a2 * (param$a1/1500)^(1/soil_b)

## Soil parameters, grouped by soil_b
if(soil_b > 20){
  trial_i <- trial_i %>%
    mutate(
      kappa = 0.0008,
      gamma_i = 0.00002701,
      stress_wA = 5,
      stress_wB = 0.8,
      stress_wC = 200,
      rue_zero1 = 2.1
  )
} else {
  trial_i <- trial_i %>%
    mutate(
      kappa = 0.0027,
      gamma_i = 0.00014,
      stress_wA = 2,
      stress_wB = 0.6,
      stress_wC = 300,
      rue_zero1 = param$rue_zero
  )
}

############################################
# TABLE OF EACH PARAMETER FOR EACH DAY OF YEAR FOR TRIAL "i".

date_sow <- date(as.POSIXct(unlist(trial_i["date_sow"]), origin = '1970-01-01'))
date_emerg <- date(as.POSIXct(unlist(trial[i,"date_emerg"]), origin = '1970-01-01'))
date_harvest <- date(as.POSIXct(unlist(trial[i,"date_harvest"]), origin = '1970-01-01'))
doy_sow <-yday(date_sow)
doy_emerg <-yday(date_emerg)
doy_harvest <-yday(date_harvest)

BG_i <- weath %>%
  select(doy, temp_x, precip, radiation_solar, evap
         ) %>%

# Crop status
  mutate(bbch = 00,
         bbch = replace(bbch, doy >= doy_sow, 01),
         bbch = replace(bbch, doy >= doy_emerg, 09),
         bbch = replace(bbch, doy >= doy_harvest, 99)
         ) %>% 

# Temperature
  mutate(
    #temp_x = (Tmax + Tmin)/2, # Called Temp in OpenModel
    temp_b_d = temp_x - param$Tbase,
    temp_b_d = replace(temp_b_d, temp_b_d < 0, 0)
    ) %>%
  group_by(sown = bbch >= 01) %>%
  mutate(
    temp_b_cd_sow = cumsum(temp_b_d),
    temp_b_cd_sow = replace(temp_b_cd_sow, bbch == 00, 0),
    bbch = replace(bbch, doy_emerg <= doy_sow & temp_b_cd_sow >= param$Tzero, 01)
  ) %>%
  group_by(emerged = bbch >= 09) %>%
  mutate(
    temp_b_cd_em = cumsum(temp_b_d) + 90,
    temp_b_cd_em = replace(temp_b_cd_em, bbch < 09, 0)
  ) %>%
  ungroup

# GOING ROW WISE(). BUT THE PROBLEM IS THAT YOU CAN'T LAG IN DPLYR::ROWWISE(), SO HAVE TO GO TO LOOP
for(j in 1:(doy_sow-1)){
  BG_ij <- BG_i[j,] %>%
    mutate(soil_md = 20,
           soil_md_0 = 20,
           soil_qrel = 1,
           canopy_cover = param$fZero,
           temp_s_cd = 0,
           evap_soil = 0,
           yield_biom = 0,
           yield_root = 0)
  
  if(j==1) BG_j <- BG_ij
  if(j!=1) BG_j <- bind_rows(BG_j, BG_ij)
}
for(j in doy_sow:nrow(BG_i)){
  BG_ij <- BG_i[j,] %>%
    mutate(
      # VALUES WITH LAGS, AND REMOVING NEGATIVES.
      soil_qrel = BG_j$soil_qrel[j-1],
      soil_md = BG_j$soil_md[j-1], 
      soil_md_0 = case_when(doy == doy_sow ~ 0, soil_md < 0 ~ 0, T ~ soil_md), # This is soil_md from previous period, with min = 0
      evap_soil = BG_j$evap_soil[j-1],
      yield_biom = BG_j$yield_biom[j-1],
      radiation_solar = replace(radiation_solar, radiation_solar < 0, 0),
      yield_root = BG_j$yield_root[j-1],
      
      # CANOPY (canopy_cover)
      stress_water = ifelse(soil_qrel < trial_i$stress_wB & temp_b_cd_em > trial_i$stress_wC, (soil_qrel/trial_i$stress_wB)^trial_i$stress_wC, 1),
      temp_s_d = ifelse(temp_b_d > 0 & temp_b_d < 22 & bbch >= 09, temp_b_d * stress_water, 0), # temp_x Called Weather.DailyTemp in OpenModel, temp_s_d called DailyTemp in OpenModel
      temp_s_cd = unlist(BG_j[j-1,"temp_s_cd"]) + temp_s_d, # temp_s_cd called TotalTemp in OpenModel
      temp_s_cd = ifelse(doy == doy_emerg, 90 + temp_s_d, temp_s_cd),
      temp_f = temp_s_cd * 0.001,
      temp_f = replace(temp_f, temp_f < 0.00001, 0.00001),
      temp_s_cd = replace(temp_s_cd, temp_s_cd > 950, 950),
      canopy_cover = 0.0015 + (0.99-0.0015)/(1+exp(-4*log(temp_f/(1-temp_f)))),

      # ROOT DEPTH (is sowing depth until emergence.)
      root_depth = param$Dsowing + param$length0*exp(param$beta0/param$delta*(1-exp(-param$delta*(temp_b_cd_em-param$Tzero)))),
      root_depth = replace(root_depth, bbch < 09, param$Dsowing),

      # WATER RELATIONS
      # SSE SOIL SURFACE EVAPORATION
      evap_soil_d = min(1.5, evap)*(1-canopy_cover), # this did have if canopy_covere <1, but it always is.
      evap_soil_d = replace(evap_soil_d, evap_soil > 20, 0),
      evap_soil = evap_soil + evap_soil_d - precip,
      evap_soil = replace(evap_soil, evap_soil < 0, 0),
      # ATMOSPHERE LIMITED CROP TRANSPIRATION
      evap_cal = 1.2*canopy_cover*evap,
      evap_cal = replace(evap_cal, evap_cal <= 0, 0.001),
      # PARAMETERS
      soil_q = max(soil_qfc - soil_md_0/(root_depth*1000), 0.01), # depth is always greater than 0 post sowing, so if statement for soil_q removed.
      soil_qrel = soil_q/soil_qfc,
      psi_soil = -5*(soil_qrel)^(-soil_b),
      rsum = param$c1 + param$c2/root_depth*(soil_qrel^(-1*(2*soil_b+3))-1),
      soil_hc = soil_qrel^(2*soil_b+3),
      # SOIL LIMITED MAXIMUM CROP TRANSPIRATION
      evap_csl = (psi_soil - param$psiCrop)/rsum,
      rsum = replace(rsum, soil_qrel == 0, param$c1),
      evap_csl = replace(evap_csl, soil_qrel == 0, 0),
      # ACTUAL CROP EVAPOTRANSPIRATION
      evap_actual = min(evap_cal, evap_csl),
      # SOIL MOISTURE DEFICIT NB: IF THIS GIVE A NEGATIVE RESULT, 0 IS USED WHEN SMD IS USED TO CALC soil_q IN THE NEXT DAY.
      soil_md_d = evap_soil_d + evap_actual - precip,
      soil_md = soil_md_0 + soil_md_d,
      
      
      # RADIATION USE EFFICIENCY (rue)
      rue = 0.6*trial_i$rue_zero1*exp(-trial_i$gamma_i*yield_biom)+0.4*trial_i$rue_zero1*(evap_actual/evap_cal)*exp(-trial_i$gamma_i*yield_biom),
      
      # YIELDS
      yield_biom_d = rue*canopy_cover*radiation_solar, #There is a calculation for radiation in OpenModel, if radiation_solar is negative.
      yield_biom = yield_biom + yield_biom_d,
      yield_root = (yield_root + yield_biom_d*(trial_i$kappa*yield_biom/(1+trial_i$kappa*yield_biom))),
      yield_sugar = (trial_i$kappa*yield_biom/(1+trial_i$kappa*yield_biom)),
      yield_pop = pop_loss*yield_root
      )
  
  BG_j <- bind_rows(BG_j, BG_ij)
}

BG_i_names <- names(BG_i)
BG_i_names <- BG_i_names[-1]
BG_j <- BG_j[,-which(names(BG_j) %in% BG_i_names)]

BG_i <- BG_i %>% 
  left_join(BG_j, by ="doy")

BG_i$trial_id <- trial_i$id         # Add trial_id back in to use for presentation of data.
BG_i$id <- trial_i$id*1000+BG_i$doy # Add id that is unique forever to allow data to go to SQL. 
                                    ## THIS MIGHT CAUSE ISSUES IF TRY TO SEND THE SAME TRIAL TO SQL AGAIN! 
                                    ## MIGHT NEED TO SYSTEM TO CHECK, THROW WARNING, THEN DELETE OLD DATA FROM SQL IF DESIRED

# SAVE OUTPUT FILE BY TRIAL (SITE x YEAR)
write_xlsx(BG_i, paste0("./results/BG_Site",sprintf("%03d", trial_i$site),"_",trial_i$year,".xlsx"))

# SAVE OUTPUT TO SELECTED DATASETS FOR DISPLAY
# dbWriteTable(con, "results", BG_i, append=T)
# dbDisconnect(con)
# rm(con)

## END LOOP HERE

ggplot(BG_i, aes(x = doy, y = soil_md)) +
  geom_line() +
  geom_vline(xintercept = doy_emerg) +
  geom_vline(xintercept = doy_sow) + 
  geom_vline(xintercept = doy_harvest) +
  geom_vline(xintercept = 150) + 
  geom_vline(xintercept = 160) + 
  geom_vline(xintercept = 210)
