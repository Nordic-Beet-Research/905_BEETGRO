############################################
############################################
##
## Building an NBR Beet Growth Model
##
## v1.0 - Getting ET, putting that in the BBRO model, and getting through to Vintermötet2022
##
## This project uses R 4.1.2 
## with snapshot date 2021-11-01
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
    "data.table_1.14.2",
    "ggplot2_3.3.5",
    "dplyr_1.0.7",
    "writexl_1.4.0",
    "lubridate_1.8.0",
    "readxl_1.3.1"
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
# IMPORT TRIALDATA AND EXTRACT WEATHER DATA SOURCES
## Read in trial data
trial <- read_xlsx("TrialData.xlsx", sheet = "TrialData")
param <- pivot_wider(read_xlsx("parameters.xlsx", sheet = "parameters"), names_from = parameter)

i=1L

for(i in 1:nrow(trial)){
  trial_i <- trial[i,]
  source_i <- trial_i$weather_source
  if(source_i == 1) source_id_i <- trial_i$weather_source_info # 1 == Lantmet, 2 == File
  if(source_i == 2) source_id_i <- paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),".xlsx")


  ############################################
  # IMPORT DATA FROM LANTMET https://www.ffe.slu.se/lm/JSON/JSON-Specifikation.pdf
  if(source_i == 1){
    ### create url values
    urlBase <- "https://www.ffe.slu.se/lm/json/downloadJS.cfm?outputType=CSV&AddID=1"
    startDate <-  date(ISOdatetime(year = as.integer(trial_i$year,'%Y'),
                                   month = as.integer("01",'%m'),
                                   day = as.integer("01",'%d'),
                                   hour = 00, min = 00, sec = 00, tz = "UTC"))
    endDate <- date(ISOdatetime(year = as.integer(trial_i$year,'%Y'),
                                month = as.integer("12",'%m'),
                                day = as.integer("31",'%d'),
                                hour = 00, min = 00, sec = 00, tz = "UTC"))
    logInterval <- 2                                # 1= hourly, 2 = daily
    elementMeasurement <- "TM,TN,TX,RR,FM2,Q0,UM,UN,UX"
    names_old <- c("DAY","TM","TN","TX","RR","FM2","Q0","UM","UN","UX")
    names_new <- c("date","temp_x","temp_l","temp_h","precip","vh_x","radiation_solar","hum_x","hum_l","hum_h")
    
    ### create url
    urlStation <- paste0("weatherStationID=",source_id_i)
    urlStart <- paste0("startDate=",startDate)
    urlEnd <- paste0("endDate=",endDate)
    urlLog <- paste0("LogIntervalID=",logInterval)
    urlEle <- paste0("elementMeasurementTypeList=",elementMeasurement)
    url <- paste(urlBase,urlStation,urlStart,urlEnd,urlLog,urlEle,sep="&")
    
    # Import from Lantmet.
    dat_Lantmet <- data.frame(fread(url))
    
    # Fixing parameters
    dat_weather_i <- dat_Lantmet %>%
      select(!c("HOUR","WSTNID")) %>% 
      rename_at(vars(all_of(names_old)), ~ names_new) %>% 
      mutate(doy = yday(date),
             vh_x = replace(vh_x, vh_x == "NA", 2))
    
    wind_height <- 2 # height above ground at which wind is measured
    Albedo <- param$Albedo
    SBconstant <- param$SBconstant
    mamsl <- trial_i$elevation
    lat <- trial_i$latitude
  }
  
  ############################################
  # CREATE DATAFRAME THAT WILL ALLOW FOR FORECASTING AND FOR MISSING WEATHER DATA TO BE FILLED
  dat_weather_i_full <- data.frame(doy = 1:365)
  
  ## FORECAST METHOD
  forecast_method <- 1 # 1 = use historical averages from region. 2 = use specific year and station
  ### BUILD THIS OUT LIKE WITH THE HARVEST AND STORAGE MODEL
  #### TREAT THIS LIKE MISSING DATA, AND SO JUST FILL OUT IN THE MISSING DATA PROCESS BELOW.
  #### ALSO PROVIDE THE OPTION THAT 
  
  ## MISSING DATA
  dat_weather_i_missing <- dat_weather_i[rowSums(is.na(dat_weather_i)) > 0,]
  ### BUILD THIS OUT USING THE HISTORICAL DATA SETS
  #### WILL USE HISTORICAL DATA TO FILL THIS OUT. THE SYSTEM NEEDS TO BE ABLE TO LOOK AT EACH CELL INDIVIDUALLY.
  #### THERE ARE MODELS AVAILABLE FROM THE BeetGRO MODEL FOR SOME OF THESE PARAMETERS, BUT I'M JUST GOING TO USE HISTORICAL DATA INSTEAD.
  #### THE USER WILL HAVE TO CHOOSE FROM A DROP-DOWN LIST WHICH HISTORICAL DATA THEY WANT TO USE TO FILL MISSING.
  
  dat_weather_i_full <- dat_weather_i_full %>% 
    left_join(dat_weather_i, by = "doy")
  
  ############################################
  # CALCULATE ET, AND THE STEPS ALONG THE WAY
  
  dat_weather_i_full <- dat_weather_i_full %>% 
    mutate(
      # WIND SPEED AT GROUND LEVEL
      wind_ground = vh_x * 4.87/log(67.8*wind_height-5.42),
          
      # SLOPE OF THE SATURATION PRESSURE CURVE
      press_slope = 4098*(0.6108*exp(17.27*temp_x/(temp_x+237.3)))/(temp_x+237.3)^2,
  
      # PRESSURE
      press = 101.3*((293-0.0065*mamsl)/293)^5.26,
  
      # PSYKROMETRIC CONSTANT
      psychroC = 0.000665*press,
  
      # DELTA
      delta = press_slope/(press_slope+psychroC*(1+0.34*vh_x)),
  
      # PSI
      psi = psychroC/(press_slope+psychroC*(1+0.34*vh_x)),
      
      # TEMP TERM
      temp_t = 900*vh_x/(temp_x+273.15),
      
      # SATURATION PRESSURE MAX
      press_sat_h = 0.6108*exp(17.27*temp_h/(temp_h+237.3)),
      
      # SATURATION PRESSURE MIN
      press_sat_l = 0.6108*exp(17.27*temp_l/(temp_l+237.3)),
      
      # VAPOUR PRESSURE
      press_vapour = (press_sat_h*hum_h/100 + press_sat_l*hum_l/100)/2,
      
      # INVERTED SUN DISTANCE
      sun_dist = 1 + 0.033*cos(2*pi/365*doy),
      
      # SUN DECLINATION
      sun_decl = 0.409*sin(2*pi/365*doy - 1.39),
      
      # SUN ANGLE
      sun_angle = acos(-tan(lat*pi/180)*tan(sun_decl)),
      
      # SPACE RADIATION
      radiation_space = 24*60/pi*0.082*sun_dist*(sun_angle*sin(lat*pi/180)*sin(sun_decl)+cos(lat*pi/180)*cos(sun_decl)*sin(sun_angle)),
      
      # GLOBAL RADIATION
      radiation_global = (0.75+2*10^(-5)*mamsl)*radiation_space,
      
      # NET RADIATION
      radiation_net = (1-Albedo)*radiation_solar,
      
      # LONG-WAVE RADIATION
      radiation_LW = SBconstant*10^(-9)*((temp_l+273.16)^4+(temp_h+273.16)^4)/2*(0.34-0.14*press_vapour^0.5)*(1.35*radiation_solar/radiation_global-0.35),
      
      # RADIATION BALANCE
      radiation_balance = 0.408*(radiation_net - radiation_LW),
      
      # EVAPOTRANSPIRATION
      evap = delta * radiation_balance + psi * temp_t * ((press_sat_h + press_sat_l)/2 - press_vapour)
    )

  # CREATE FILL WITH JUST THE DATA NEEDED
  dat_weather_i_abbrev <- dat_weather_i_full %>% 
    select(doy,temp_x,precip,radiation_solar,vh_x,evap)
  
  ############################################
  # WRITE EXCEL
  
  write_xlsx(dat_weather_i_abbrev, paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),"_",trial_i$Year,".xlsx"))
  write_xlsx(dat_weather_i_full, paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),"_",trial_i$Year,"_full.xlsx"))

}


# FOR IF READING IN A FILE, BUT NEEDS TO BE LOOKED AT!
# read which sites are in trail data, and read in appropriate weather data
SiteIDs <- unique(trial$SiteID)
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