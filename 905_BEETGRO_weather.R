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
# IMPORT TRIALDATA AND EXTRACT WEATHER DATA SOURCES
## Read in trial data
trial <- read_xlsx("TrialData.xlsx", sheet = "TrialData")

i=1L

trial_i <- trial[i,]

for(i in 1:nrow(trial)){
  source_i <- trial_i$Weather_source
  if(source_i == "Lantmet") source_id_i <- trial_i$Lantmet
  if(source_i == "File") source_id_i <- paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),".xlsx")


  ############################################
  # IMPORT DATA FROM LANTMET https://www.ffe.slu.se/lm/JSON/JSON-Specifikation.pdf
  if(source_i == "Lantmet"){
    ### create url values
    urlBase <- "https://www.ffe.slu.se/lm/json/downloadJS.cfm?outputType=CSV&AddID=1"
    startDate <-  date(ISOdatetime(year = as.integer(trial_i$Year,'%Y'),
                                   month = as.integer("01",'%m'),
                                   day = as.integer("01",'%d'),
                                   hour = 00, min = 00, sec = 00, tz = "UTC"))
    endDate <- date(ISOdatetime(year = as.integer(trial_i$Year,'%Y'),
                                month = as.integer("12",'%m'),
                                day = as.integer("31",'%d'),
                                hour = 00, min = 00, sec = 00, tz = "UTC"))
    logInterval <- 2                                # 1= hourly, 2 = daily
    elementMeasurement <- "TM,TN,TX,RR,FM2,Q0,UM,UN,UX"
    names_old <- c("DAY","TM","TN","TX","RR","FM2","Q0","UM","UN","UX")
    names_new <- c("date","xtemp","ltemp","htemp","precip","xvh","solin","xhum","lhum","hhum")
    
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
      rename_at(vars(names_old), ~ names_new) %>% 
      mutate(doy = yday(date),
             xvh = replace(xvh, xvh == "NA", 2))
    
    wind_height <- 2
    mamsl <- trial_i$Elevation
    lat <- trial_i$Latitude
  }
  
  ############################################
  # CALCULATE ET, AND THE STEPS ALONG THE WAY
  
  dat_weather_i_full <- dat_weather_i %>% 
    mutate(
      # WIND SPEED AT GROUND LEVEL
      wind_ground = xvh * 4.87/log(67.8*wind_height-5.42),
          
      # SLOPE OF THE SATURATION PRESSURE CURVE
      slopesat = 4098*(0.6108*exp(17.27*xtemp/(xtemp+237.3)))/(xtemp+237.3)^2,
  
      # PRESSURE
      press = 101.3*((293-0.0065*mamsl)/293)^5.26,
  
      # PSYKROMETRIC CONSTANT
      psychroC = 0.000665*press,
  
      # DELTA
      delta = slopesat/(slopesat+psychroC*(1+0.34*xvh)),
  
      # PSI
      psi = psychroC/(slopesat+psychroC*(1+0.34*xvh)),
      
      # TEMP TERM
      tempt = 900*xvh/(xtemp+273),
      
      # SATURATION PRESSURE MAX
      satPress_max = 0.6108*exp(17.27*htemp/(htemp+237.3)),
      
      # SATURATION PRESSURE MIN
      satPress_min = 0.6108*exp(17.27*ltemp/(ltemp+237.3)),
      
      # VAPOUR PRESSURE
      vapourPress = (satPress_max*hhum/100 + satPress_min*lhum/100)/2,
      
      # INVERTED SUN DISTANCE
      invSun = 1 + 0.033*cos(2*pi/365*doy),
      
      # SUN DECLINATION
      sunDecl = 0.409*sin(2*pi/365*doy - 1.39),
      
      # SUN ANGLE
      sunAngle = acos(-tan(lat*pi/180)*tan(sunDecl)),
      
      # SPACE RADIATION
      spaceRadiation = 24*60/pi*0.082*invSun*(sunAngle*sin(lat*pi/180)*sin(sunDecl)+cos(lat*pi/180)*cos(sunDecl)*sin(sunAngle)),
      
      # GLOBAL RADIATION
      globalRadiation = (0.75+2*10^(-5)*mamsl)*spaceRadiation,
      
      # NET RADIATION
      netRadiation = (1-0.23)*solin,
      
      # LONGTERM RADIATION
      longtermRadiation = 4.903*10^(-9)*((ltemp+273.16)^4+(htemp+273.16)^4)/2*(0.34-0.14*vapourPress^0.5)*(1.35*solin/globalRadiation-0.35),
      
      # RADIATION BALANCE
      radiationBalance = 0.408*(netRadiation - longtermRadiation),
      
      # EVAPOTRANSPIRATION
      et = delta * radiationBalance + psi * tempt * ((satPress_max + satPress_min)/2 - vapourPress)
    )

  # CREATE FILL WITH JUST THE DATA NEEDED
  dat_weather_i_abbrev <- dat_weather_i_full %>% 
    select(doy,xtemp,precip,solin,xvh,et)
  
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