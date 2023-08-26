############################################
############################################
##
## Reading in current year's weather data, backfilling with historical averages
##
## v1.1 - Getting data from Lantmet daily, 
## # Requires a local DropBox connection
## # sources information on trials from Dropbox .../80. Projects/905 NBYC/Data/2023/ for the live 2023 data
## # writes data back to the same DropBox folder
## # used in discussions around the model in 2023
##
## This project uses R 4.3.1 
## with snapshot date 2023-08-01
##
############################################
############################################

# Setup
# SETUP HAS BEEN SIMPLIFIED BECAUSE OF THE PROBLEM OF THE DEPRECIATED/ CLOSED MRAN REPOSITORY. 
# FOR NOW, JUST USING LIBRARY
{  
  library(data.table)
  library(ggplot2)
  library(dplyr)
  library(writexl)
  library(lubridate)
  library(readxl)
}

############################################
# IMPORT TRIALDATA AND EXTRACT WEATHER DATA SOURCES
## Read in trial data
trial <- read_xlsx("C:/Users/we/Dropbox/NBR/80. Projects/905 Nordic Beet Yield Challenge NBYC/Data/2023/TrialData.xlsx", sheet = "TrialData")
param <- pivot_wider(read_xlsx("C:/Users/we/Dropbox/NBR/80. Projects/905 Nordic Beet Yield Challenge NBYC/Data/2023/parameters.xlsx", sheet = "parameters"), names_from = parameter)
weath_hist <- read_xlsx("C:/Users/we/Dropbox/NBR/80. Projects/905 Nordic Beet Yield Challenge NBYC/Data/2023/NBYCweather_hist.xlsx", sheet = "Sheet1")

# CREATE HISTORICAL AVERAGES BY STATION
weath_hist_aves <- weath_hist %>% 
  group_by(weath_id, doy) %>% 
  summarise(weath_id = max(weath_id),
            Tmax = mean(Tmax),
            Tmin = mean(Tmin),
            precip = mean(precip),
            radiation_solar = mean(radiation_solar),
            evap = mean(evap),
            vh_x = mean(vh_x),
            temp_x = mean(temp_x)) %>% 
  mutate(year = 1522) %>% 
  ungroup()


# SOURCE CURRENT YEAR's DATA

year_current <- 2023

trial_current <- trial %>% 
  filter(year == year_current) %>% 
  distinct(weather_source, .keep_all = T)

i=1L

weath_current <- data.frame(date = Date(), year = numeric(), weath_id = numeric(), doy = numeric(),
                            temp_x = numeric(), temp_l = numeric(), temp_h = numeric(), precip = numeric(),           
                            vh_x = numeric(), radiation_solar = numeric(), hum_x = numeric(), hum_l = numeric(), hum_h = numeric(),           
                            wind_ground= numeric(), press_slope = numeric(), press = numeric(), psychroC = numeric(),         
                            delta = numeric(), psi = numeric(),temp_t = numeric(), press_sat_h = numeric(), press_sat_l = numeric(),      
                            press_vapour = numeric(), sun_dist = numeric(),sun_decl = numeric(), sun_angle = numeric(), radiation_space = numeric(),  
                            radiation_global= numeric(), radiation_net = numeric(), radiation_LW = numeric(), radiation_balance = numeric(), evap = numeric())

for(i in 1:nrow(trial_current)){
  trial_i <- trial_current[i,]
  source_i <- trial_i$weather_source
  if(source_i == 1) source_id_i <- trial_i$weather_source_info # 1 == Lantmet, 2 == File
  if(source_i == 2) source_id_i <- paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),".xlsx")


  ############################################
  # IMPORT DATA FROM LANTMET https://www.ffe.slu.se/lm/JSON/JSON-Specifikation.pdf
  if(source_i == 1){
    ### create url values
    urlBase <- "https://www.ffe.slu.se/lm/json/downloadJS.cfm?outputType=CSV&AddID=1"
    startDate <-  date(paste0(year_current,"-01-01"))
    endDate <- today()-1
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
    wind_height <- 2 # height above ground at which wind is measured
    Albedo <- param$Albedo
    SBconstant <- param$SBconstant
    mamsl <- trial_i$elevation
    lat <- trial_i$latitude
    
    dat_weather_i <- dat_Lantmet %>%
      select(!c("HOUR","WSTNID")) %>%
      rename_at(vars(all_of(names_old)), ~ names_new) %>% 
      mutate(date = date(date),
             year = year(date),
             weath_id = source_id_i,
            doy = yday(date),
            vh_x = replace(vh_x, vh_x == "NA", 2),
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
    
  }
  
  weath_current <- weath_current %>% 
    bind_rows(dat_weather_i)
  
  ############################################
  # WRITE EXCEL
  
  # write_xlsx(dat_weather_i_abbrev, paste0("./weather/Site",sprintf("%03d", trial_i$SiteID),"_",trial_i$Year,".xlsx"))
  write_xlsx(dat_weather_i, paste0("./weather/Site",sprintf("%03d", trial_i$user),"_",trial_i$year,"_full.xlsx"))

}

# JOIN ALL THE DATA TOGETHER
# CREATE FILE WITH JUST THE DATA NEEDED
weather_current_abbrev <- weath_current %>% 
  select(year, weath_id, doy, temp_h, temp_l, precip,radiation_solar,vh_x,evap,temp_x,) %>% 
  rename(Tmax = temp_h,
         Tmin = temp_l)

NBYCweath <- weath_hist %>% 
  bind_rows(weather_current_abbrev, weath_hist_aves)

write_xlsx(NBYCweath, "C:/Users/we/Dropbox/NBR/80. Projects/905 Nordic Beet Yield Challenge NBYC/Data/2023//NBYCweather.xlsx")
