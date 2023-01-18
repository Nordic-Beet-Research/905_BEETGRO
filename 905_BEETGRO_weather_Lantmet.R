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
    "RPostgreSQL_0.7-3",
    "RPostgres_1.4.1",
    "ggplot2_3.3.5",
    "dplyr_1.0.7",
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
  
  rm(list = ls())
}

############################################
# IMPORT DATA FROM WEATHER DATABASE

# CONNECT TO DATABASE

con <- dbConnect(RPostgres::Postgres(),
                 user="postgres", 
                 password="easy",
                 host="localhost", 
                 port=5432,
                 dbname="nbr_weather")

dbListTables(con) 

# QUERY DATABASE

weather_dat <- dbGetQuery(con, "SELECT * FROM weather")

dbDisconnect(con)

############################################
# TAKE DATA FROM JUST THE ONE YEAR

dat <- weather_dat %>%
  filter(year %in% c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021))

############################################
# SOME EXTRA STATION DATA

dat_stations <- data.frame("nr" = c(40141, 40142, 40143, 40144, 40145),
                           "lat" = c(55.88643,55.50544,55.37719,55.89354,55.77791),
                           "long" = c(14.03551, 14.07499, 13.40477, 12.9145510, 13.32169),
                           "mamsl" = c(10, 10, 10, 10, 10),
                           "wind_height" = c(2 ,2 ,2 ,2, 2))

dat <- left_join(dat, dat_stations, by = "nr") %>% 
  rename(doy = dagnr)

############################################
# CALCULATE ET, AND THE STEPS ALONG THE WAY

filt_year <- 2021
filt_nr <- 40141

dat_weather <- dat %>% 
  filter(year == filt_year,
         nr == filt_nr) %>% 
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

############################################
# WRITE EXCEL FOR CONNECTION TO THE NEXT STAGE: EXAMPLE DATA ONLY!!

write_xlsx(dat_weather, paste0("dat_weather_",filt_year,"_",filt_nr,".xlsx"))

############################################
# # VISUALISATIONS
# 
# ggplot(data = dat[which(dat$nr == 40145),], aes(x=doy, y=et)) +
#   geom_line()
# 
# ggplot(data = dat, aes(x=et)) +
#   geom_histogram()
