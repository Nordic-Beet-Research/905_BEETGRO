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
    "plotrix_3.8-2",
    "data.table_1.14.2",
    "dplyr_1.0.7"
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
  filter(year == 2021)

############################################
# SOME EXTRA STATION DATA

dat_stations <- data.frame("nr" = c(40141, 40142, 40143, 40144, 40145),
                           "lat" = c(55.88643,55.50544,55.37719,55.89354,55.77791),
                           "long" = c(14.03551, 14.07499, 13.40477, 12.9145510, 13.32169),
                           "height" = c(10, 10, 10, 10, 10),
                           "wind_height" = c(10 ,10 ,10 ,10, 10))

dat <- left_join(dat, dat_stations, by = "nr")

############################################
# CALCULATE ET, AND THE STEPS ALONG THE WAY

dat <- dat %>% 
  mutate(wind_ground = xvh * 4.87/log(67.8*wind_height-5.42))
         