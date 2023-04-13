############################################
############################################
##
## 905.0 NBYC.
##
## This script is used to send trial data data to SQL database
## 
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
              "data.table_1.14.2",
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
}


############################################
# IMPORT DATA

trial_dat <- read_xlsx("./TrialData.xlsx", sheet = "TrialData")

trial_dat <- trial_dat[,which(colnames(trial_dat) %in% c(
  "id",
  "external_id",
  "site",
  "design",
  "year",
  "latitude",
  "elevation",
  "soil_b",
  "weather_source",
  "weather_source_info",
  "sow_date",
  "harvest_date",
  "em_date",
  "emergence",
  "plant_pop",
  "canopy1",
  "canopy2",
  "pop1",
  "pop2",
  "pop3",
  "can1adoy",	
  "can1acov",	
  "can1atem",
  "can1bdoy",
  "can1bcov",
  "can1btem",
  "can1cdoy",
  "can1ccov",
  "can1ctem",
  "can1ddoy",
  "can1dcov",
  "can1dtem",
  "can1edoy",
  "can1ecov",
  "can1etem",
  "can2adoy",
  "can2alos",
  "can2bdoy",
  "can2blos"))]
  
############################################
# CONNECT TO SERVER

con <- dbConnect(RPostgres::Postgres(),
                 user="postgres", 
                 password="easy",
                 host="localhost", 
                 port=5433,
                 dbname="nbyc")

dbListTables(con)

dbWriteTable(con, "trials", trial_dat, append=T)

#dbDisconnect(con)

############################################
# CHECK FOR COPIES
# it checks for copies on the nr + datum columns.

query <- dbSendQuery(con,
            "DELETE FROM trials
            WHERE id IN
              (SELECT id
              FROM
                (SELECT id,
                ROW_NUMBER() OVER( PARTITION BY external_id, design, year
                ORDER BY  id ) AS row_num
              FROM trials ) t
              WHERE t.row_num > 1 );"
)
query
dbClearResult(query)

dbDisconnect(con)
