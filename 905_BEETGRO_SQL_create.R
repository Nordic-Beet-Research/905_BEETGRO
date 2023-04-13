############################################
############################################
##
## BBRO BEETGRO MODEL - SQL Database
##
## v2023-01-18
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
  
  rm(list = ls())
}

 ############################################
# CONNECT TO SERVER

con <- dbConnect(RPostgres::Postgres(),
                 user="postgres",
                 password="easy",
                 host="localhost",
                 port=5433, # This might now be 5433 with the new version (v6.15) of PostgreSQL
                 dbname="postgres")

dbListTables(con)

# dbDisconnect(con)

############################################
# CREATE DATABASE
## REMOVE OLD VERSION
query <- dbSendQuery(con, "DROP DATABASE nbyc;")
query
dbClearResult(query)

## CREATE NEW VERSION
query <- dbSendQuery(con, "CREATE DATABASE nbyc;")
query
dbClearResult(query)

## SWAP CONNECTION TO THE NBR_WEATHER DATABASE
con <- dbConnect(RPostgres::Postgres(),
                 user="postgres",
                 password="easy",
                 host="localhost",
                 port=5433,
                 dbname="nbyc")

############################################
#CREATE TABLES
## SITES
### CREATE TABLE
query <- dbSendQuery(con,
                     "create table sites(
            id BIGSERIAL NOT NULL PRIMARY KEY,
            site VARCHAR(25)
            );"
)
query
dbClearResult(query)

### POPULATE TABLE
sites <- transpose(data.frame(
  c(1,"Ädelholm")
))
colnames(sites) <- c("id","site")
sites$id <- as.numeric(sites$id)

dbWriteTable(con, "sites", sites, append=T)

## DESIGN
### CREATE TABLE
query <- dbSendQuery(con,
                     "create table designs(
            id BIGSERIAL NOT NULL PRIMARY KEY,
            design VARCHAR(50)
            );"
)
query
dbClearResult(query)

### POPULATE TABLE
designs <- transpose(data.frame(
  c(1,"Basic"),
  c(2,"Plant population adjusted"),
  c(3,"Canopy adjusted"),
  c(4,"Alternative soil b value"),
  c(5,"Alternative weather source")
))
colnames(designs) <- c("id","design")
designs$id <- as.numeric(designs$id)

dbWriteTable(con, "designs", designs, append=T)

## WEATHER SOURCES
### CREATE TABLE
query <- dbSendQuery(con,
                     "create table weather_sources(
            id BIGSERIAL NOT NULL PRIMARY KEY,
            weather_source VARCHAR(25)
            );"
)
query
dbClearResult(query)

### POPULATE TABLE
weather_sources <- transpose(data.frame(
  c(1,"Lantmet"),
  c(2, "Own")
))
colnames(weather_sources) <- c("id","weather_source")
weather_sources$id <- as.numeric(weather_sources$id)

dbWriteTable(con, "weather_sources", weather_sources, append=T)

## TRIAL
### CREATE TABLE
query <- dbSendQuery(con,
                     "create table trials(
            id NUMERIC NOT NULL PRIMARY KEY,
            external_id NUMERIC,
            site INTEGER REFERENCES sites (id),
            design INTEGER REFERENCES designs (id),
            year NUMERIC,
            latitude NUMERIC,
            elevation NUMERIC,
            soil_b NUMERIC,
            weather_source INTEGER REFERENCES weather_sources (id),
            weather_source_info NUMERIC,
            sow_date DATE,
            harvest_date DATE,
            em_date DATE,
            emergence NUMERIC,
            plant_pop NUMERIC,
            canopy1 NUMERIC,
            canopy2 NUMERIC,
            pop1 NUMERIC,
            pop2 NUMERIC,
            pop3 NUMERIC,
            can1adoy NUMERIC,	
            can1acov NUMERIC,	
            can1atem NUMERIC,
            can1bdoy NUMERIC,
            can1bcov NUMERIC,
            can1btem NUMERIC,
            can1cdoy NUMERIC,
            can1ccov NUMERIC,
            can1ctem NUMERIC,
            can1ddoy NUMERIC,
            can1dcov NUMERIC,
            can1dtem NUMERIC,
            can1edoy NUMERIC,
            can1ecov NUMERIC,
            can1etem NUMERIC,
            can2adoy NUMERIC,
            can2alos NUMERIC,
            can2bdoy NUMERIC,
            can2blos NUMERIC
                     );"
)
query
dbClearResult(query)

## RESULTS
### CREATE TABLE
query <- dbSendQuery(con,
                     "create table results(
            id NUMERIC NOT NULL PRIMARY KEY,
            trial_id INTEGER REFERENCES trials (id),
            doy NUMERIC,
            xtemp NUMERIC,
            precip NUMERIC,
            solin NUMERIC,
            et NUMERIC,
            bbch NUMERIC,
            d_t NUMERIC,
            sown BOOL,
            cd_sow NUMERIC,
            emerged BOOL,
            cd_em NUMERIC,
            smd NUMERIC,
            smd0 NUMERIC,
            qrel NUMERIC,
            f NUMERIC,
            cd_stress NUMERIC,
            sse NUMERIC,
            biomass NUMERIC,
            yield NUMERIC,
            w_stress NUMERIC,
            xt_stress NUMERIC,
            f_temp NUMERIC,
            root_depth NUMERIC,
            d_sse NUMERIC,
            eatmos NUMERIC,
            q NUMERIC,
            psi_soil NUMERIC,
            rsum NUMERIC,
            hc_soil NUMERIC,
            esoil NUMERIC,
            ea NUMERIC,
            d_smd NUMERIC,
            rue NUMERIC,
            d_biomass NUMERIC,
            fsugar NUMERIC,
            yield_pop NUMERIC
                     );"
)
query
dbClearResult(query)



dbDisconnect(con)
rm(con, query)
