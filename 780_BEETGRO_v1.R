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
    "readxl_1.3.1",
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
}

############################################
# READ IN DATA

param <- read_xlsx("parameters.xlsx", sheet = "parameters")
TrialData <- read_xlsx("parameters.xlsx", sheet = "TrialData")
Site <- read_xlsx("parameters.xlsx", sheet = "Site1")

############################################
# INITIAL A single site 
i=1L

TrialData_i <- TrialData[i,]

latitude <- TrialData_i$Latitude*2*pi/360
b <- ifelse(TrialData_i$SoilB < 1, 2.1, TrialData_i$SoilB)
pop1 <- ifelse(TrialData_i$POP1 < 90000, -0.0003*(TrialData_i$POP1/1000)^2+0.0456*(TrialData_i$POP1/1000)-1.0246, 1)
pop2 <- ifelse(TrialData_i$POP2 < 90000, -0.0003*(TrialData_i$POP2/1000)^2+0.0456*(TrialData_i$POP2/1000)-1.0246, 1)
pop3 <- ifelse(TrialData_i$POP3 < 90000, -0.0003*(TrialData_i$POP3/1000)^2+0.0456*(TrialData_i$POP3/1000)-1.0246, 1)
poploss <- ifelse(TrialData_i$PlantPop == 0, 1, mean(pop1, pop2, pop3))
