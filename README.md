# 905_BEETGRO

Modelling potential sugar beet growth across the season.

This model is a translation of the BBRO BeetGRO model into R. No changes have been made to the calculations in the model, but the system is slightly different

########################################################
OPERATION

To run the model, ensure you have:
# The right R setup
- R (version 4.1.2: https://cran.r-project.org/bin/windows/base/old/4.1.2/)
- R studio (https://www.rstudio.com/products/rstudio/download/#download)
- A directory that mirrors the system environment variable R_LIBS or R_LIBS_USER. In this version, the directory C:/R packages_412. This can be changed at line 51.
# The right scripts
- The model files.
-- One of the weather import and manipulation files. "905_BEETGRO_weather"
-- One of the model files. "905_BEETGRO_basic.R"
# The right input data
- input files, housed in the same directory as the model file.
-- "parameters.xlsx" (One file total. Contains model parameters that shouldn't be modified.)
-- "TrialData.xlsx" (One file total. Contains a list of the trials that are to be analysed. A trial = a unique site x year combination.)

What is run is based on what is in the TrialData.xlsx file. That is, the system will work through each row of this file. Note that there are two changes in the NBR version of this file; 1. Dates are used instead of Day-Of-Year, 2. A source to the weather data is given. This weather data source will determine how where raw data is imported from. This is currently either Lantmet in Sweden, or a separate .xlsx file. 

Weather data: 
- The weather data source is defined in TrialData. 
-- It can be either "Lantmet" or "File". 
--- "Lantmet" will query the Lantmet database for the weather station specified in the column "LantmetID". 
--- "File" will query the directory "./weather" for the right Site. 
---- If using "File", there should be one .xlsx file of the raw weather data per Site (= per grower ID), with one day per row, and with the name Site###.xlsx. 
---- As many years of data as necessary can be entered, but each year that is analysed should be complete. 
- The output file from the 905_BEETGRO_weather.R script will be one .xlsx file per Trial.

########################################################
THE MODEL

###########################
SETUP

Just sets up R to run the rest of the code

###########################
READ IN DATA

Reads in the data specified in parameters.xlsx, TrialData.xlsx, and the relevant Site###.xlsx files.

stop(): if a required Site###.xlsx file is missing, this will stop the code and throw a message in the R Studio console.

###########################
INITIALISE TRIAL i

Sets key parameters for trial i.

Taken from the original 'Initial' tab of the Beet module.

########################################################
TROUBLESHOOTING

########################################################
ISSUES

The following issues were noted during the translation of the model.

- Given q = qfc, qrel always = 1.
- Why does T_Stress (DailyTemp in OpenModel) not apply if temp >= 25?
- in Beet::Main::row 88, it is not possible to solve this when TotalTemp is no set to the limit of 950 (or 990, as taken in the translaton. rows 82-84) before fTemp is defined (row 76). Otherwise, the Ln of a negative is asked for...
- The User Observed canopy updates do not seem to be active. In Beet::Main::row_93, Trials.Can1aTemp is required, but it is not defined well after row 93 (row 376).

########################################################
ASSUMPTIONS IN THE MODEL

If no soil texture is given, a sandy loam (b = 2.1) is taken.
If no date of emergence is given, a date based on sowing data and degree days is taken.

########################################################
DEVELOPER NOTES

This version of the model was developed using the Tidyverse, where possible.

Following the original OpenModel version, this version of the model loops through the trials
