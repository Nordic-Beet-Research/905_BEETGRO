# 780_growth_model

Modelling potential sugar beet growth across the season.

This model is a translation of the BBRO BeetGRO model into R. No changes have been made to the model, but the system is slightly different

########################################################
OPERATION

To run the model, ensure you have:
- R (version 4.1.2: https://cran.r-project.org/bin/windows/base/old/4.1.2/)
- R studio (https://www.rstudio.com/products/rstudio/download/#download)
- A directory that mirrors the system environment variable R_LIBS or R_LIBS_USER. In this version, the directory C:/R packages_412. This can be changed at line 51.
- The model file "780_BEETGRO_v1.R"
- input files, housed in the same directory as the model file.
-- "parameters.xlsx" (One file total. Contains model parameters that shouldn't be modified.)
-- "TrialData.xlsx" (One file total. Contains a list of the trials that are to be analysed. A trial = a unique site x year combination.)
-- "Site###.xlsx" (One file per site listed in TrialData.xlsx. Must contain weather data for the all years used in the definition of a trial.)

In a future version, this whole system will be thrown in a Dockr or run through a shell file, so these set-up issues will disappear...

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

########################################################
ASSUMPTIONS IN THE MODEL

If no soil texture is given, a sandy loam (b = 2.1) is taken.
If no date of emergence is given, a date based on sowing data and degree days is taken.

########################################################
DEVELOPER NOTES

This version of the model was developed using the Tidyverse, where possible.

Following the original OpenModel version, this version of the model loops through the trials
