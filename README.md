# 780_growth_model

Modelling potential sugar beet growth across the season.

This model is a translation of the BBRO BeetGRO model into R. No changes have been made to the model, but the system is slightly different

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
