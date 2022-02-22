# HumidityProject

## Overview 
Data and R script used for manuscript: _Temnothorax rugatulus_ ants do not change their nest walls in response to environmental humidity 

## Dependencies
##### The script "Humidity_Script.R" is executable so long as all data are loaded into the environment
##### Several packages are required, however all are loaded through the package "pacman", so be certain to install this package before running any other code.
##### See the following documentation for further information on the "packman" package: https://www.rdocumentation.org/packages/pacman/versions/0.5.1 

## Structure of the data
##### SupplementalHygrometerDatabase.csv
###### Raw hygrometer data that is used to calculate the average environmental humidity and temperature for each Trial:Salt combination
* Colony: Unique experimental colony identifiers 
* TrialNumber: Sequential trial number (1-4) that is NOT unique for each colony - see "Trial"
* Salt: Saturated salt solution used
* "Date Time, GMT-07:00": Date and time of each observation  
Colony,TrialNumber,Salt,"Date Time, GMT-07:00",Temp,RH,Trial
