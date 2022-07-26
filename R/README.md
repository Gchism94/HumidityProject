# README for R scripts used in _Temnothorax rugatulus_ ants do not change their nest walls in response to environmental humidity

## Overview 
R scripts used for manuscript: _Temnothorax rugatulus_ ants do not change their nest walls in response to environmental humidity 

***

## Purpose of the study 
### The regulation of humidity is one purpose of ant nests. _Temnothorax rugatulus_ colonies modify their nests in rock crevices through walls build from environmental substrates. This study examines whether _T. rugatulus_ colonies change their built nest walls in response to environmental humidity. 


***

## Dependencies
##### The run order for the scripts:
1. SETUP.R - Contains all REQUIRED R packages and loads all raw data from [Analysis]/data/raw_data
2. Humidity_Script.R - Script used to run all data wrangling, analyses, and generate plots for all but power simulations
3. HumidPwrSim.R - Power simulations for linear mixed effects models concerning the effect of humidity and colony size on built nest wall properties
_NOTE_: You can run HumidPwrSim.R independently (after SETUP.R) if you desire.

##### The script "Humidity_Script.R" is executable so long as all data are loaded into the environment
##### Several packages are required, however all are loaded through the package "pacman", so be certain to install this package before running any other code.
##### See the following documentation for further information on the "pacman" package: https://www.rdocumentation.org/packages/pacman/versions/0.5.1 

***
