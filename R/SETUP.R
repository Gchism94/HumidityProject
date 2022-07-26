#########################################################################################################################################
# Author: GREG CHISM
# Date: JULY 2022
# email: gchism@arizona.edu
# Project: "Temnothorax rugatulus" ants do not change their nest walls in response to environmental humidity
# Title: SETUP
#########################################################################################################################################

# MUST RUN BEFORE ANY OTHER .R SCRIPT!!

# The below script was utilized to analyze and visualize data from the associated manuscript.

# Installing and loading packages used for graphs and analyses
install.packages("pacman") # Download package with function to load multiple packaged at once

# Loading required packages for code below. p_load() will download packages that aren't in system library
pacman::p_load(FSA,
               ggpubr,
               here,
               lme4,
               lmerTest,
               MuMIn,
               scales,
               simr,
               tidyverse)

#########################################################################################################################################
# IMPORTING THE REQUIRED DATASETS TO RUN THE BELOW SCRIPTS
#########################################################################################################################################

# RAW DATA 
#########################################################################################################################################
# Main database with nest wall properties
HumidityExperimentalDatabase <- read.csv(here("analysis", "data", "raw_data", "HumidityExperimentalDatabase.csv"))

# Supplemental database with relative humidity (%) and temperature (celcius) values from experimental container
SupplementalHygrometerDatabase <- read.csv(here("analysis", "data", "raw_data", "SupplementalHygrometerDatabase.csv"))

# Database showing the number of workers and brood that died after each colony was exposed to their first relative humidity (%) trial
HumidMortalityRaw <- read.csv(here("analysis", "data", "raw_data", "HumidMortalityRaw.csv"))

# Porosity values for all substrates
PorosityComparisonRaw <- read.csv(here("analysis", "data", "raw_data", "PorosityComparisonRaw.csv"))

# DERIVED DATA (from Humidity_Script.R, if you want to run HumidPwrSim.R independently)
#########################################################################################################################################
# Final wrangled/usable data from Humidity_Script.R for all data points
Complete_Data_Final <- read.csv(here("analysis", "data", "derived_data", "Complete_Data_Final.csv"))

# Final wrangled/usable data from Humidity_Script.R for Brood
Complete_Data_FinalBroodCount <- read.csv(here("analysis", "data", "derived_data", "Complete_Data_FinalBroodCount.csv"))

# Final wrangled/usable data from Humidity_Script.R for Workers
Complete_Data_FinalWorkerCount <- read.csv(here("analysis", "data", "derived_data", "Complete_Data_FinalWorkerCount.csv"))

