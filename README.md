# README for _Temnothorax rugatulus_ ants do not change their nest walls in response to environmental humidity

## Overview 
Data and R script used for manuscript: _Temnothorax rugatulus_ ants do not change their nest walls in response to environmental humidity 

## Purpose of the study 
### The regulation of humidity is one purpose of ant nests. _Temnothorax rugatulus_ colonies modify their nests in rock crevices through walls build from environmental substrates. This study examines whether _T. rugatulus_ colonies change their built nest walls in response to environmental humidity. 

## Dependencies
##### The run order for the scripts: 
1. Humidity_Script.R - Script used to run all data wrangling, analyses, and generate plots for all but power simulations
2. HumidPwrSim.R - Power simulations for linear mixed effects models concerning the effect of humidity and colony size on built nest wall properties
##### The script "Humidity_Script.R" is executable so long as all data are loaded into the environment
##### Several packages are required, however all are loaded through the package "pacman", so be certain to install this package before running any other code.
##### See the following documentation for further information on the "pacman" package: https://www.rdocumentation.org/packages/pacman/versions/0.5.1 

## Structure of the data
#### SupplementalHygrometerDatabase.csv
###### Raw hygrometer data that is used to calculate the average environmental humidity and temperature for each Trial:Salt combination
* Colony: Unique experimental colony identifiers 
* TrialNumber: Sequential trial number (1-4) that is NOT unique for each colony - see "Trial"
* Salt: Saturated salt solution used
* "Date Time, GMT-07:00": Date and time of each observation  
* Temp: Temperature in celcius
* RH: Relative humidity (%) 
* Trial: The trial number for each individual colony, each colony underwent two trials

#### HumidityExperimentalDatabase.csv
###### Raw experimental data with nest features and colony size
* Colony: Unique experimental colony identifiers
* Trial: The trial number for each individual colony, each colony underwent two trials
* TrialNumber: Sequential trial number (1-4) that is NOT unique for each colony - see "Trial"
* Day: The day in the experimental timeline (always 10, but days 1 and 5 were captured and not considered)
* Area: Area of the built nest wall (mm<sup>2</sup>)
* Length: Length of the built nest wall (mm)
* Nest.Area: Area of the internal nest space (mm<sup>2</sup>)
* HumLevel: Whether the colony started with a higher or lower relative humidity (High/Low)
* Number.Ant: The number of workers in the colony
* Number.Brood: The number of brood in the colony
* Number.Queens: The number of brood in the colony
* Salt: Saturated salt solution used
* SubstrateISide: Substrate I placement in the container from the perspective of looking out from the nest entrance
* StartWtI: The initial weight (g) of the available substrate I building nest wall material
* UsedWtI: The weight (g) of the available substrate I building nest wall material following the experimental building phase
* StartWtII: The initial weight (g) of the available substrate II building nest wall material
* UsedWtII: The weight (g) of the available substrate II building nest wall material following the experimental building phase
* CollWallWt: The weight (g) of the experimental nest wall that each colony built

#### HumidMortalityRaw.csv
###### Raw experimental data with proportion of workers and brood dead after each colony underwent Trial 1
* Colony: Unique experimental colony identifiers
* WorkerDeath: Proportion of workers that died 
* BroodDeath: Proportion of brood that died 
* TrialNumber: Sequential trial number (1-4) that is NOT unique for each colony

#### PorosityComparison.csv
###### Data comparing the porosities of each experimental substrate, experimentally built walls, and collected _Temnothorax rugatulus_ walls
* SubstrateID: A unique identifier for each substrate replicate
* Trial: The trial number for each individual colony, each colony underwent two trials (only applicable for experimentally built walls) 
* SubCategory: The type of substrate (Sub I, Sub II, Built, Natural)
* TotalVolume: The combined pore (void space in compact substrate grains) and soil volume (ml) of a substrate
* SoilVolume: The volume that the soil filled (ml), without pore volume (ml)
* PoreVolume: The void space in between compact substrate grains (ml)
* Porosity: The percentage of void space in compact substrate - PoreVolume/TotalVolume
