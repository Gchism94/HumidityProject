# HumidityProject compendium

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Gchism94/HumidityProject/HEAD)
[![DOI](https://zenodo.org/badge/511707834.svg)](https://zenodo.org/badge/latestdoi/511707834)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

A compendium of code, data, and author's manuscript accompanying the preprint:

#### Greg Chism, [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-5478-2445). *Temnothorax rugatulus ants do not change their nest walls in response to environmental humidity*. Preprint on *bioRxiv*, 02 July 2022 <https://doi.org/10.1101/2022.06.30.497551
>

To cite this repository use the following: 

Chism, G., Nichols, W., & Dornhaus, A. (2022). NestArchOrg (Version 1.0.0) [Computer software]. https://doi.org/10.5281/zenodo.6828919

## Overview
This repository is organized as a reproducible research compendium. 
Click the [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/Gchism94/HumidityProject/main?urlpath=rstudio) button above to explore in an interactive RStudio session.  Binder uses [rocker-project.org](https://rocker-project.org) Docker images to ensure a consistent and reproducible computational environment.  These Docker images can also be used locally.  

## File Organization

    analysis/
    |
    ├── paper/
    │   ├── paper.Rmd       # this is the main document to edit
    │   └── paper.pdf       # this is an elsevier .pdf written from paper.Rmd
    |
    ├── figures/            # location of the figures produced by the scripts in R
    |
    ├── data/
    │   └── RawData/        # data obtained from elsewhere
    |   
    ├── supplementary-materials/
    │   ├── Supplementary_Figures/     
    |   |                   # supplementary figures for the main manuscript
    │   └── Supplementary_Tables/      
    |                       # supplementary tables for the main manuscript 
    |
    └── R                   # Run in the following order (also see associated README.md
        ├── Humidity_Script.R
        |                   # R script used to wrangle the raw data, produce figures, analyses, and supplementary materials
        └── HumidPwrSim.R   # R script used to conduct all power analyses 
        

An `Rmd` notebook and associated pdf for the manuscript can be found in [analysis](/paper). This notebook produces a .pdf document in elsevier format.  

README.md files are included in all subdirectories with explanations or contents related to the paper. It should also provide a useful starting point for extending and exploring these materials for other projects.

Or to explore the code locally, clone or download this repository into RStudio or your preferred environment and install the compendium by running `devtools::install()`.  To install additional dependencies used only in formatting the figures, use `devtools::install(dep=TRUE)`.  


This compendium includes data found on the Zenodo repository: 
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6780270.svg)](https://doi.org/10.5281/zenodo.6780270)


