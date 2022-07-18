# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.6.0

# required
MAINTAINER Greg Chism <gchism@arizona.edu>

COPY . /HumidityProject

# go into the repo directory
RUN . /etc/environment \

  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  
  # build this compendium package
  && R -e "devtools::install('/HumidityProject', dep=TRUE)" \
  
    # make project directory writable to save images and other output
  && sudo chmod a+rwx -R HumidityProject \
  
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && sudo R -e "setwd('/HumidityProject/analysis/paper'); rmarkdown::render('paper.Rmd')"
