[![Project Supported by CyVerse](https://de.cyverse.org/Powered-By-CyVerse-blue.svg)](https://learning.cyverse.org/projects/vice/en/latest/) [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4527797.svg)](https://doi.org/10.5281/zenodo.4527797) [![license](https://img.shields.io/badge/license-GPLv2-blue.svg?style=flat-square)](https://opensource.org/licenses/GPL-2.0) 

# rstudio-verse

Based on the [Rocker-Project.org](https://rocker-project.org) Docker [RStudio `verse` container](https://hub.docker.com/r/rocker/verse) for CyVerse Discovery Environment (DE) data science workbench. CyVerse DE requires additional configuration files (e.g. `nginx`) to be compatible with Kubernetes orchestration. 

[An Introduction to Rocker: Docker Containers for R](https://journal.r-project.org/archive/2017/RJ-2017-065/RJ-2017-065.pdf) Carl Boettiger and Dirk Eddelbuettel, The R Journal (2017) 9:2, pages 527-536. <https://doi.org/10.32614/RJ-2017-065>

[The RockerVerse](https://journal.r-project.org/archive/2020/RJ-2020-007/index.html) Daniel Nüst, Dirk Eddelbuettel, Dom Bennett, Robrecht Cannoodt, Dav Clark, Gergely Daróczi, Mark Edmondson, Colin Fay, Ellis Hughes, Lars Kjeldgaard, Sean Lopp, Ben Marwick, Heather Nolis, Jacqueline Nolis, Hong Ooi, Karthik Ram, Noam Ross, Lori Shepherd, Péter Sólymos, Tyson Lee Swetnam, Nitesh Turaga, Charlotte Van Petegem, Jason Williams, Craig Willis and Nan Xiao , The R Journal (2020) 12:1, pages 437-461. <https://doi.org/10.32614/RJ-2020-007>

![GH actions branch parameter](https://github.com/github/docs/actions/workflows/main.yml/badge.svg?branch=main) ![GitHub commits since tagged version](https://img.shields.io/github/commits-since/cyverse-vice/rstudio-verse/latest/main?style=flat-square) [![Docker Pulls](https://img.shields.io/docker/pulls/cyversevice/rstudio-verse?color=blue&logo=docker&logoColor=white)](https://hub.docker.com/r/cyversevice/rstudio-verse) 

quick launch | size | 
------------ | ---- | 
<a href="https://de.cyverse.org/apps/de/3548f43a-bed1-11e9-af16-008cfa5ae621/launch?quick-launch-id=81b187d6-cc94-4c53-81b5-f09f31c9c8ba" target="_blank"><img src="https://img.shields.io/badge/Verse-latest-blue?style=plastic&logo=rstudio"></a> | [![SIZE](https://img.shields.io/docker/image-size/cyversevice/rstudio-verse/latest.svg)](https://img.shields.io/docker/image-size/cyversevice/rstudio-verse/latest)

# Instructions

## Run this Docker locally or on a Virtual Machine

To run these containers, you must first pull them from DockerHub

```
docker pull harbor.cyverse.org/vice/rstudio/verse:latest
```

```
docker run -it --rm -p 8787:80 -e REDIRECT_URL=http://localhost:8787 harbor.cyverse.org/vice/rstudio/verse:latest
```

The default username is `rstudio` and password is `rstudio1`. To reset the password, add the flag `-e PASSWORD=<yourpassword>` in the `docker run` statement.

## Build your own Docker container and deploy on CyVerse VICE

This container is intended to run on the CyVerse data science workbench, called [VICE](https://cyverse-visual-interactive-computing-environment.readthedocs-hosted.com/en/latest/index.html). 

Unless you plan on making changes to this container, you should use the existing launch button above. 

###### Developer notes

To build your own container with a Dockerfile and additional dependencies, pull the pre-built image from CyVerse private/public [Harbor Registry](https://harbor.cyverse.org).

```
FROM harbor.cyverse.org/vice/rstudio/verse:latest
```

Follow the instructions in the [VICE manual for integrating your own tools and apps](https://cyverse-visual-interactive-computing-environment.readthedocs-hosted.com/en/latest/developer_guide/building.html).
