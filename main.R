# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggpubr)
library(patchwork)
library(exactextractr)
library(ggridges)

# functions ----
source("scripts/functions/cache_csv.R")

# prep ----
source("scripts/my_colours.R")

# analysis scripts ----

## systematic review ----

# script below for pre-processing systematic review data, do not need to re-run
#source("scripts/sysreview_dataprep.R")

# using data from full manual systematic review, prepare figures and tables
source("scripts/sysreview_analysis.R")

## spatial exploration ----

# mapping wind and solar facilities against ecosystems and biodiversity across Aus
# prepare data on locations of wind and solar facilities across Australia
# then intersect spatial locations with different rasters of environmental data
source("scripts/spatial_dataprep.R")
# (rasters of environmental data too large for github, sources in data_cache 
# readme but otherwise extracted values cached in csv)

# prepare figures and run analyses from spatial/environmental data
source("scripts/spatial_figures.R")

# scripts in the folder scripts/supplementary_analyses were used to assess and validate
# some of the data choices made in the main scripts, e.g. the use of GIPT data,
# the restriction of GIPT data to facilities already constructed or under construction
# no need to re-run but provided to support these choices
# compare_windsolar_datasources.R - GEM data for Australia vs National Map government data
# compare_gpkg_gipt.R - GEM points vs Global Renewables Watch footprints for the global
#   comparison of studies and facilities (needs data_cache files)
# preconstruction_gipt_analysis.R - effect of including pre-construction GEM projects
#   on the study vs facility comparison

# record R and package versions used
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")

