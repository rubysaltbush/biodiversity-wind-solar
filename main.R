# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggpubr)
library(patchwork)
library(jsonlite)
library(networkD3)
library(htmlwidgets)
library(webshot)
library(exactextractr)
library(ggridges)

# functions ----
source("scripts/functions/cache_csv.R")

# prep ----
source("scripts/my_colours.R")

# analysis scripts ----

## systematic review ----

# script below for preprocessing systematic review data, do not need to re-run
#source("scripts/sysreview_dataprep.R")

# using data from full manual systematic review, prepare figures and summaries
source("scripts/sysreview_analysis.R")

## spatial analysis ----

# mapping wind and solar facilities against ecosystems and biodiversity across Aus
# prepare data on locations of wind and solar facilities across Australia
source("scripts/spatial_dataprep.R")

# then intersect spatial locations with different rasters of environmental data
# prepare figures and run analyses from these data
source("scripts/spatial_figures.R")

