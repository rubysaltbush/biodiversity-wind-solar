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
# first, preprocess systematic review data and make figures from categories
source("scripts/sysreview_dataprep.R")
# then analyse data from full manual systematic review
source("scripts/sysreview_analysis.R")

## spatial analysis ----
# mapping wind and solar facilities against ecosystems and biodiversity across Aus
source("scripts/spatial_dataprep.R")
source("scripts/spatial_figures.R")

# TO DO - extract mean population in area around wind and solar farms??
#       - tried to create density plot for land capability but raster so large
#         it crashes R every time I try to as.data.frame it, won't work for now
