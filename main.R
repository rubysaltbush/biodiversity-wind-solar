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

# functions ----
source("scripts/functions/cache_csv.R")

# prep ----
source("scripts/my_colours.R")

# analysis scripts ----

## systematic review ----
# first, preprocess systematic review data
source("scripts/sysreview_dataprep.R")
# systematic review analysis (unfinished so far)
source("scripts/sysreview_analysis.R")

## spatial analysis ----
# mapping wind and solar facilities against ecosystems and biodiversity across Aus
source("scripts/spatial_dataprep.R")
source("scripts/spatial_figures.R")


