# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggpubr)
library(jsonlite)

# first, preprocess systematic review data
source("scripts/data_prep.R")

# systematic review analysis (unfinished so far)
source("scripts/sysreview_analysis.R")

# mapping wind and solar facilities against ecosystems and biodiversity across Aus
source("scripts/mapping.R")

# Will's mapping of wind/solar facilities vs climate (update with new wind/solar data once processed?)
source("scripts/mapping_climate.R")

