# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggpubr)
library(patchwork)
library(jsonlite)
library(networkD3)

# functions ----
source("scripts/functions/cache_csv.R")

# prep ----
source("scripts/my_colours.R")

# analysis scripts ----

## systematic review ----
# first, preprocess systematic review data
source("scripts/data_prep.R")
# systematic review analysis (unfinished so far)
source("scripts/sysreview_analysis.R")

## spatial analysis ----
# mapping wind and solar facilities against ecosystems and biodiversity across Aus
source("scripts/mapping.R")



