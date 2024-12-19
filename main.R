# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(sf)
library(terra)
library(rnaturalearth)
library(ggpubr)

# first, preprocess systematic review data
source("scripts/data_prep.R")

# systematic review analysis
source("scripts/sysreview_analysis.R")


