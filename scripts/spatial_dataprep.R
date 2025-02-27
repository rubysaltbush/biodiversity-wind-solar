# script to prepare and cache data for spatial analysis, of locations of Australian
# wind and solar farms with environmental data including climate, vegetation
# and leaf area index

# cache csv so it can be loaded without rerunning all extractions, especially 
# given large spatial files too big to share on github

windsolar <- cache_csv("data_output/windsolar_environments.csv", function(){

# get wind/solar location data ----

  # FIRST STEP
# load data for wind and solar farms across world from globalenergymonitor.org
# and filter down to just wind and solar farms in Australia of 3MW+ capacity
# compiling single data source
windsolar <- cache_csv("data_output/windsolar_gipt_aus.csv", function(){
  
  # data downloaded from Global Integrated Power Tracker January 2025
  # read in two tabs of spreadsheet, 2nd sheet has all >=10MW projects
  wind_gipt1 <- readxl::read_xlsx("data_input/Global-Wind-Power-Tracker-June-2024.xlsx",
                                  sheet = 2)
  # read in two tabs of spreadsheet, 3rd sheet has all <10MW projects
  wind_gipt2 <- readxl::read_xlsx("data_input/Global-Wind-Power-Tracker-June-2024.xlsx",
                                  sheet = 3)
  
  # bind sheet stogether
  wind_gipt <- wind_gipt1 %>%
    dplyr::bind_rows(wind_gipt2) %>%
    dplyr::filter(`Country/Area` == "Australia") %>% # just Australian wind farms (381)
    dplyr::filter(`Capacity (MW)` >= 3) %>% # just those greater than 3 MW (378)
    dplyr::filter(`Installation Type` == "Onshore") %>% # only onshore wind (319)
    dplyr::filter(Status %in% c("operating", "construction")) # not projects still in planning stages
  rm(wind_gipt1, wind_gipt2)  
  # separate rows include different phases of same projects, combine later?
  
  # read in two tabs of spreadsheet, first with >20MW projects
  solar_gipt1 <- readxl::read_xlsx("data_input/Global-Solar-Power-Tracker-June-2024-v2.xlsx",
                                   sheet = 2)
  # and 2nd with <20MW projects
  solar_gipt2 <- readxl::read_xlsx("data_input/Global-Solar-Power-Tracker-June-2024-v2.xlsx",
                                   sheet = 3)
  # bind sheet stogether
  solar_gipt <- solar_gipt1 %>%
    dplyr::bind_rows(solar_gipt2) %>%
    dplyr::filter(Country == "Australia") %>% # just Australian solar farms (394)
    dplyr::filter(`Capacity (MW)` >= 3) %>% # just those greater than 3 MW (384)
    dplyr::filter(Status %in% c("operating", "construction")) # not projects still in planning stages
  rm(solar_gipt1, solar_gipt2)  
  # okay this gives 154 rows, most so far although separate rows include 
  # different phases of same projects)
  
  # select and rename columns
  wind_gipt <- wind_gipt %>%
    dplyr::select(date_last_researched = `Date Last Researched`, 
                  project_name = `Project Name`,
                  phase_name = `Phase Name`,
                  project_other_names = `Other Name(s)`,
                  capacity_mw = `Capacity (MW)`,
                  status = Status,
                  start_year = `Start year`,
                  operator = Operator,
                  owner = Owner,
                  lat = Latitude,
                  lon = Longitude,
                  location_accuracy = `Location accuracy`,
                  city = City,
                  state = `State/Province`
    ) %>%
    dplyr::mutate(type = "wind")
  
  solar_gipt <- solar_gipt %>%
    dplyr::select(date_last_researched = `Date Last Researched`, 
                  project_name = `Project Name`,
                  phase_name = `Phase Name`,
                  project_other_names = `Other Name(s)`,
                  capacity_mw = `Capacity (MW)`,
                  status = Status,
                  start_year = `Start year`,
                  operator = Operator,
                  owner = Owner,
                  lat = Latitude,
                  lon = Longitude,
                  location_accuracy = `Location accuracy`,
                  city = City,
                  state = `State/Province`
    ) %>%
    dplyr::mutate(type = "solar")
  
  windsolar <- wind_gipt %>%
    dplyr::bind_rows(solar_gipt)
  rm(solar_gipt, wind_gipt)
  
  # write combined data to output folder for error checking
  readr::write_csv(windsolar, "data_output/windsolar_gipt_aus.csv")
  
})

  # SECOND STEP
# now read in error-checked version of this data, with lat/longs manually
# updated by reference to satellite imagery to ensure accuracy
windsolar <- readr::read_csv("data_input/windsolar_gipt_aus_checked.csv")

  # TIDY UP
# now summarise data so that each row represents a single project and location
windsolar <- windsolar %>%
  dplyr::filter(start_year <= 2025) %>% # filter out 7 projects that haven't started construction yet
  dplyr::group_by(project_name, project_other_names, type, status, 
                  operator, owner, lat, lon, state) %>%
  dplyr::summarise(phase_names = paste0(phase_name, collapse = ","),
                   totalcapacity_mw = sum(capacity_mw),
                   earliest_start = min(start_year),
                   latest_start = max(start_year))
length(windsolar$project_name[duplicated(windsolar$project_name)])
# 38 -> 23 projects on multiple rows, mostly those with multiple stages in different locations
length(unique(windsolar$project_name))
# 239 unique projects


# biomes ----
# read in shapefile of Australian biomes, borders = IBRA subregions
biomes <- sf::st_read("data_input/ibra7_biomes_background.shp")

# make sure all crs the same
windsolar <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
sf::st_crs(biomes) <- "EPSG:4326"

# extract biome for each wind and solar location
windsolar <- sf::st_join(windsolar, biomes) %>%
  dplyr::distinct()

# clean up df not needed anymore
rm(biomes)

# NVIS vegetation ----

## present day ----

# load geotiff raster version of national veg mapping, NVIS, in Albers equal projection
# raster is 3+GB so file is in data_cache
nvis_extant <- terra::rast("data_cache/NVIS_MVG_07_tif/nvis-mvg.tif")
# too big to plot but can use this to extract useful data I hope
nvis_extant
# class       : SpatRaster 
# dimensions  : 38371, 40101, 2  (nrow, ncol, nlyr)
# resolution  : 100, 100  (x, y)
# extent      : -1888000, 2122100, -4841000, -1003900  (xmin, xmax, ymin, ymax)
# coord. ref. : GDA2020 / Australian Albers (EPSG:9473) 
# source      : nvis-mvg.tif 
# name        : nvis-mvg_1 
# min value   :          0 
# max value   :         99 
# raster has unnecessary layer, remove
nvis_extant <- nvis_extant[[1]]

# also read in code cheat sheet for categories
mvgcodes <- readr::read_csv("data_input/mvg_codes_names.csv")

# extract values for wind/solar farms from raster 

# vectorise wind and solar farm points
windsolarV <- terra::vect(windsolar)
# re-project into raster's Albers equal area projection
windsolarV <- terra::project(windsolarV, terra::crs(nvis_extant))

# extract values
extraction <- terra::extract(nvis_extant, windsolarV)
# it works!! 
table(extraction$`nvis-mvg_1`)
# 3   5   6   7  11  12  13  14  15  17  18  19  20  21  22  25  27  29  99 
# 4   6   2   1   5   1   1   1   1   2   2   5   1   2   7 217   2   1   1 
# mostly 25, the code for cleared land, which makes sense. woot woot.

# join back onto data (all in same order, have checked)
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::rename(mvg_code = `nvis-mvg_1`) %>%
  dplyr::select(-ID) %>%
  dplyr::left_join(mvgcodes, by = "mvg_code") # add on names for MVG codes
rm(extraction, nvis_extant)

## pre-1750 ----
# try same with pre-1750 mapping

# load geotiff raster version of national veg mapping, NVIS, in Albers equal projection
# raster is 3+GB so file is in data_cache
nvis_pre1750 <- terra::rast("data_cache/NVIS_V7_0_AUST_RASTERS_PRE_ALL_GEOTIFF/nvis7_0_aust_pre_mvg_alb.tif")
# too big to plot but can use this to extract useful data I hope
nvis_pre1750
# class       : SpatRaster 
# dimensions  : 38361, 40091, 2  (nrow, ncol, nlyr)
# resolution  : 100, 100  (x, y)
# extent      : -1887000, 2122100, -4840000, -1003900  (xmin, xmax, ymin, ymax)
# coord. ref. : GDA2020 / Australian Albers (EPSG:9473) 
# source      : nvis7_0_aust_pre_mvg_alb.tif 
# names       : nvis7_0_aust_pre_mvg_alb_1, nvis7_0_aust_pre_mvg_alb_2 
# raster has unnecessary layer, remove
nvis_pre1750 <- nvis_pre1750[[1]]

# projections should all match so can just extract directly
# extract values
extraction <- terra::extract(nvis_pre1750, windsolarV)
# it works!! 
table(extraction$nvis7_0_aust_pre_mvg_alb_1)
# 1   2   3   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  26  27  31 32
# 4   1  22 107   7   1   3   1   1  21   2   1  18   2   2  12   4  28   1   7   7   1   1   1 7

# without cleared land considered the original ecosystems which wind and solar
# farms are located on seem more diverse, though eucalypt woodlands (5) has most

# rename mvgcodes columns
mvgcodes <- dplyr::rename(mvgcodes, mvgpre1750_code = mvg_code, 
                          mvgpre1750_name = mvg_name,
                          mvgpre1750_simplified = mvg_simplified)

# join back onto data (all in same order, have checked)
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::rename(mvgpre1750_code = nvis7_0_aust_pre_mvg_alb_1) %>%
  dplyr::select(-ID) %>%
  dplyr::left_join(mvgcodes, by = "mvgpre1750_code") # add on names for MVG codes
# interesting that pre1750 and extant codes are different even for uncleared land
rm(extraction, mvgcodes, nvis_pre1750)

# land capability mapping (Adams & Engert 2023) ----

# read in national scale agricultural land capability map
land_capability <- terra::rast("data_cache/RD-LANDCAPAUS-02/land_capability.tif")
land_capability
# class       : SpatRaster 
# dimensions  : 115580, 138333, 1  (nrow, ncol, nlyr)
# resolution  : 3e-04, 3e-04  (x, y)
# extent      : 112.5014, 154.0013, -43.80178, -9.12778  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat GDA94 (EPSG:4283) 
# source      : land_capability.tif 
# name        : land_capability 
# min value   :               1 
# max value   :               9 
plot(land_capability)
# 1 = high agricultural value land, 8 = low value, 9 excluded from mapping
# some obvious artefacts along state boundaries

# re-project into raster's GDA94 projection
windsolarV <- terra::project(windsolarV, terra::crs(land_capability))
plot(windsolarV, add = TRUE)

# extract values
extraction <- terra::extract(land_capability, windsolarV)
# it works!! 
table(extraction$land_capability)
# 1  2  3  4  5  6  7  8  9 
# 16 54 53 44 39 38 14  3  1 

# interestingly most facilities located on relatively decent agricultural land
# though only 16 on the best category
# probably because developments are targeting already cleared lands near 
# population centres (where transmission is)

# join back onto data (all in same order, have checked)
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::select(-ID)
rm(extraction, land_capability)

# climate density graphs ----

# below adapted from Will's code, took out of function so can retain
# climate data with wind/solar data
# MIGHT HAVE TO REWRITE TO SEPARATE INTO PREP/FIGURE

# load rasters of mean annual temperature and rainfall (crs WGS1984, same as windsolar df)
temp_raster <- terra::rast("data_cache/CHELSA_bio1_1981-2010_V.2.1.tif")
rain_raster <- terra::rast("data_cache/CHELSA_bio12_1981-2010_V.2.1.tif")

# Load the region polygon
region_polygon <- rnaturalearth::ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

# convert region to SpatVector for terra operations
region_polygon <- terra::vect(region_polygon)

# crop rasters to the region extent
temp_raster <- terra::crop(temp_raster, terra::ext(region_polygon))
rain_raster <- terra::crop(rain_raster, terra::ext(region_polygon))

# Mask the rasters with the region shape
temp_raster <- terra::mask(temp_raster, region_polygon)
rain_raster <- terra::mask(rain_raster, region_polygon)
rm(region_polygon)

# Extract raster values at the points
windsolar_temp <- terra::extract(temp_raster, windsolar)
windsolar_rain <- terra::extract(rain_raster, windsolar)

# join these values back to windsolar df
# join back onto data
windsolar <- windsolar %>%
  dplyr::bind_cols(windsolar_temp) %>%
  dplyr::select(-ID) %>%
  dplyr::bind_cols(windsolar_rain) %>%
  dplyr::select(-ID)
rm(windsolar_temp, windsolar_rain)

# Convert the full rasters to a data frame
temp_all <- terra::as.data.frame(temp_raster)
rain_all <- terra::as.data.frame(rain_raster)
rm(temp_raster, rain_raster)
temp_all <- dplyr::mutate(temp_all, type = "All Australia")
rain_all <- dplyr::mutate(rain_all, type = "All Australia")
# append wind and solar data
temp_all <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(type, `CHELSA_bio1_1981-2010_V.2.1`) %>%
  dplyr::bind_rows(temp_all) %>%
  dplyr::rename(MAT = `CHELSA_bio1_1981-2010_V.2.1`)
rain_all <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(type, `CHELSA_bio12_1981-2010_V.2.1`) %>%
  dplyr::bind_rows(rain_all) %>%
  dplyr::rename(MAP = `CHELSA_bio12_1981-2010_V.2.1`)

# now create density plots of these data
# temperature first
temp_plot <- ggplot(temp_all, aes(x = MAT, colour = type, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean annual temperature (ºC)",
       y = "Density",
       colour = "",
       fill = "") +
  scale_fill_discrete(labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  scale_colour_discrete(labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  ggpubr::theme_pubr(legend = "right")

# then precipitation
rain_plot <- ggplot(rain_all, aes(x = MAP, colour = type, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean annual precipitation (mm)",
       y = "Density",
       colour = "",
       fill = "") +
  scale_x_log10() +
  ggpubr::theme_pubr(legend = "none")

# use patchwork package to stitch plots together, and export
temp_plot + rain_plot
ggsave("figures/temp_precip_density_plots.pdf", width = 9, height = 4.5)
# plots slow to draw because 10million+ points in density graph
rm(rain_plot, rain_all, temp_plot, temp_all)

# rename columns for future use
windsolar <- dplyr::rename(windsolar, MAT = `CHELSA_bio1_1981-2010_V.2.1`,
                           MAP = `CHELSA_bio12_1981-2010_V.2.1`)

# sankey graph showing pre1750 to today change in veg for solar locations? ----

# need to work out source (pre-1750 veg), target (extant veg), value (MW)
sankeylinks <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mvgpre1750_simplified, mvg_simplified, type, totalcapacity_mw) %>% 
  dplyr::group_by(mvgpre1750_simplified, mvg_simplified) %>% # leave out solar/wind distinction for now
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup()

# add dummy spaces in one column so sankey diagram won't lump categories
sankeylinks$mvgpre1750_simplified <- gsub("(^[A-Z][a-z]*[ ,\\/]?)", " \\1", sankeylinks$mvgpre1750_simplified)

# create nodes df for sankey
sankeynodes <- data.frame(
  name = c(as.character(sankeylinks$mvgpre1750_simplified), 
           as.character(sankeylinks$mvg_simplified)) %>% unique()
)

# reformat name to id for networkD3 connection
sankeylinks$IDsource <- match(sankeylinks$mvgpre1750_simplified, sankeynodes$name)-1
sankeylinks$IDtarget <- match(sankeylinks$mvg_simplified, sankeynodes$name)-1

# prepare colour scale: rename below with categories and hex codes once ready to make final graph
# i.e. once have lumped all vegetation categories
#my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

# Make the Network
p <- networkD3::sankeyNetwork(Links = sankeylinks, Nodes = sankeynodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "sumcapmw", NodeID = "name", 
                              sinksRight = FALSE)
p

# looks much better with simplified veg categories!

# TO DO - separate sankeys for wind and solar
#       - colour veg groups (and lines?) consistently
rm(p, sankeylinks, sankeynodes)

# LAI unfinished ----

# load in pre-summarised Leaf Area Index rasters from GLOBMAP (doi.org/10.5281/zenodo.12698637)
# for 2001-2003 and 2021-2023
# can then compare pre- and post- development LAI for wind/solar locations?
# theoretically should indicate change in density of vegetation
# may want to BUFFER POINTS to pick up area
# but have checked that all points are currently in the middle of the impact area
# which differs in magnitude for wind and solar - dif buffer sizes maybe?

mean_LAI_0103 <- terra::rast("data_output/tiff2001_2003.tif")
mean_LAI_2123 <- terra::rast("data_output/tiff2021_2023.tif")

# extract values (should buffer first but just trying it for now)
extraction <- terra::extract(mean_LAI_0103, windsolar)
hist(extraction$lyr.1) # mostly low LAI values
extraction2 <- terra::extract(mean_LAI_2123, windsolar)
hist(extraction2$lyr.1) # mostly low still, some higher

# join this data back to main df
names(extraction) <- c("ID", "mean_LAI_0103")
names(extraction2) <- c("ID", "mean_LAI_2123")
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::select(-ID) %>%
  dplyr::bind_cols(extraction2) %>%
  dplyr::select(-ID) %>%
  dplyr::mutate(difLAI = mean_LAI_2123 - mean_LAI_0103) # dif between LAI times?
rm(extraction, extraction2, mean_LAI_0103, mean_LAI_2123)

hist(windsolar$difLAI) # mostly close to 0, some -ve but most +ve

# TO DOs
#   - try buffering (prop to MW?) with leaf area index and Hansen forest cover mapping, from ~1998 (earliest start year) to most recent, to see change in cover
#   - cache all data as csv, make more graphs and a nice map to export

# write data output for now

readr::write_csv(windsolar, "data_output/windsolar_environments.csv")

})
