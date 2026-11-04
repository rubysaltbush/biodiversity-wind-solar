# script to prepare and cache data for spatial analysis, of locations of Australian
# wind and solar farms with environmental data including climate and vegetation

# cache csv so it can be loaded without rerunning all extractions, especially 
# given large spatial files too big to share on github

windsolar <- cache_csv("data_output/windsolar_environments.csv", function(){

# get wind/solar location data ----

  # FIRST STEP
# load data for wind and solar farms across world from globalenergymonitor.org
# and filter down to just wind and solar farms in Australia of 3MW+ capacity
# compiling single data source
windsolar <- cache_csv("data_output/windsolar_gipt_aus.csv", function(){
  
  # June 2024 releases of the Global Wind Power Tracker and Global Solar Power
  # Tracker (Global Energy Monitor), downloaded January 2025
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

## present day 2024 ----

# load geotiff raster version of national veg mapping, NVIS, in Albers equal projection
# NVIS is version 7 released 2024
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
# can now drop sf geometry from windsolar df
windsolar <- sf::st_drop_geometry(windsolar)
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

# load rasters of mean annual temperature and rainfall (crs WGS1984, same as windsolar df)
temp_raster <- terra::rast("data_cache/CHELSA_bio1_1981-2010_V.2.1.tif")
temp_raster
# class       : SpatRaster 
# dimensions  : 20880, 43200, 1  (nrow, ncol, nlyr)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : -180.0001, 179.9999, -90.00014, 83.99986  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source      : CHELSA_bio1_1981-2010_V.2.1.tif 
# name        : CHELSA_bio1_1981-2010_V.2.1 
rain_raster <- terra::rast("data_cache/CHELSA_bio12_1981-2010_V.2.1.tif")
rain_raster
# class       : SpatRaster 
# dimensions  : 20880, 43200, 1  (nrow, ncol, nlyr)
# resolution  : 0.008333333, 0.008333333  (x, y)
# extent      : -180.0001, 179.9999, -90.00014, 83.99986  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source      : CHELSA_bio12_1981-2010_V.2.1.tif 
# name        : CHELSA_bio12_1981-2010_V.2.1 

# check projections all similar
windsolarV <- terra::project(windsolarV, terra::crs(temp_raster))

# Extract raster values at the points
windsolar_temp <- terra::extract(temp_raster, windsolarV)
windsolar_rain <- terra::extract(rain_raster, windsolarV)

# join these values back to windsolar df
# join back onto data
windsolar <- windsolar %>%
  dplyr::bind_cols(windsolar_temp) %>%
  dplyr::select(-ID) %>%
  dplyr::bind_cols(windsolar_rain) %>%
  dplyr::select(-ID)
rm(windsolar_temp, windsolar_rain, rain_raster, temp_raster)

# rename columns for future use
windsolar <- dplyr::rename(windsolar, MAT = `CHELSA_bio1_1981-2010_V.2.1`,
                           MAP = `CHELSA_bio12_1981-2010_V.2.1`)

# Hansen forest cover ----

## forest loss year ----
lossyear <- terra::rast("data_cache/Hansen_GFC_lossyear_aus.tif")

# now create buffer of points
windsolarV <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
# buffer points by 1km, in Australian Albers (EPSG:3577) so distance is in metres
# whatever the s2 setting (exactextractr reprojects buffers to the raster CRS)
windsolarVbuff <- windsolarV %>%
  sf::st_transform(3577) %>%
  sf::st_buffer(dist = 1000)

# now extract fraction of 0 (no clearing) and 00-23 (year cleared) values per 
# buffer area from tree cover loss data
fraclossyear <- exactextractr::exact_extract(lossyear, windsolarVbuff, 
                                             fun = "frac")

# also extract loss year for exact point
lossyearpoint <- terra::extract(lossyear, windsolarV)

# join outputs to OG wind/solar data
fraclossyear <- dplyr::select(fraclossyear, frac_uncleared = frac_0) # don't think I want fractions for all other years of clearing at this point
windsolar <- windsolar %>%
  dplyr::bind_cols(lossyearpoint) %>%
  dplyr::select(-ID) %>%
  dplyr::rename(yr_pt_cleared = Layer_1) %>%
  dplyr::bind_cols(fraclossyear)

rm(fraclossyear, lossyear, lossyearpoint)

## tree cover in year 2000 ----

treecover <- terra::rast("data_cache/Hansen_GFC_treecover2000_aus.tif")

# now extract mean percent tree cover in buffer area from year 2000
meantreecoverbuff <- exactextractr::exact_extract(treecover, windsolarVbuff, 
                                                  fun = "mean")
# also extract points
meantreecoverpt <- terra::extract(treecover, windsolarV)

# join output to OG data
windsolar$meantreecover2000_buff <- meantreecoverbuff
windsolar$meantreecover2000_pt <- meantreecoverpt$`Hansen_GFC-2023-v1.11_treecover2000_10S_110E`

rm(meantreecoverpt, meantreecoverbuff, treecover, windsolarV, windsolarVbuff)

# footprints ----
# match wind and solar locations to footprints from Global Renewables Watch
# (Robinson et al., 2025): solar array polygons and individual wind turbines

windsolar <- dplyr::ungroup(windsolar)
windsolar$fac_id <- seq_len(nrow(windsolar))

# facility locations in Australian Albers (metres)
facilities <- windsolar %>%
  dplyr::select(fac_id, type, lat, lon) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
  sf::st_transform(3577)

# read Global Renewables Watch footprints for Australia
grw_solar <- sf::st_read("data_cache/solar_all_2024q2_v1.gpkg",
                         query = "SELECT * FROM solar_all_2024q2 WHERE COUNTRY = 'Australia'") %>%
  sf::st_transform(3577)
grw_wind <- sf::st_read("data_cache/wind_all_2024q2_v1.gpkg",
                        query = "SELECT * FROM wind_all_2024q2 WHERE COUNTRY = 'Australia'") %>%
  sf::st_transform(3577)
# 434 solar polygons, 4038 turbines

# assign each footprint feature to the nearest facility, keeping those within
# a maximum distance (m)
match_footprints <- function(grw, fac, max_dist) {
  nearest <- sf::st_nearest_feature(grw, fac)
  grw %>%
    dplyr::mutate(fac_id = fac$fac_id[nearest],
                  match_dist_m = as.numeric(sf::st_distance(grw, fac[nearest, ],
                                                            by_element = TRUE))) %>%
    dplyr::filter(match_dist_m <= max_dist) %>%
    dplyr::select(fac_id, match_dist_m)
}

# solar polygons within 2 km of a solar farm, and a 50 m radius (turbine pad
# and surrounds) around turbines within 15 km of a wind farm
footprints <- rbind(
  match_footprints(grw_solar, dplyr::filter(facilities, type == "solar"), 2000),
  sf::st_buffer(match_footprints(grw_wind, dplyr::filter(facilities, type == "wind"), 15000), 50)
)
rm(grw_solar, grw_wind, match_footprints)

# number of footprint features and furthest match distance per facility
windsolar <- footprints %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(fac_id) %>%
  dplyr::summarise(n_footprint_features = dplyr::n(),
                   max_match_dist_m = max(match_dist_m)) %>%
  dplyr::right_join(windsolar, by = "fac_id") %>%
  dplyr::arrange(fac_id) %>%
  dplyr::mutate(footprint_source = dplyr::if_else(is.na(n_footprint_features), "point", "GRW"))

# how many points from my error-checked locations are matched by footprints in GRW data?
table(windsolar$type, windsolar$footprint_source)
#        GRW point
# solar  132    17
# wind   100    13

# impact area of each facility: GRW footprints, or a 1 km radius around the
# point location for facilities without a GRW footprint
footprint_areas <- rbind(
  footprints %>% dplyr::select(fac_id),
  facilities %>%
    dplyr::filter(!fac_id %in% footprints$fac_id) %>%
    sf::st_buffer(1000) %>%
    dplyr::select(fac_id) %>%
    sf::st_set_geometry("geom")
) %>%
  dplyr::group_by(fac_id) %>%
  dplyr::summarise() %>%
  dplyr::mutate(footprint_ha = as.numeric(sf::st_area(.)) / 10000)
rm(footprints, facilities)

# fraction of each footprint in each class of present-day and pre-1750 vegetation
# (NVIS Major Vegetation Groups) and agricultural land capability
categorical <- list(
  mvg = terra::rast("data_cache/NVIS_MVG_07_tif/nvis-mvg.tif")[[1]],
  mvgpre1750 = terra::rast("data_cache/NVIS_V7_0_AUST_RASTERS_PRE_ALL_GEOTIFF/nvis7_0_aust_pre_mvg_alb.tif")[[1]],
  land_capability = terra::rast("data_cache/RD-LANDCAPAUS-02/land_capability.tif")
)

footprint_classes <- purrr::imap_dfr(categorical, function(r, layer) {
  exactextractr::exact_extract(r, footprint_areas, "frac", append_cols = "fac_id") %>%
    tidyr::pivot_longer(-fac_id, names_to = "class", names_prefix = "frac_",
                        values_to = "fraction") %>%
    dplyr::filter(fraction > 0) %>%
    dplyr::mutate(layer = layer, class = as.numeric(class))
})
rm(categorical)

# mean tree cover in 2000, and fraction of footprint area with tree cover loss
# 2000-2023 (Hansen et al., 2013)
footprint_areas$treecover2000_fp <- exactextractr::exact_extract(
  terra::rast("data_cache/Hansen_GFC_treecover2000_aus.tif"), footprint_areas, "mean")
footprint_areas$frac_treeloss_fp <- exactextractr::exact_extract(
  terra::rast("data_cache/Hansen_GFC_lossyear_aus.tif"), footprint_areas,
  function(values, coverage_fraction) sum(coverage_fraction[values > 0]) / sum(coverage_fraction))

# majority class of each layer per footprint, with simplified vegetation names
mvgcodes <- readr::read_csv("data_input/mvg_codes_names.csv")

footprint_majority <- footprint_classes %>%
  dplyr::group_by(fac_id, layer) %>%
  dplyr::slice_max(fraction, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(fac_id, layer, class) %>%
  tidyr::pivot_wider(names_from = layer, values_from = class, names_glue = "{layer}_fp") %>%
  dplyr::left_join(dplyr::select(mvgcodes, mvg_fp = mvg_code, mvg_fp_simplified = mvg_simplified),
                   by = "mvg_fp") %>%
  dplyr::left_join(dplyr::select(mvgcodes, mvgpre1750_fp = mvg_code,
                                 mvgpre1750_fp_simplified = mvg_simplified),
                   by = "mvgpre1750_fp")

# fraction of each footprint on cleared land (NVIS MVG 25)
footprint_cleared <- footprint_classes %>%
  dplyr::filter(layer == "mvg", class == 25) %>%
  dplyr::select(fac_id, frac_cleared_fp = fraction)
# BUT looking at these there seems to be a decent error rate in NVIS cleared land category
# so maybe won't use frac_cleared_fp

# join footprint environments to wind and solar data
windsolar <- windsolar %>%
  dplyr::left_join(sf::st_drop_geometry(footprint_areas), by = "fac_id") %>%
  dplyr::left_join(footprint_majority, by = "fac_id") %>%
  dplyr::left_join(footprint_cleared, by = "fac_id") %>%
  dplyr::mutate(frac_cleared_fp = tidyr::replace_na(frac_cleared_fp, 0))
rm(footprint_areas, footprint_majority, footprint_cleared)

# export facilities to check against satellite imagery: implausible footprint
# area or number of turbines for their capacity, or operating with no footprint
windsolar %>%
  dplyr::mutate(ha_per_mw = footprint_ha / totalcapacity_mw,
                mw_per_turbine = totalcapacity_mw / n_footprint_features,
                check_reason = dplyr::case_when(
                  footprint_source == "GRW" & type == "solar" &
                    (ha_per_mw < 0.5 | ha_per_mw > 5) ~ "ha per MW outside 0.5-5",
                  footprint_source == "GRW" & type == "wind" &
                    (mw_per_turbine < 1 | mw_per_turbine > 7) ~ "MW per turbine outside 1-7",
                  footprint_source == "point" & status == "operating" ~ "operating, no footprint matched")) %>%
  dplyr::filter(!is.na(check_reason)) %>%
  dplyr::select(fac_id, project_name, type, status, totalcapacity_mw, earliest_start,
                lat, lon, n_footprint_features, footprint_ha, max_match_dist_m,
                footprint_source, ha_per_mw, mw_per_turbine, check_reason) %>%
  readr::write_csv("data_output/footprint_outliers_to_check.csv")

# have exported possible mismatches between my point locations and GRW data - the 30
# facilities with points but no GRW footprint are a mix of recently constructed 
# facilities (so possibly not visible in mid-2024 when GRW data from) or projects
# that GRW's machine learning must have missed for whatever reason - still better
# to have proper footprints for most facilities, and will use a buffer around the 
# point for these 30 facilities where no footprint easily available

# add facility type, footprint source, simplified vegetation names and area (ha)
# to footprint classes, and save
footprint_classes <- footprint_classes %>%
  dplyr::left_join(dplyr::select(windsolar, fac_id, type, footprint_source, footprint_ha),
                   by = "fac_id") %>%
  dplyr::left_join(dplyr::select(mvgcodes, class = mvg_code, mvg_simplified), by = "class") %>%
  dplyr::mutate(class_name = dplyr::if_else(layer == "land_capability",
                                            as.character(class), mvg_simplified),
                area_ha = fraction * footprint_ha) %>%
  dplyr::select(fac_id, type, footprint_source, layer, class, class_name, fraction, area_ha)
readr::write_csv(footprint_classes, "data_output/footprint_classes.csv")
rm(footprint_classes, mvgcodes)

# write data output for now
readr::write_csv(windsolar, "data_output/windsolar_environments.csv")

})

# fraction and area of each facility footprint in each class of present-day and
# pre-1750 vegetation and land capability (several rows per facility)
footprint_classes <- readr::read_csv("data_output/footprint_classes.csv")
