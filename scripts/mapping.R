# script to map Australian wind and solar farms against Australian ecosystems
# and biodiversity e.g. bird species, bat species, mammal species

# get wind/solar location data ----

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

# write combined data to cache
readr::write_csv(windsolar, "data_output/windsolar_gipt_aus.csv")
})

# now summarise data so that each row represents a single project and location
windsolar <- windsolar %>%
  dplyr::group_by(project_name, project_other_names, type, status, 
                  operator, owner, lat, lon, state) %>%
  dplyr::summarise(phase_names = paste0(phase_name, collapse = ","),
                   totalcapacity_mw = sum(capacity_mw),
                   start_years = paste0(start_year, collapse = ","))
windsolar$project_name[duplicated(windsolar$project_name)]
# collapses 17 rows, still 18 projects on multiple rows
length(unique(windsolar$project_name))
# 246 unique projects

# need background Aus map
# read in shapefile of Aus outline
aus <- terra::vect("data_input/Aus_outline.shp")
# bit slow and over complex, simplify to make faster
aus <- terra::simplifyGeom(aus, tolerance = 0.01)
# crop to remove Norfolk and Christmas Islands from outline
aus <- terra::crop(aus, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# convert aus to sf for easy plotting
aus <- sf::st_as_sf(aus)

# okay now for biome map
biomes <- sf::st_read("data_input/ibra7_biomes_background.shp")

# make sure all crs the same
windsolar <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
sf::st_crs(aus) <- "EPSG:4326"
sf::st_crs(biomes) <- "EPSG:4326"

# make map!
ggplot() +
  geom_sf(data = aus, fill = NA, linewidth = 0.1) +
  geom_sf(data = biomes, aes(fill = biome), linewidth = 0.01, alpha = 0.3) +
  scale_fill_brewer(palette = "Set1") +
  geom_sf(data = windsolar, aes(shape = type, colour = type, size = totalcapacity_mw)) +
  scale_colour_brewer(palette = "Set1", name = "facility type") +
  scale_shape(solid = FALSE, name = "facility type") +
  scale_size(name = "total capacity (mw)") +
  theme_void()
ggsave("figures/map_windsolar_aus_biomes.png", width = 8, height = 7)

# extract biome for each wind and solar location
windsolar <- sf::st_join(windsolar, biomes) %>%
  dplyr::distinct()

# simplify to a df of biome with #s of wind and solar facilities in them
windsolar_biomesummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(biome, type) %>%
  dplyr::group_by(biome, type) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()
windsolar_biomesummary
# biome                                                        type  number
# <chr>                                                        <chr>  <int>
#   1 Deserts and Xeric Shrublands                                 solar     24
# 2 Deserts and Xeric Shrublands                                 wind      15
# 3 Mediterranean Forests, Woodlands and Scrub                   solar     20
# 4 Mediterranean Forests, Woodlands and Scrub                   wind      28
# 5 Temperate Broadleaf and Mixed Forest                         solar     42
# 6 Temperate Broadleaf and Mixed Forest                         wind      64
# 7 Temperate Grasslands, Savannas and Shrublands                solar     20
# 8 Tropical and Subtropical Grasslands, Savannas and Shrublands solar     46
# 9 Tropical and Subtropical Grasslands, Savannas and Shrublands wind       7
# 10 Tropical and Subtropical Moist Broadleaf Forests             wind       2

# now display in stacked bar chart
windsolar_biomesummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(biome = forcats::fct_inorder(biome)) %>%
  ggplot(aes(x = biome, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Biome") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_biome.png", width = 9, height = 7)
# interesting that temperate forest is most represented biome
# given deserts is our biggest biome by land area
# bias of population centres - could show this in data or just discuss it

# enough of biomes, let's use better veg mapping
# clean up df not needed anymore
rm(windsolar_biomesummary, biomes)

# NVIS ----

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
# 3   5   6   7   9  10  11  14  15  16  17  18  19  20  21  22  25  27  29  99 
# 6   9  10   7   1   1   3   2   1   2   1   4   3   2   2  16 193   2   2   1 
# mostly 25, the code for cleared land, which makes sense. woot woot.

# join back onto data (all in same order, have checked)
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::rename(mvg_code = `nvis-mvg_1`) %>%
  dplyr::select(-ID) %>%
  dplyr::left_join(mvgcodes, by = "mvg_code") # add on names for MVG codes
rm(extraction, nvis_extant)

# now we have data to play with, yay!

# simplify to a df of MVG with #s of wind and solar facilities in them
mvg_windsolarsummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mvg_name, type) %>%
  dplyr::group_by(mvg_name, type) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvg_name = forcats::fct_inorder(mvg_name)) %>%
  ggplot(aes(x = mvg_name, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Major Vegetation Group") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_mvg.png", width = 9, height = 7)

# now try same with pre-1750 mapping

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
# 1   2   3   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  21  22  27  31  32 
# 2   1  23 113  10   2   3   1   1  17   1   1  11   2  11   9   5  22   7  17   1   2   6 

# without cleared land considered the original ecosystems which wind and solar
# farms are located on seem more diverse, though eucalypt woodlands (5) has most

# rename mvgcodes columns
mvgcodes <- dplyr::rename(mvgcodes, mvgpre1750_code = mvg_code, 
                          mvgpre1750_name = mvg_name)

# join back onto data (all in same order, have checked)
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::rename(mvgpre1750_code = nvis7_0_aust_pre_mvg_alb_1) %>%
  dplyr::select(-ID) %>%
  dplyr::left_join(mvgcodes, by = "mvgpre1750_code") # add on names for MVG codes
# interesting that pre1750 and extant codes are different even for uncleared land
rm(extraction, mvgcodes, nvis_pre1750)

# simplify to a df of MVG with #s of wind and solar facilities in them
mvg_windsolarsummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mvgpre1750_name, type) %>%
  dplyr::group_by(mvgpre1750_name, type) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvgpre1750_name = forcats::fct_inorder(mvgpre1750_name)) %>%
  ggplot(aes(x = mvgpre1750_name, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Major Vegetation Group (pre-1750)") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_mvgpre1750.png", width = 9, height = 7)

rm(mvg_windsolarsummary)

# Adams & Engert 2023 land capability mapping ----

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
# 20 43 55 49 30 40 29  1  1 

# interestingly most facilities located on relatively decent agricultural land
# though only 14 on the best
# probably because developments are targeting mostly cleared lands

# join back onto data (all in same order, have checked)
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::select(-ID)
rm(extraction)

# simplify to a df of land capability with #s of wind and solar facilities in them
landcap_windsolarsummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(land_capability, type) %>%
  dplyr::group_by(land_capability, type) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
landcap_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  ggplot(aes(x = land_capability, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Agricultural land capability") +
  ylab("Number of facilities") +
  scale_x_continuous(breaks = c(1:9)) +
  coord_flip()
ggsave("figures/aus_facilities_by_landcapability.png", width = 9, height = 7)

# TO DOs
#   - REDO ALL GRAPHS AS MW NOT # FACILITIES
#   - cache all data as csv, make more graphs and a nice map to export
#   - work out spatial resolution needed for insights
#   - combine similar vegetation groups (e.g. eucalypt open forests, eucalypt woodlands)???

# write data output for now

readr::write_csv(windsolar, "data_output/windsolar_environments.csv")
