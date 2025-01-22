# script to map Australian wind and solar farms against Australian ecosystems
# and biodiversity e.g. bird species, bat species, mammal species

# get wind/solar location data ----

# TO DO
# load data for wind and solar farms across world from globalenergymonitor.org
# and filter down to just wind and solar farms in Australia above 10MW capacity
# compiling single data source
windsolar <- cache_csv("data_output/windsolar_aus_gipt.csv", function(){

# read in data on wind and solar farms from maps on Renew Economy website, converting from JSON format
windfarms <- jsonlite::fromJSON("data_input/tidy_reneweconomy_windfarms.json")

# some messy columns, build into data I want
windfarms_tidy <- windfarms %>%
  tidyr::unnest(cols = description) %>%
  dplyr::select(title, address, lat, long = lng, 
                size_mw = `Size:`,
                yr_operational = `Operational:`, 
                technology = `Technology:`,
                owner = `Owner:`,
                comments = `Comments:`)

# export these as csv to fill in few missing fields
readr::write_csv(windfarms_tidy, "data_output/windfarms_aus_fromjson.csv")

# and read back in (error checking not finished yet but will try mapping for now)
windfarms <- readr::read_csv("data_input/windfarms_aus_updated.csv")
})

# rewrite below to match new wind and solar data source

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
windfarms <- sf::st_as_sf(windfarms, coords = c("long", "lat"), remove = FALSE, crs = "EPSG:4326")
solarfarms <- sf::st_as_sf(solarfarms, coords = c("long", "lat"), remove = FALSE, crs = "EPSG:4326")
sf::st_crs(aus) <- "EPSG:4326"
sf::st_crs(biomes) <- "EPSG:4326"

# make map!
ggplot() +
  geom_sf(data = aus, fill = NA, linewidth = 0.3) +
  geom_sf(data = biomes, aes(fill = biome), linewidth = 0.1, alpha = 0.3) +
  geom_sf(data = windfarms, shape = 1, colour = "blue") +
  geom_sf(data = solarfarms, shape = 2, colour = "yellow") +
  theme_void()
ggsave("figures/map_windsolar_aus_biomes.png", width = 8, height = 7)

# extract biome for each wind and solar location
wind_biome <- sf::st_join(windfarms, biomes) %>%
  dplyr::distinct() %>%
  dplyr::mutate(facility = "wind")
solar_biome <- sf::st_join(solarfarms, biomes) %>%
  dplyr::distinct() %>%
  dplyr::mutate(facility = "solar")

# simplify to a df of biome with #s of wind and solar facilities in them
windsolar <- wind_biome %>%
  dplyr::bind_rows(solar_biome) 

biome_windsolarsummary <- windsolar %>%
  dplyr::select(biome, facility) %>%
  dplyr::group_by(biome, facility) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
biome_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(biome = forcats::fct_inorder(biome)) %>%
  ggplot(aes(x = biome, y = number, fill = facility)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "YlGnBu") +
  xlab("Biome") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_biome.png", width = 9, height = 7)
# interesting, has changed a bit with addition of renew economy data. 

# enough of biomes, let's use better veg mapping
# clean up df not needed anymore
rm(biome_windsolarsummary, biomes, solar_biome, wind_biome, windfarms, solarfarms)

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
# first vectorise wind and solar farm points
windsolarV <- terra::vect(windsolar)
# re-project into raster's Albers equal area projection
windsolarV <- terra::project(windsolarV, terra::crs(nvis_extant))

# extract values
extraction <- terra::extract(nvis_extant, windsolarV)
# it works!! 
table(extraction$`nvis-mvg_1`)
# 2   3   4   5   7   9  11  14  15  18  19  20  21  22  25  99 
# 3   6   1  13   1   1   1   2   1   2   2   3   2   5 152   1
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
  dplyr::select(mvg_name, facility) %>%
  dplyr::group_by(mvg_name, facility) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvg_name = forcats::fct_inorder(mvg_name)) %>%
  ggplot(aes(x = mvg_name, y = number, fill = facility)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "YlGnBu") +
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
# 1  2  3  5  6  7  8  9 11 12 13 14 15 16 17 18 19 20 21 22 26 31 32 
# 3  2 16 81  9  1  3  2 15  3  1  9  2  1  9  1 16  2  5  6  1  1  7 

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
  dplyr::select(mvgpre1750_name, facility) %>%
  dplyr::group_by(mvgpre1750_name, facility) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvgpre1750_name = forcats::fct_inorder(mvgpre1750_name)) %>%
  ggplot(aes(x = mvgpre1750_name, y = number, fill = facility)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "YlGnBu") +
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
# 14 38 41 34 24 34  9  1  1 

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
  dplyr::select(land_capability, facility) %>%
  dplyr::group_by(land_capability, facility) %>%
  dplyr::summarise(number = n()) %>%
  dplyr::ungroup()

# now display in stacked bar chart
landcap_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  ggplot(aes(x = land_capability, y = number, fill = facility)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "YlGnBu") +
  xlab("Agricultural land capability") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_landcapability.png", width = 9, height = 7)

# TO DOs
#   - fix y axis labels
#   - cache all data as csv, make more graphs and a nice map to export
#   - work out spatial resolution needed for insights
#   - combine similar vegetation groups (e.g. eucalypt open forests, eucalypt woodlands)???


