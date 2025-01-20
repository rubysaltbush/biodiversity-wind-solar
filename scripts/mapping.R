# script to map Australian wind and solar farms against Australian ecosystems
# and biodiversity e.g. bird species, bat species, mammal species

# get data ----

# current wind/solar data are lat/longs ripped from Renew Economy website, need to:
# 1) verify lat/longs accurate and e.g. not on adjacent roads
# 2) tidy up what is currently mess of constructed and planned projects
windfarms <- cache_csv("data_input/windfarms_aus_updated.csv", function(){

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

solarfarms <- cache_csv("data_input/solarfarms_aus_updated.csv", function(){
  
# read in data on wind and solar farms from maps on Renew Economy website, converting from JSON format
solarfarms <- jsonlite::fromJSON("data_input/tidy_reneweconomy_solarfarms.json")

# some messy columns, build into data I want
solarfarms_tidy <- solarfarms %>%
  tidyr::unnest(cols = description) %>%
  dplyr::select(title, address, lat, long = lng, 
                size_mw = `Size:`,
                yr_operational = `Operational:`, 
                technology = `Technology:`,
                owner = `Owner:`,
                comments = `Comments:`)

# export these as csv to fill in few missing fields
readr::write_csv(solarfarms_tidy, "data_output/solarfarms_aus_fromjson.csv")

# and read back in (error checking not finished yet but will try mapping for now)
solarfarms <- readr::read_csv("data_input/solarfarms_aus_updated.csv")
})

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
rm(extraction, mvgcodes, windsolarV, nvis_extant)

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

#### NEM data for southeast (less complete) ----

# read in data from NEM on locations of wind and solar farms
wind <- readr::read_csv("data_input/wind_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon)# %>%
#dplyr::filter(!(is.na(lat))) # 2 missing coordinates, could patch but remove for now

solar <- readr::read_csv("data_input/solar_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon) #%>%
#dplyr::filter(!(is.na(lat))) # 7 missing coordinates, could patch but remove for now
