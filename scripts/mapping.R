# script to map Australian wind and solar farms against Australian ecosystems
# and biodiversity e.g. bird species, bat species, mammal species

# get data ----

# read in data on wind and solar farms from maps on Renew Economy website, converting from JSON format
windfarms <- jsonlite::fromJSON("data_input/tidy_reneweconomy_windfarms.json")
solarfarms <- jsonlite::fromJSON("data_input/tidy_reneweconomy_solarfarms.json")

# some messy columns, build into data I want
windfarms_tidy <- windfarms %>%
  tidyr::unnest(cols = description) %>%
  dplyr::select(title, address, lat, long = lng, 
                size_mw = `Size:`,
                yr_operational = `Operational:`, 
                technology = `Technology:`,
                owner = `Owner:`,
                comments = `Comments:`)
solarfarms_tidy <- solarfarms %>%
  tidyr::unnest(cols = description) %>%
  dplyr::select(title, address, lat, long = lng, 
                size_mw = `Size:`,
                yr_operational = `Operational:`, 
                technology = `Technology:`,
                owner = `Owner:`,
                comments = `Comments:`)

# export these as csv to fill in few missing fields
readr::write_csv(windfarms_tidy, "data_output/windfarms_aus_fromjson.csv")
readr::write_csv(solarfarms_tidy, "data_output/solarfarms_aus_fromjson.csv")




# need background Aus map
# read in shapefile of Aus outline
aus <- terra::vect("data_input/Aus_outline.shp")
# bit slow and over complex, simplify to make faster
aus <- terra::simplifyGeom(aus, tolerance = 0.01)
# crop to remove Norfolk and Christmas Islands from outline
aus <- terra::crop(aus, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# convert aus to sf for easy plotting
aus <- sf::st_as_sf(aus)

# read in data from NEM on locations of wind and solar farms
wind <- readr::read_csv("data_input/wind_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon) %>%
  dplyr::filter(!(is.na(lat))) # 2 missing coordinates, could patch but remove for now

solar <- readr::read_csv("data_input/solar_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon) %>%
  dplyr::filter(!(is.na(lat))) # 7 missing coordinates, could patch but remove for now

# okay now for biome map
biomes <- sf::st_read("data_input/ibra7_biomes_background.shp")

# make sure all crs the same
wind <- sf::st_as_sf(wind, coords = c("lon", "lat"), crs = "EPSG:4326")
solar <- sf::st_as_sf(solar, coords = c("lon", "lat"), crs = "EPSG:4326")
sf::st_crs(aus) <- "EPSG:4326"
sf::st_crs(biomes) <- "EPSG:4326"

# make map!
ggplot() +
  geom_sf(data = aus, fill = NA, linewidth = 0.75) +
  geom_sf(data = biomes, aes(fill = biome), linewidth = 0.2) +
  geom_sf(data = wind, shape = 1, colour = "blue") +
  geom_sf(data = solar, shape = 2, colour = "yellow") +
  theme_void()

# extract biome for each wind and solar location
wind_biome <- sf::st_join(wind, biomes) %>%
  dplyr::distinct() %>%
  dplyr::mutate(facility = "wind")
solar_biome <- sf::st_join(solar, biomes) %>%
  dplyr::distinct() %>%
  dplyr::mutate(facility = "solar")

# hack for now
wind_biome$`Max Cap (MW)` <- as.numeric(wind_biome$`Max Cap (MW)`)
wind_biome$`Reg Cap (MW)` <- as.numeric(wind_biome$`Reg Cap (MW)`)

# simplify to a df of biome with #s of wind and solar facilities in them
biome_windsolar <- wind_biome %>%
  dplyr::bind_rows(solar_biome) 

biome_windsolarsummary <- biome_windsolar %>%
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
ggsave("figures/aus_facilities_by_biome.png", width = 8, height = 7)

# that will do for now
rm(aus, biome_windsolar, biome_windsolarsummary, biomes, solar, solar_biome, wind, wind_biome)
