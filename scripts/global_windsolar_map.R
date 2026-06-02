# script to make a map from GIPT data for global locations of utility-scale
# wind and solar farms

# get data ----

# sources August 2025 data from the Global Integrated Power Tracker 
# filtered to projects >3 MW capacity that are onshore, and either operating, 
# in construction or retired (given systematic review studies go back decades)
source("scripts/gipt_data.R")

# map ----

# first tables out of curiosity
table(windsolar_gipt$region)
# Africa  Americas     Asia   Europe  Oceania 
# 618     8776        23841    16993      497 

table(windsolar_gipt$country_area)

# 435 wind and solar facilities for Australia - much more than my carefully curated data set

# make map of locations of study
# first get world map data without Antarctica (not included)
worldmap <- rnaturalearth::ne_countries() %>% 
  filter(sovereignt != "Antarctica")

# then get spatial version of GIPT data where lat/long available
windsolar_gipt_spat <- windsolar_gipt %>%
  dplyr::select(country_area, type, lat, lon) %>%
  dplyr::filter(!is.na(lat)) %>%
  dplyr::distinct() # get rid of duplicate points on same spot

# split data to prepare separate maps for wind and solar facilities
wind_gipt_spat <- windsolar_gipt_spat %>%
  dplyr::filter(type == "wind")

solar_gipt_spat <- windsolar_gipt_spat %>%
  dplyr::filter(type == "solar")

# wind power map
ggplot() +
  geom_sf(data = worldmap) +
  theme_void() +
  geom_point(data = wind_gipt_spat, aes(x = lon, y = lat), colour = "#377EB8", size = 0.5, alpha = 0.5)
ggsave("figures/global_map_wind_gipt.pdf", width = 6, height = 4)

# solar power map
ggplot() +
  geom_sf(data = worldmap) +
  theme_void() +
  geom_point(data = solar_gipt_spat, aes(x = lon, y = lat), colour = "#FACB0F", size = 0.5, alpha = 0.5)
ggsave("figures/global_map_solar_gipt.pdf", width = 6, height = 4)

rm(solar_gipt_spat, wind_gipt_spat)
