# script to make a map from GIPT data for global locations of utility-scale
# wind and solar farms

# get data ----
source("scripts/gipt_data.R")

# map ----

# first tables out of curiosity
table(windsolar_gipt$region)
# Africa  Americas     Asia   Europe  Oceania 
# 618     8776        23841    16993      497 

table(windsolar_gipt$country_area)

# Australia 435?

# make map of locations of study
# first get world map data without Antarctica (not included)
worldmap <- rnaturalearth::ne_countries() %>% 
  filter(sovereignt != "Antarctica")

# then get spatial version of sys review data where lat/long available
windsolar_gipt_spat <- windsolar_gipt %>%
  dplyr::select(country_area, type, lat, lon) %>%
  dplyr::filter(!is.na(lat)) %>%
  dplyr::distinct() # get rid of duplicate points on same spot

# then plot background world map and points coloured by wind or solar
ggplot() +
  geom_sf(data = worldmap) +
  theme_void() +
  geom_point(data = windsolar_gipt_spat, aes(x = lon, y = lat, colour = type), alpha = 0.4, size = 0.5) +
  labs(colour = "Facility type") +
  scale_colour_discrete(type = my_colours$windsolar)
ggsave("figures/global_map_windsolar_gipt.pdf", width = 6, height = 4)

# very overlapping, two separate maps might be better
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

rm(solar_gipt_spat, wind_gipt_spat, windsolar_gipt, windsolar_gipt_spat, worldmap)
