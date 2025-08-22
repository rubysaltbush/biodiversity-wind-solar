# script to make a map from GIPT data for global locations of utility-scale
# wind and solar farms

# get data ----
# first read in data from GIPT and filter

#* wind ----
# data downloaded from Global Integrated Power Tracker August 2025
# read in two tabs of spreadsheet, 2nd sheet has all >=10MW projects
wind_gipt1 <- readxl::read_xlsx("data_input/Global-Wind-Power-Tracker-February-2025.xlsx",
                                sheet = 2)
# read in two tabs of spreadsheet, 3rd sheet has all <10MW projects
wind_gipt2 <- readxl::read_xlsx("data_input/Global-Wind-Power-Tracker-February-2025.xlsx",
                                sheet = 3)

# bind sheets together
wind_gipt <- wind_gipt1 %>%
  dplyr::bind_rows(wind_gipt2) %>% #31454 all together
  dplyr::filter(`Capacity (MW)` >= 3) %>% # just those greater than 3 MW (30754)
  dplyr::filter(`Installation Type` == "Onshore") %>% # only onshore wind (28666)
  dplyr::filter(Status %in% c("operating", "construction", "shelved", "mothballed", "retired")) # exclude projects still in planning stages, but keep retired projects given studies go back decades
rm(wind_gipt1, wind_gipt2)  
# separate rows include different phases of same projects, not a problem for global map
# 19606 rows

#* solar ----
# read in two tabs of spreadsheet, first with >20MW projects
solar_gipt1 <- readxl::read_xlsx("data_input/Global-Solar-Power-Tracker-February-2025.xlsx",
                                 sheet = 2)
# and 2nd with <20MW projects
solar_gipt2 <- readxl::read_xlsx("data_input/Global-Solar-Power-Tracker-February-2025.xlsx",
                                 sheet = 3)

#*combine ----
# bind sheets together
solar_gipt <- solar_gipt1 %>%
  dplyr::bind_rows(solar_gipt2) %>% #75689 total
  dplyr::filter(`Capacity (MW)` >= 3) %>% # just those greater than 3 MW (46095)
  dplyr::filter(Status %in% c("operating", "construction", "shelved", "mothballed", "retired")) # exclude projects still in planning stages, but keep retired projects given studies go back decades
rm(solar_gipt1, solar_gipt2)  
# 31897 after filtering, some projects on multiple rows

# select and rename columns
wind_gipt <- wind_gipt %>%
  dplyr::select(date_last_researched = `Date Last Researched`, 
                country_area = `Country/Area`,
                project_name = `Project Name`,
                phase_name = `Phase Name`,
                project_other_names = `Other Name(s)`,
                capacity_mw = `Capacity (MW)`,
                status = Status,
                start_year = `Start year`,
                retired_year = `Retired year`,
                operator = Operator,
                owner = Owner,
                lat = Latitude,
                lon = Longitude,
                location_accuracy = `Location accuracy`,
                city = City,
                state = `State/Province`,
                region = Region
  ) %>%
  dplyr::mutate(type = "wind")

solar_gipt <- solar_gipt %>%
  dplyr::select(date_last_researched = `Date Last Researched`, 
                country_area = `Country/Area`,
                project_name = `Project Name`,
                phase_name = `Phase Name`,
                project_other_names = `Other Name(s)`,
                capacity_mw = `Capacity (MW)`,
                status = Status,
                start_year = `Start year`,
                retired_year = `Retired year`,
                operator = Operator,
                owner = Owner,
                lat = Latitude,
                lon = Longitude,
                location_accuracy = `Location accuracy`,
                city = City,
                state = `State/Province`,
                region = Region
  ) %>%
  dplyr::mutate(type = "solar")

# numbers in lat lon stored as text in solar_gipt for some reason
solar_gipt$lat <- as.numeric(solar_gipt$lat)
solar_gipt$lon <- as.numeric(solar_gipt$lon)

windsolar_gipt <- wind_gipt %>%
  dplyr::bind_rows(solar_gipt)
rm(solar_gipt, wind_gipt)

# map ----

# first tables out of curiosity
table(windsolar_gipt$region)
# Africa Americas     Asia   Europe  Oceania 
# 666     8849    23970    17500      518 

table(windsolar_gipt$country_area)

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
  geom_point(data = windsolar_gipt_spat, aes(x = lon, y = lat, colour = type)) +
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
  geom_point(data = wind_gipt_spat, aes(x = lon, y = lat), colour = "#377EB8")
ggsave("figures/global_map_wind_gipt.pdf", width = 6, height = 4)

# solar power map
ggplot() +
  geom_sf(data = worldmap) +
  theme_void() +
  geom_point(data = solar_gipt_spat, aes(x = lon, y = lat), colour = "#FACB0F")
ggsave("figures/global_map_solar_gipt.pdf", width = 6, height = 4)

rm(solar_gipt_spat, wind_gipt_spat, windsolar_gipt, windsolar_gipt_spat, worldmap)
