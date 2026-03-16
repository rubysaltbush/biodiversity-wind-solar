# script to read in and process GIPT data on global wind and solar facilities

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
  dplyr::filter(Status %in% c("operating", "construction", "retired")) # exclude projects still in planning stages or shelved/mothballed, but keep retired projects given studies go back decades
rm(wind_gipt1, wind_gipt2)  
# separate rows include different phases of same projects, not a problem for global map
# 19143 rows

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
  dplyr::filter(Status %in% c("operating", "construction", "retired")) # exclude projects still in planning stages, but keep retired projects given studies go back decades
rm(solar_gipt1, solar_gipt2)  
# 31582 after filtering, some projects on multiple rows

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
# 50725 total
rm(solar_gipt, wind_gipt)
