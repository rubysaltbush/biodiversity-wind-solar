# script to read in and compare data for wind and solar facility locations and size
# from multiple data sources across Australia

# first, the NEM (only southeast Aus but government data)
#### NEM data for southeast (less complete) ----

# read in data from NEM on locations of wind and solar farms
wind_nem <- readr::read_csv("data_input/wind_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon) %>%
  dplyr::mutate(max_cap_mw = as.numeric(`Max Cap (MW)`))
# manually fix missing value in max capacity (mw)
wind_nem$max_cap_mw[is.na(wind_nem$max_cap_mw)] <- 165.5
# data includes wind farms with 3MW or greater capacity, make this my standard?
  
#dplyr::filter(!(is.na(lat))) # 2 missing coordinates, could patch but remove for now

solar_nem <- readr::read_csv("data_input/solar_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon) %>%
  dplyr::filter(`Max Cap (MW)` >= 3) # remove any facilities with maximum capacity less than 3 MW
#dplyr::filter(!(is.na(lat))) # 7 missing coordinates, could patch but remove for now

# now, globalenergymonitor.org
#### Global Integrated Power Tracker ----

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
# okay this gives 131 rows, most so far although separate rows include different 
# phases of same projects)  

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

# quick comparison - MW capacity of each energy type, currently operating

# National Energy Market data
sum(wind_nem$max_cap_mw)
# [1] 10136.5
sum(solar_nem$`Max Cap (MW)`)
# [1] 7204.26


# Global Integrated Power Tracker data
sum(wind_gipt$`Capacity (MW)`)
# [1] 15318.2
sum(solar_gipt$`Capacity (MW)`)
# [1] 11547.3


# for now the GIPT data seems most reliable and doesn't seem to be missing too much
# if we decide we want a more complete data set I could update these all but that
# is probably unnecessary for our purposes for now

rm(solar_gipt, solar_nem, wind_gipt, wind_nem)
