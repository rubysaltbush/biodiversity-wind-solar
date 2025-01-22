# script to read in and compare data fro wind and solar facility locations and size
# from multiple data sources

# first, the NEM (only southeast Aus but government data)
#### NEM data for southeast (less complete) ----

# read in data from NEM on locations of wind and solar farms
wind_nem <- readr::read_csv("data_input/wind_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon)# %>%
#dplyr::filter(!(is.na(lat))) # 2 missing coordinates, could patch but remove for now

solar_nem <- readr::read_csv("data_input/solar_nationalmap_20241203.csv") %>%
  dplyr::mutate(lat = Lat, lon = Lon) #%>%
#dplyr::filter(!(is.na(lat))) # 7 missing coordinates, could patch but remove for now

# then, RenewEconomy website
#### RenewEconomy ----

wind_re <- cache_csv("data_input/windfarms_aus_updated.csv", function(){
  
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

solar_re <- cache_csv("data_input/solarfarms_aus_updated.csv", function(){
  
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

# now, globalenergymonitor.org
#### Global Integrated Power Tracker ----

