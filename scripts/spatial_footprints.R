# script to match Australian wind and solar facility locations to footprints from
# Global Renewables Watch (Robinson et al., 2025): solar array polygons and
# individual wind turbine locations

# facility locations from spatial_dataprep.R, in Australian Albers (metres)
facilities <- windsolar %>%
  dplyr::mutate(fac_id = dplyr::row_number()) %>%
  sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326") %>%
  sf::st_transform(3577)

# read Global Renewables Watch footprints for Australia
grw_solar <- sf::st_read("data_cache/solar_all_2024q2_v1.gpkg",
                         query = "SELECT * FROM solar_all_2024q2 WHERE COUNTRY = 'Australia'") %>%
  sf::st_transform(3577)
grw_wind <- sf::st_read("data_cache/wind_all_2024q2_v1.gpkg",
                        query = "SELECT * FROM wind_all_2024q2 WHERE COUNTRY = 'Australia'") %>%
  sf::st_transform(3577)
# 434 solar polygons, 4038 turbines

# assign each footprint feature to the nearest facility, keeping those within
# a maximum distance (m)
match_footprints <- function(grw, fac, max_dist) {
  nearest <- sf::st_nearest_feature(grw, fac)
  grw %>%
    dplyr::mutate(fac_id = fac$fac_id[nearest],
                  match_dist_m = as.numeric(sf::st_distance(grw, fac[nearest, ],
                                                            by_element = TRUE))) %>%
    dplyr::filter(match_dist_m <= max_dist)
}

# solar polygons within 2 km of a solar farm, turbines within 15 km of a wind farm
footprints_solar <- match_footprints(grw_solar, dplyr::filter(facilities, type == "solar"), 2000)
footprints_wind <- match_footprints(grw_wind, dplyr::filter(facilities, type == "wind"), 15000)
rm(grw_solar, grw_wind)

# summarise footprints per facility
footprint_summary <- dplyr::bind_rows(
  footprints_solar %>%
    dplyr::mutate(area_ha = as.numeric(sf::st_area(.)) / 10000) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(fac_id) %>%
    dplyr::summarise(n_features = dplyr::n(), area_ha = sum(area_ha),
                     max_match_dist_m = max(match_dist_m)),
  footprints_wind %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(fac_id) %>%
    dplyr::summarise(n_features = dplyr::n(), max_match_dist_m = max(match_dist_m))
)

footprint_summary <- facilities %>%
  sf::st_drop_geometry() %>%
  dplyr::select(fac_id, project_name, type, status, totalcapacity_mw, earliest_start, lat, lon) %>%
  dplyr::left_join(footprint_summary, by = "fac_id") %>%
  dplyr::mutate(footprint_source = dplyr::if_else(is.na(n_features), "point", "GRW"),
                ha_per_mw = area_ha / totalcapacity_mw,
                mw_per_turbine = dplyr::if_else(type == "wind", totalcapacity_mw / n_features, NA_real_))

# how many points from my error-checked locations are matched by footprints in GRW data?
table(footprint_summary$type, footprint_summary$footprint_source)
#        GRW point
# solar  132    17
# wind   100    13

# have exported possible mismatches between my point locations and GRW data - the 30
# facilities with points but no GRW footprint are a mix of recently constructed 
# facilities (so possibly not visible in mid-2024 when GRW data from) or projects
# that GRW's machine learning must have missed for whatever reason - still better
# to have proper footprints for most facilities, and will use a buffer around the 
# point for these 30 facilities where no footprint easily available

readr::write_csv(footprint_summary, "data_output/footprint_summary.csv")

rm(facilities, match_footprints)
