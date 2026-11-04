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

# footprint environments ----

# impact area of each facility: solar array polygons, a 50 m radius around each
# turbine (turbine pad and surrounds), or a 1 km radius around the point location
# for facilities without a GRW footprint
footprint_areas <- rbind(
  dplyr::select(footprints_solar, fac_id),
  dplyr::select(sf::st_buffer(footprints_wind, 50), fac_id),
  facilities %>%
    dplyr::filter(!fac_id %in% c(footprints_solar$fac_id, footprints_wind$fac_id)) %>%
    sf::st_buffer(1000) %>%
    dplyr::select(fac_id) %>%
    sf::st_set_geometry("geom")
) %>%
  dplyr::group_by(fac_id) %>%
  dplyr::summarise() %>%
  dplyr::mutate(footprint_ha = as.numeric(sf::st_area(.)) / 10000)

# fraction of each footprint in each class of present-day and pre-1750 vegetation
# (NVIS Major Vegetation Groups) and agricultural land capability
categorical <- list(
  mvg = terra::rast("data_cache/NVIS_MVG_07_tif/nvis-mvg.tif")[[1]],
  mvgpre1750 = terra::rast("data_cache/NVIS_V7_0_AUST_RASTERS_PRE_ALL_GEOTIFF/nvis7_0_aust_pre_mvg_alb.tif")[[1]],
  land_capability = terra::rast("data_cache/RD-LANDCAPAUS-02/land_capability.tif")
)

footprint_classes <- purrr::imap_dfr(categorical, function(r, layer) {
  exactextractr::exact_extract(r, footprint_areas, "frac", append_cols = "fac_id") %>%
    tidyr::pivot_longer(-fac_id, names_to = "class", names_prefix = "frac_",
                        values_to = "fraction") %>%
    dplyr::filter(fraction > 0) %>%
    dplyr::mutate(layer = layer, class = as.numeric(class))
})
rm(categorical)

readr::write_csv(footprint_classes, "data_output/footprint_classes.csv")

# majority class of each layer per footprint
footprint_majority <- footprint_classes %>%
  dplyr::group_by(fac_id, layer) %>%
  dplyr::slice_max(fraction, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(fac_id, layer, class) %>%
  tidyr::pivot_wider(names_from = layer, values_from = class,
                     names_glue = "{layer}_fp_majority")

# fraction of each footprint on cleared land (NVIS MVG 25)
footprint_cleared <- footprint_classes %>%
  dplyr::filter(layer == "mvg", class == 25) %>%
  dplyr::select(fac_id, frac_cleared_fp = fraction)
# BUT looking at these there seems to be a decent error rate in NVIS cleared land category
# so maybe won't use frac_cleared_fp

# mean tree cover in 2000, and fraction of footprint area with tree cover loss
# 2000-2023 (Hansen et al., 2013)
footprint_areas$treecover2000_fp <- exactextractr::exact_extract(
  terra::rast("data_cache/Hansen_GFC_treecover2000_aus.tif"), footprint_areas, "mean")
footprint_areas$frac_treeloss_fp <- exactextractr::exact_extract(
  terra::rast("data_cache/Hansen_GFC_lossyear_aus.tif"), footprint_areas,
  function(values, coverage_fraction) sum(coverage_fraction[values > 0]) / sum(coverage_fraction))

# join footprint environments to footprint summary
footprint_summary <- footprint_summary %>%
  dplyr::left_join(sf::st_drop_geometry(footprint_areas), by = "fac_id") %>%
  dplyr::left_join(footprint_majority, by = "fac_id") %>%
  dplyr::left_join(footprint_cleared, by = "fac_id") %>%
  dplyr::mutate(frac_cleared_fp = tidyr::replace_na(frac_cleared_fp, 0))

readr::write_csv(footprint_summary, "data_output/footprint_summary.csv")

rm(facilities, match_footprints, footprints_solar, footprints_wind, footprint_areas,
   footprint_majority, footprint_cleared)
