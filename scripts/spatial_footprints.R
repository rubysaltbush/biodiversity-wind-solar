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
rm(grw_solar, grw_wind, match_footprints)

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
rm(footprint_classes)

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

rm(facilities, footprints_solar, footprints_wind, footprint_areas,
   footprint_majority, footprint_cleared)

# summary numbers ----

# footprint area and tree cover by facility type; area per MW uses GRW footprints
# only, tree cover and loss use facilities first operating from 2001
type_summary <- footprint_summary %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(
    n_facilities = dplyr::n(),
    n_grw_footprint = sum(footprint_source == "GRW"),
    total_footprint_ha_grw = sum(footprint_ha[footprint_source == "GRW"]),
    median_ha_per_mw_grw = median((footprint_ha / totalcapacity_mw)[footprint_source == "GRW"]),
    n_since_2001 = sum(earliest_start >= 2001),
    mean_treecover2000_since_2001 = mean(treecover2000_fp[earliest_start >= 2001]),
    mean_pct_treeloss_since_2001 = 100 * mean(frac_treeloss_fp[earliest_start >= 2001]),
    total_treeloss_ha_since_2001 = sum((frac_treeloss_fp * footprint_ha)[earliest_start >= 2001]),
    n_with_treeloss_since_2001 = sum(frac_treeloss_fp[earliest_start >= 2001] > 0)
  )
type_summary
# type  n_facilities n_grw_footprint total_footprint_ha_grw median_ha_per_mw_grw n_since_2001 mean_treecover2000_since_2001 mean_pct_treeloss_since_2001
# 1 solar          149             132                 14763.                1.77           149                          1.29                         1.79
# 2 wind           113             100                  3105.                0.263          111                          4.41                         4.24

# so yes, solar uses more land area per MW than wind, solar tends to have been built
# more on areas with less tree cover in yr 2000 though thus less tree loss for
# solar than for wind to date

readr::write_csv(type_summary, "data_output/spatial_results_summary.csv")
rm(type_summary)

# number and capacity of facilities by majority footprint class of present-day
# and pre-1750 simplified vegetation and land capability
mvgcodes <- readr::read_csv("data_input/mvg_codes_names.csv")

class_summary <- footprint_summary %>%
  dplyr::select(type, totalcapacity_mw, mvg = mvg_fp_majority,
                mvgpre1750 = mvgpre1750_fp_majority,
                land_capability = land_capability_fp_majority) %>%
  tidyr::pivot_longer(c(mvg, mvgpre1750, land_capability),
                      names_to = "layer", values_to = "code") %>%
  dplyr::left_join(dplyr::select(mvgcodes, code = mvg_code, mvg_simplified), by = "code") %>%
  dplyr::mutate(class = dplyr::if_else(layer == "land_capability",
                                       as.character(code), mvg_simplified)) %>%
  dplyr::group_by(type, layer, class) %>%
  dplyr::summarise(n = dplyr::n(), capacity_mw = sum(totalcapacity_mw), .groups = "drop_last") %>%
  dplyr::mutate(prop_facilities = n / sum(n), prop_capacity = capacity_mw / sum(capacity_mw)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(layer, type, desc(n))
print(class_summary, n = 50)
# type  layer           class                                              n capacity_mw prop_facilities prop_capacity
# 1 solar land_capability 2                                                 33       1826.         0.221         0.163  
# 2 solar land_capability 3                                                 32       2552.         0.215         0.228  
# 3 solar land_capability 5                                                 25       2356.         0.168         0.210  
# 4 solar land_capability 6                                                 23       1788.         0.154         0.160  
# 5 solar land_capability 4                                                 21       1613.         0.141         0.144  
# 6 solar land_capability 7                                                  9        573.         0.0604        0.0512 
# 7 solar land_capability 1                                                  3        370          0.0201        0.0331 
# 8 solar land_capability 8                                                  3        115.         0.0201        0.0103 
# 9 wind  land_capability 2                                                 25       3496          0.221         0.235  
# 10 wind  land_capability 4                                                 24       2976.         0.212         0.200  
# 11 wind  land_capability 3                                                 18       2129.         0.159         0.143  
# 12 wind  land_capability 6                                                 17       2897.         0.150         0.194  
# 13 wind  land_capability 5                                                 13       1341          0.115         0.0900 
# 14 wind  land_capability 1                                                 10        927          0.0885        0.0622 
# 15 wind  land_capability 7                                                  4        827.         0.0354        0.0555 
# 16 wind  land_capability 8                                                  1        197          0.00885       0.0132 
# 17 wind  land_capability 9                                                  1        112          0.00885       0.00752
# 18 solar mvg             Cleared, non-native vegetation, buildings        127       9710          0.852         0.867  
# 19 solar mvg             Grasslands                                         7        560.         0.0470        0.0500 
# 20 solar mvg             Shrublands                                         6        460          0.0403        0.0411 
# 21 solar mvg             Eucalypt forests and woodlands                     4        224.         0.0268        0.0200 
# 22 solar mvg             Non-eucalypt forests and woodlands                 4        179.         0.0268        0.0160 
# 23 solar mvg             Bare ground, modified native vegetation, other     1         60          0.00671       0.00536
# 24 wind  mvg             Cleared, non-native vegetation, buildings         93      12566.         0.823         0.843  
# 25 wind  mvg             Eucalypt forests and woodlands                    10       1381          0.0885        0.0927 
# 26 wind  mvg             Shrublands                                         6        514.         0.0531        0.0345 
# 27 wind  mvg             Grasslands                                         2        257          0.0177        0.0172 
# 28 wind  mvg             Bare ground, modified native vegetation, other     1        118          0.00885       0.00792
# 29 wind  mvg             Non-eucalypt forests and woodlands                 1         66          0.00885       0.00443
# 30 solar mvgpre1750      Eucalypt forests and woodlands                   110       8253.         0.738         0.737  
# 31 solar mvgpre1750      Grasslands                                        14        800.         0.0940        0.0715 
# 32 solar mvgpre1750      Non-eucalypt forests and woodlands                13       1182.         0.0872        0.106  
# 33 solar mvgpre1750      Shrublands                                        11        918.         0.0738        0.0821 
# 34 solar mvgpre1750      Rainforests and Vine Thickets                      1         40          0.00671       0.00357
# 35 wind  mvgpre1750      Eucalypt forests and woodlands                    69       9336.         0.611         0.627  
# 36 wind  mvgpre1750      Shrublands                                        20       1990.         0.177         0.134  
# 37 wind  mvgpre1750      Grasslands                                        17       2814          0.150         0.189  
# 38 wind  mvgpre1750      Non-eucalypt forests and woodlands                 4        531          0.0354        0.0356 
# 39 wind  mvgpre1750      Rainforests and Vine Thickets                      2        124          0.0177        0.00832
# 40 wind  mvgpre1750      Bare ground, modified native vegetation, other     1        107          0.00885       0.00718

readr::write_csv(class_summary, "data_output/spatial_results_classes.csv")
rm(mvgcodes, class_summary)
