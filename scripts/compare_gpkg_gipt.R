# compare_gpkg_gipt.R
# Compare two global wind/solar facility datasets for suitability in matching
# published biodiversity study locations against real facility locations globally.
#
# Dataset 1: GIPT (Global Integrated Power Tracker, Feb 2025)
#   - project-level points; crowdsourced/curated by Global Energy Monitor
#
# Dataset 2: satellite/OSM-derived geopackages (2024 Q2)
#   - solar_all_2024q2_v1.gpkg  — polygon footprints (~86k features)
#   - wind_all_2024q2_v1.gpkg   — individual turbine points (~375k features)
#
# Key question: which is more appropriate for comparing study locations vs
# the global distribution of wind/solar facilities?

library(dplyr)
library(sf)
library(ggplot2)
library(tibble)
library(readr)
library(stringr)

# ---- project colours ----
source("scripts/my_colours.R")

# ---- load and process GIPT data ----
source("scripts/gipt_data.R")
# creates: windsolar_gipt (tibble with lat/lon columns)

windsolar_gipt_spat <- windsolar_gipt |>
  dplyr::filter(!is.na(lat)) |>
  dplyr::distinct() |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# ---- load and pre-process systematic review data ----
sysrevdata <- readr::read_csv(
  "data_input/wind_solar_biodiversity_sysreview_v8_20260602.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(exclude == FALSE)

sysrevlatlong <- sysrevdata |>
  dplyr::select(title, wind_solar, latitude, longitude) |>
  dplyr::filter(!is.na(latitude))

rm(sysrevdata)

# ---- load gpkg data ----
solar_gpkg <- sf::st_read("data_cache/solar_all_2024q2_v1.gpkg", quiet = TRUE) |>
  sf::st_transform(4326) |>
  dplyr::mutate(type = "solar")

wind_gpkg <- sf::st_read("data_cache/wind_all_2024q2_v1.gpkg", quiet = TRUE) |>
  sf::st_transform(4326) |>
  dplyr::mutate(type = "wind")

# extract centroid coordinates for solar polygons (needed for lat-based summaries)
# st_make_valid() required: some polygons have degenerate edges/duplicate vertices
solar_gpkg <- solar_gpkg |>
  sf::st_make_valid() |>
  dplyr::mutate(
    lon = sf::st_coordinates(sf::st_centroid(geom))[, 1],
    lat = sf::st_coordinates(sf::st_centroid(geom))[, 2]
  )

# extract coordinates for wind turbine points
wind_gpkg <- wind_gpkg |>
  dplyr::mutate(
    lon = sf::st_coordinates(geom)[, 1],
    lat = sf::st_coordinates(geom)[, 2]
  )

# combine attribute data (drop geometry for summaries)
gpkg_attr <- dplyr::bind_rows(
  sf::st_drop_geometry(solar_gpkg),
  sf::st_drop_geometry(wind_gpkg)
)

# ---- helper: latitude band classification ----
lat_bands <- function(df, lat_col = "lat") {
  df |>
    dplyr::mutate(lat_band = dplyr::case_when(
      abs(.data[[lat_col]]) < 23.5 ~ "Tropical",
      abs(.data[[lat_col]]) < 66.5 ~ "Temperate",
      TRUE                         ~ "Polar"
    )) |>
    dplyr::count(type, lat_band) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop = round(n / sum(n), 3)) |>
    dplyr::ungroup()
}

# ---- 1. basic coverage summary ----
# note: GIPT is at project/phase level; gpkg wind is at individual turbine level
# and gpkg solar is at installation-polygon level — raw counts are not directly
# comparable, but country and geographic coverage are still informative

cat("=== Record counts ===\n")
cat("GIPT (project-level):\n")
print(dplyr::count(sf::st_drop_geometry(windsolar_gipt_spat), type))

cat("\ngpkg (turbine/polygon level):\n")
print(dplyr::count(gpkg_attr, type))

cat("\nNote: wind gpkg counts individual turbines; GIPT counts project phases.\n")
cat("Solar gpkg counts individual installations; GIPT counts project phases.\n\n")

# country coverage
gipt_countries <- windsolar_gipt_spat |>
  sf::st_drop_geometry() |>
  dplyr::distinct(type, country_area) |>
  dplyr::count(type, name = "n_countries_gipt")

gpkg_countries <- gpkg_attr |>
  dplyr::filter(!is.na(COUNTRY)) |>
  dplyr::distinct(type, COUNTRY) |>
  dplyr::count(type, name = "n_countries_gpkg")

cat("=== Country coverage ===\n")
dplyr::left_join(gipt_countries, gpkg_countries, by = "type") |> print()

rm(gipt_countries, gpkg_countries)

# ---- 2. temporal coverage ----
cat("\n=== Construction year range — gpkg ===\n")
gpkg_attr |>
  dplyr::filter(!is.na(construction_year)) |>
  dplyr::group_by(type) |>
  dplyr::summarise(
    min_year       = min(construction_year),
    max_year       = max(construction_year),
    pct_missing_yr = round(mean(is.na(construction_year)) * 100, 1),
    .groups = "drop"
  ) |>
  print()

cat("\nGIPT start year range:\n")
windsolar_gipt |>
  dplyr::group_by(type) |>
  dplyr::summarise(
    min_year       = min(start_year, na.rm = TRUE),
    max_year       = max(start_year, na.rm = TRUE),
    pct_missing_yr = round(mean(is.na(start_year)) * 100, 1),
    .groups = "drop"
  ) |>
  print()

# ---- 3. hemisphere distribution ----
cat("\n=== Hemisphere proportions — GIPT ===\n")
windsolar_gipt_spat |>
  sf::st_drop_geometry() |>
  dplyr::mutate(hemisphere = dplyr::if_else(lat > 0, "North", "South")) |>
  dplyr::count(type, hemisphere) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop = round(n / sum(n), 3)) |>
  dplyr::ungroup() |>
  print()

cat("\n=== Hemisphere proportions — gpkg ===\n")
gpkg_attr |>
  dplyr::mutate(hemisphere = dplyr::if_else(lat > 0, "North", "South")) |>
  dplyr::count(type, hemisphere) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop = round(n / sum(n), 3)) |>
  dplyr::ungroup() |>
  print()

# ---- 4. latitude band distribution ----
cat("\n=== Latitude band proportions — GIPT ===\n")
windsolar_gipt_spat |>
  sf::st_drop_geometry() |>
  lat_bands() |>
  print()

cat("\n=== Latitude band proportions — gpkg ===\n")
gpkg_attr |>
  lat_bands() |>
  print()

# ---- 5. spatial cross-check: GIPT vs gpkg proximity ----
# For a random sample of GIPT points, find the nearest gpkg feature.
# Large distances suggest GIPT coordinates are imprecise; small distances
# suggest good agreement between sources.

set.seed(9271)

gipt_wind    <- windsolar_gipt_spat |> 
  dplyr::filter(type == "wind")  |> 
  dplyr::slice_sample(n = 500)
gipt_wind_ea <- sf::st_transform(gipt_wind, 8857)
gpkg_wind_ea <- sf::st_transform(wind_gpkg, 8857) |> dplyr::select(type, geom)
rm(gipt_wind)

nn_wind_idx  <- sf::st_nearest_feature(gipt_wind_ea, gpkg_wind_ea)
nn_wind_dist <- sf::st_distance(
  gipt_wind_ea,
  gpkg_wind_ea[nn_wind_idx, ],
  by_element = TRUE
) |> as.numeric()

rm(gipt_wind_ea, gpkg_wind_ea, nn_wind_idx)

gipt_solar    <- windsolar_gipt_spat |> dplyr::filter(type == "solar") |> dplyr::slice_sample(n = 500)
gipt_solar_ea <- sf::st_transform(gipt_solar, 8857)
gpkg_solar_ea <- sf::st_transform(solar_gpkg, 8857) |> dplyr::select(type, geom)
rm(gipt_solar)

nn_solar_idx  <- sf::st_nearest_feature(gipt_solar_ea, gpkg_solar_ea)
nn_solar_dist <- sf::st_distance(
  gipt_solar_ea,
  gpkg_solar_ea[nn_solar_idx, ],
  by_element = TRUE
) |> as.numeric()

rm(gipt_solar_ea, gpkg_solar_ea, nn_solar_idx)

cat("\n=== Nearest-neighbour distance: GIPT → gpkg (metres, n=500 sample each) ===\n")
tibble::tibble(
  type           = c("wind", "solar"),
  median_dist_km = round(c(median(nn_wind_dist), median(nn_solar_dist)) / 1000, 1),
  p25_dist_km    = round(c(quantile(nn_wind_dist, 0.25), quantile(nn_solar_dist, 0.25)) / 1000, 1),
  p75_dist_km    = round(c(quantile(nn_wind_dist, 0.75), quantile(nn_solar_dist, 0.75)) / 1000, 1),
  p95_dist_km    = round(c(quantile(nn_wind_dist, 0.95), quantile(nn_solar_dist, 0.95)) / 1000, 1)
) |> print()

# ---- 6. location accuracy field in GIPT ----
cat("\n=== GIPT location_accuracy breakdown ===\n")
windsolar_gipt |>
  dplyr::count(type, location_accuracy) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop = round(n / sum(n), 3)) |>
  dplyr::ungroup() |>
  dplyr::arrange(type, dplyr::desc(n)) |>
  print(n = 30)

# ---- 7. visualise geographic distribution ----
worldmap_plot <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::select(geometry)

set.seed(3847)
gpkg_plot <- dplyr::bind_rows(
  sf::st_drop_geometry(solar_gpkg) |> dplyr::slice_sample(n = 5000) |> dplyr::mutate(source = "gpkg"),
  sf::st_drop_geometry(wind_gpkg)  |> dplyr::slice_sample(n = 5000) |> dplyr::mutate(source = "gpkg")
) |>
  dplyr::select(type, source, lat, lon)

gipt_plot <- windsolar_gipt_spat |>
  sf::st_drop_geometry() |>
  dplyr::mutate(source = "GIPT") |>
  dplyr::select(type, source, lat, lon)

combined_plot_data <- dplyr::bind_rows(gpkg_plot, gipt_plot)
rm(gpkg_plot, gipt_plot)

p_coverage <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = worldmap_plot, fill = "grey90", colour = "grey70", linewidth = 0.2) +
  ggplot2::geom_point(
    data = combined_plot_data,
    ggplot2::aes(x = lon, y = lat, colour = type),
    size = 0.3, alpha = 0.4
  ) +
  ggplot2::facet_grid(source ~ type) +
  ggplot2::scale_colour_manual(values = my_colours$windsolar) +
  ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-70, 80), expand = FALSE) +
  ggplot2::labs(
    title    = "Global coverage: GIPT (project-level) vs gpkg (turbine/installation-level)",
    subtitle = "gpkg points are a 5 000-record random sample per energy type",
    x = NULL, y = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    strip.text      = ggplot2::element_text(face = "bold")
  )

print(p_coverage)
rm(combined_plot_data, p_coverage)

# ---- 8. latitude-band proportion comparison: side-by-side bar chart ----
lat_compare <- dplyr::bind_rows(
  windsolar_gipt_spat |> sf::st_drop_geometry() |> lat_bands() |> dplyr::mutate(source = "GIPT"),
  gpkg_attr            |>                          lat_bands() |> dplyr::mutate(source = "gpkg (satellite/OSM)")
)

ggplot2::ggplot(lat_compare,
  ggplot2::aes(x = lat_band, y = prop, fill = type)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::facet_grid(source ~ type) +
  ggplot2::scale_fill_manual(values = my_colours$windsolar) +
  ggplot2::labs(
    title = "Latitude-band distribution: GIPT vs gpkg",
    x     = "Latitude band",
    y     = "Proportion of facilities",
    fill  = "Energy type"
  ) +
  ggpubr::theme_pubr() +
  ggplot2::theme(legend.position = "none")

rm(lat_compare)

# ---- 9. nearest-neighbour distance distribution plot ----
nn_df <- tibble::tibble(
  dist_km = c(nn_wind_dist / 1000, nn_solar_dist / 1000),
  type    = rep(c("wind", "solar"), each = 500)
)
rm(nn_wind_dist, nn_solar_dist)

ggplot2::ggplot(nn_df, ggplot2::aes(x = dist_km, fill = type)) +
  ggplot2::geom_histogram(bins = 60) +
  ggplot2::facet_wrap(~ type, scales = "free_x") +
  ggplot2::scale_x_continuous(limits = c(0, NA)) +
  ggplot2::coord_cartesian(ylim = c(0, 100)) +
  ggplot2::scale_fill_manual(values = my_colours$windsolar) +
  ggplot2::labs(
    title    = "Distance from GIPT point to nearest gpkg feature",
    subtitle = "Sampled 500 GIPT points per energy type; projected to Equal Earth (EPSG:8857)",
    x = "Distance (km)", y = "Count"
  ) +
  ggpubr::theme_pubr() +
  ggplot2::theme(legend.position = "none")

rm(nn_df)

# ---- 10. geographic bias: sensitivity to facility dataset choice ----
# Replicate the hemisphere, latitude-band, and continent study-vs-facility bias
# ratios from sysreview_analysis.R ("where" section) using gpkg as the facility
# reference instead of GIPT. Ratio = prop_studies / prop_facilities; ~1 = no bias.
#
# Note: gpkg wind is at individual turbine level (~375k), not project level.
# Proportions should be interpreted accordingly — a large farm contributes
# many turbines, which may skew geographic proportions relative to GIPT.

# ---- 10a. hemisphere ----

study_hemi <- sysrevlatlong |>
  dplyr::mutate(hemisphere = dplyr::if_else(latitude > 0, "North", "South")) |>
  dplyr::distinct(title, wind_solar, hemisphere) |>
  dplyr::count(wind_solar, hemisphere) |>
  dplyr::group_by(wind_solar) |>
  dplyr::mutate(prop_studies = n / sum(n)) |>
  dplyr::rename(n_studies = n) |>
  dplyr::ungroup()

gipt_hemi <- windsolar_gipt_spat |>
  sf::st_drop_geometry() |>
  dplyr::mutate(hemisphere = dplyr::if_else(lat > 0, "North", "South")) |>
  dplyr::count(type, hemisphere) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gipt = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gipt = n) |>
  dplyr::ungroup()

gpkg_hemi <- gpkg_attr |>
  dplyr::mutate(hemisphere = dplyr::if_else(lat > 0, "North", "South")) |>
  dplyr::count(type, hemisphere) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gpkg = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gpkg = n) |>
  dplyr::ungroup()

hemi_compare <- study_hemi |>
  dplyr::left_join(gipt_hemi, by = c("wind_solar", "hemisphere")) |>
  dplyr::left_join(gpkg_hemi, by = c("wind_solar", "hemisphere")) |>
  dplyr::mutate(
    ratio_gipt = round(prop_studies / prop_fac_gipt, 3),
    ratio_gpkg = round(prop_studies / prop_fac_gpkg, 3)
  )

cat("\n=== Geographic bias ratios — hemisphere ===\n")
cat("(ratio = prop_studies / prop_facilities; ~1 = no bias)\n")
hemi_compare |>
  dplyr::select(wind_solar, hemisphere, n_studies, n_fac_gipt, n_fac_gpkg,
                ratio_gipt, ratio_gpkg) |>
  print()

rm(study_hemi, gipt_hemi, gpkg_hemi)

# ---- 10b. latitude band ----

study_lat <- sysrevlatlong |>
  dplyr::mutate(lat_band = dplyr::case_when(
    abs(latitude) < 23.5 ~ "Tropical",
    abs(latitude) < 66.5 ~ "Temperate",
    TRUE                 ~ "Polar"
  )) |>
  dplyr::distinct(title, wind_solar, lat_band) |>
  dplyr::count(wind_solar, lat_band) |>
  dplyr::group_by(wind_solar) |>
  dplyr::mutate(prop_studies = n / sum(n)) |>
  dplyr::rename(n_studies = n) |>
  dplyr::ungroup()

gipt_lat <- windsolar_gipt_spat |>
  sf::st_drop_geometry() |>
  dplyr::mutate(lat_band = dplyr::case_when(
    abs(lat) < 23.5 ~ "Tropical",
    abs(lat) < 66.5 ~ "Temperate",
    TRUE            ~ "Polar"
  )) |>
  dplyr::count(type, lat_band) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gipt = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gipt = n) |>
  dplyr::ungroup()

gpkg_lat <- gpkg_attr |>
  dplyr::mutate(lat_band = dplyr::case_when(
    abs(lat) < 23.5 ~ "Tropical",
    abs(lat) < 66.5 ~ "Temperate",
    TRUE            ~ "Polar"
  )) |>
  dplyr::count(type, lat_band) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gpkg = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gpkg = n) |>
  dplyr::ungroup()

lat_compare_bias <- study_lat |>
  dplyr::left_join(gipt_lat, by = c("wind_solar", "lat_band")) |>
  dplyr::left_join(gpkg_lat, by = c("wind_solar", "lat_band")) |>
  dplyr::mutate(
    ratio_gipt = round(prop_studies / prop_fac_gipt, 3),
    ratio_gpkg = round(prop_studies / prop_fac_gpkg, 3)
  )

cat("\n=== Geographic bias ratios — latitude band ===\n")
lat_compare_bias |>
  dplyr::select(wind_solar, lat_band, n_studies, n_fac_gipt, n_fac_gpkg,
                ratio_gipt, ratio_gpkg) |>
  print()

rm(study_lat, gipt_lat, gpkg_lat)

# ---- 10c. continent ----
# worldmap_plot has geometry only; reload with continent attributes
worldmap_cont <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(sovereignt != "Antarctica") |>
  dplyr::select(continent, admin, geometry)

# s2 off: worldmap has invalid geometries (matches sysreview_analysis.R behaviour)
sf::sf_use_s2(FALSE)

# study locations → continent
sysrev_sf <- sf::st_as_sf(
  sysrevlatlong, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
)
study_cont_join <- sf::st_join(sysrev_sf, worldmap_cont)
na_idx <- which(is.na(study_cont_join$continent))
if (length(na_idx) > 0) {
  nn_idx <- sf::st_nearest_feature(sysrev_sf[na_idx, ], worldmap_cont)
  study_cont_join[na_idx, "continent"] <-
    sf::st_drop_geometry(worldmap_cont)[nn_idx, "continent"]
  rm(nn_idx)
}
rm(sysrev_sf, na_idx)

study_cont <- study_cont_join |>
  sf::st_drop_geometry() |>
  dplyr::distinct(title, wind_solar, continent) |>
  dplyr::count(wind_solar, continent) |>
  dplyr::group_by(wind_solar) |>
  dplyr::mutate(prop_studies = n / sum(n)) |>
  dplyr::rename(n_studies = n) |>
  dplyr::ungroup()
rm(study_cont_join)

# GIPT facilities → continent
gipt_cont_join <- sf::st_join(windsolar_gipt_spat, worldmap_cont)
na_idx <- which(is.na(gipt_cont_join$continent))
if (length(na_idx) > 0) {
  nn_idx <- sf::st_nearest_feature(windsolar_gipt_spat[na_idx, ], worldmap_cont)
  gipt_cont_join[na_idx, "continent"] <-
    sf::st_drop_geometry(worldmap_cont)[nn_idx, "continent"]
  rm(nn_idx)
}
rm(na_idx)

gipt_cont <- gipt_cont_join |>
  sf::st_drop_geometry() |>
  dplyr::count(type, continent) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gipt = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gipt = n) |>
  dplyr::ungroup()
rm(gipt_cont_join)

# gpkg facilities → continent (make spatial from lon/lat)
gpkg_spat <- gpkg_attr |>
  dplyr::filter(!is.na(lat), !is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

gpkg_cont_join <- sf::st_join(gpkg_spat, worldmap_cont)
na_idx <- which(is.na(gpkg_cont_join$continent))
if (length(na_idx) > 0) {
  nn_idx <- sf::st_nearest_feature(gpkg_spat[na_idx, ], worldmap_cont)
  gpkg_cont_join[na_idx, "continent"] <-
    sf::st_drop_geometry(worldmap_cont)[nn_idx, "continent"]
  rm(nn_idx)
}
rm(gpkg_spat, na_idx, worldmap_cont)

gpkg_cont <- gpkg_cont_join |>
  sf::st_drop_geometry() |>
  dplyr::count(type, continent) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gpkg = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gpkg = n) |>
  dplyr::ungroup()
rm(gpkg_cont_join)

cont_compare <- study_cont |>
  dplyr::full_join(gipt_cont, by = c("wind_solar", "continent")) |>
  dplyr::full_join(gpkg_cont, by = c("wind_solar", "continent")) |>
  tidyr::replace_na(list(n_studies = 0L, prop_studies = 0)) |>
  dplyr::mutate(
    ratio_gipt = round(prop_studies / prop_fac_gipt, 3),
    ratio_gpkg = round(prop_studies / prop_fac_gpkg, 3)
  ) |>
  dplyr::arrange(wind_solar, continent)

cat("\n=== Geographic bias ratios — continent ===\n")
cont_compare |>
  dplyr::select(wind_solar, continent, n_studies, n_fac_gipt, n_fac_gpkg,
                ratio_gipt, ratio_gpkg) |>
  print(n = Inf)

rm(study_cont, gipt_cont, gpkg_cont)

# ---- 10d. visualise ratio sensitivity ----
# Dot plot: GIPT vs gpkg ratio for each geographic strata.
# Deviation between points indicates where dataset choice affects conclusions.

ratio_long <- dplyr::bind_rows(
  hemi_compare |>
    dplyr::select(wind_solar, strata = hemisphere, ratio_gipt, ratio_gpkg) |>
    dplyr::mutate(strata_type = "Hemisphere"),
  lat_compare_bias |>
    dplyr::select(wind_solar, strata = lat_band, ratio_gipt, ratio_gpkg) |>
    dplyr::mutate(strata_type = "Latitude band"),
  cont_compare |>
    dplyr::filter(!is.na(ratio_gipt) | !is.na(ratio_gpkg)) |>
    dplyr::select(wind_solar, strata = continent, ratio_gipt, ratio_gpkg) |>
    dplyr::mutate(strata_type = "Continent")
) |>
  tidyr::pivot_longer(
    cols      = c(ratio_gipt, ratio_gpkg),
    names_to  = "dataset",
    names_prefix = "ratio_",
    values_to = "ratio"
  ) |>
  dplyr::mutate(
    dataset     = dplyr::if_else(dataset == "gipt", "GIPT", "gpkg (satellite/OSM)"),
    strata_type = factor(strata_type, levels = c("Hemisphere", "Latitude band", "Continent"))
  )

ggplot2::ggplot(ratio_long,
  ggplot2::aes(x = strata, y = ratio,
               colour = wind_solar, shape = dataset)) +
  ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(
    size     = 2.5,
    position = ggplot2::position_dodge(width = 0.4)
  ) +
  ggplot2::facet_wrap(
    ~ strata_type + wind_solar,
    scales = "free_x", nrow = 3
  ) +
  ggplot2::scale_colour_manual(values = my_colours$windsolar) +
  ggplot2::scale_shape_manual(
    values = c("GIPT" = 16, "gpkg (satellite/OSM)" = 17)
  ) +
  ggplot2::labs(
    title    = "Sensitivity of geographic bias ratios to facility dataset",
    subtitle = "ratio = prop_studies / prop_facilities; dashed line = no bias (ratio = 1)",
    x = NULL, y = "Ratio (studies / facilities)",
    colour = "Energy type", shape = "Facility dataset"
  ) +
  ggpubr::theme_pubr() +
  ggplot2::theme(
    strip.text  = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
  )

rm(ratio_long, hemi_compare, lat_compare_bias, cont_compare)

# ---- 11. biome analysis ----
# Replicate the biome bias analysis from sysreview_analysis.R (Dinerstein et al.
# 2017 ecoregions), comparing GIPT vs gpkg as the facility reference.
# Ratio = prop_studies / prop_facilities; > 1 = biome overrepresented in studies.
#
# Note: the shapefile is ~150 MB and the gpkg spatial join is slow (~460k points).

# load and validate biome shapefile
biomes_shp <- sf::st_read("data_cache/Ecoregions2017/Ecoregions2017.shp",
                           quiet = TRUE) |>
  sf::st_make_valid()

# proportional area of each biome (in shapefile area units — square degrees;
# sufficient for proportional comparisons)
biome_areas <- biomes_shp |>
  sf::st_drop_geometry() |>
  dplyr::group_by(BIOME_NAME) |>
  dplyr::summarise(total_area = sum(SHAPE_AREA), .groups = "drop") |>
  dplyr::filter(BIOME_NAME != "N/A") |>
  dplyr::mutate(prop_area = total_area / sum(total_area))

# study locations → biome
sysrev_sf <- sf::st_as_sf(
  sysrevlatlong, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE
)
extract_sysrev <- sf::st_join(sysrev_sf, biomes_shp)
rm(sysrev_sf)

# GIPT facilities → biome
extract_gipt <- sf::st_join(windsolar_gipt_spat, biomes_shp)

# gpkg facilities → biome (slow: ~460k points against complex polygon shapefile)
gpkg_spat_biome <- gpkg_attr |>
  dplyr::filter(!is.na(lat), !is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
extract_gpkg <- sf::st_join(gpkg_spat_biome, biomes_shp)
rm(gpkg_spat_biome, biomes_shp)

# study proportions
study_biome <- extract_sysrev |>
  sf::st_drop_geometry() |>
  dplyr::distinct(title, wind_solar, BIOME_NAME) |>
  dplyr::count(wind_solar, BIOME_NAME) |>
  dplyr::group_by(wind_solar) |>
  dplyr::mutate(prop_studies = n / sum(n)) |>
  dplyr::rename(n_studies = n) |>
  dplyr::ungroup()
rm(extract_sysrev)

# GIPT facility proportions
gipt_biome <- extract_gipt |>
  sf::st_drop_geometry() |>
  dplyr::count(type, BIOME_NAME) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gipt = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gipt = n) |>
  dplyr::ungroup()
rm(extract_gipt)

# gpkg facility proportions
gpkg_biome <- extract_gpkg |>
  sf::st_drop_geometry() |>
  dplyr::count(type, BIOME_NAME) |>
  dplyr::group_by(type) |>
  dplyr::mutate(prop_fac_gpkg = n / sum(n)) |>
  dplyr::rename(wind_solar = type, n_fac_gpkg = n) |>
  dplyr::ungroup()
rm(extract_gpkg)

# combine into one comparison table
biome_compare <- study_biome |>
  dplyr::full_join(gipt_biome, by = c("wind_solar", "BIOME_NAME")) |>
  dplyr::full_join(gpkg_biome, by = c("wind_solar", "BIOME_NAME")) |>
  dplyr::left_join(biome_areas, by = "BIOME_NAME") |>
  tidyr::replace_na(list(n_studies = 0L, prop_studies = 0)) |>
  dplyr::mutate(
    ratio_gipt = round(prop_studies / prop_fac_gipt, 3),
    ratio_gpkg = round(prop_studies / prop_fac_gpkg, 3)
  ) |>
  dplyr::arrange(BIOME_NAME, wind_solar)

rm(study_biome, gipt_biome, gpkg_biome, biome_areas)

cat("\n=== Biome bias ratios: GIPT vs gpkg ===\n")
cat("(ratio = prop_studies / prop_facilities; ~1 = no bias)\n")
biome_compare |>
  dplyr::filter(!is.na(BIOME_NAME)) |>
  dplyr::select(wind_solar, BIOME_NAME, n_studies, n_fac_gipt, n_fac_gpkg,
                ratio_gipt, ratio_gpkg) |>
  print(n = Inf)

# ---- 11a. figure: biome ratios using GIPT (main result, matches sysreview_analysis.R) ----
biome_compare |>
  dplyr::filter(!is.na(BIOME_NAME), n_studies > 0 | n_fac_gipt > 50) |>
  dplyr::mutate(
    reliable   = n_studies >= 5,
    short_name = stringr::str_remove(BIOME_NAME, " & Shrublands")
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x      = ratio_gipt,
    y      = forcats::fct_reorder(short_name, ratio_gipt),
    colour = wind_solar,
    alpha  = reliable
  )) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.35), guide = "none") +
  ggplot2::scale_colour_manual(values = my_colours$windsolar) +
  ggplot2::facet_wrap(~ wind_solar) +
  ggplot2::labs(
    title   = "Biome-level study bias (GIPT as facility reference)",
    x       = "Ratio (prop. studies / prop. facilities)",
    y       = NULL,
    colour  = "Energy type",
    caption = "Faded points: fewer than 5 studies"
  ) +
  ggpubr::theme_pubr(legend = "none")

# ---- 11b. figure: sensitivity of biome ratios to facility dataset ----
# Dumbbell plot: lines connect GIPT and gpkg ratios for the same biome.
# Short lines = conclusions robust to dataset choice; long lines = sensitive.
biome_long <- biome_compare |>
  dplyr::filter(!is.na(BIOME_NAME), n_studies > 0 | n_fac_gipt > 50 | n_fac_gpkg > 50) |>
  dplyr::mutate(
    reliable   = n_studies >= 5,
    short_name = stringr::str_remove(BIOME_NAME, " & Shrublands"),
    # use mean of the two ratios to order y axis
    mean_ratio = rowMeans(cbind(ratio_gipt, ratio_gpkg), na.rm = TRUE)
  ) |>
  tidyr::pivot_longer(
    cols         = c(ratio_gipt, ratio_gpkg),
    names_to     = "dataset",
    names_prefix = "ratio_",
    values_to    = "ratio"
  ) |>
  dplyr::mutate(
    dataset = dplyr::if_else(dataset == "gipt", "GIPT", "gpkg (satellite/OSM)")
  )

ggplot2::ggplot(biome_long,
  ggplot2::aes(
    x      = ratio,
    y      = forcats::fct_reorder(short_name, mean_ratio),
    colour = wind_solar,
    shape  = dataset,
    alpha  = reliable
  )) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_line(
    ggplot2::aes(group = interaction(short_name, wind_solar)),
    colour    = "grey70",
    linewidth = 0.5
  ) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.35), guide = "none") +
  ggplot2::scale_colour_manual(values = my_colours$windsolar) +
  ggplot2::scale_shape_manual(values = c("GIPT" = 16, "gpkg (satellite/OSM)" = 17)) +
  ggplot2::facet_wrap(~ wind_solar) +
  ggplot2::labs(
    title    = "Sensitivity of biome bias ratios to facility dataset",
    subtitle = "Lines connect GIPT and gpkg ratios for the same biome",
    x        = "Ratio (prop. studies / prop. facilities)",
    y        = NULL,
    colour   = "Energy type",
    shape    = "Facility dataset",
    caption  = "Faded points: fewer than 5 studies"
  ) +
  ggpubr::theme_pubr()

rm(biome_compare, biome_long)

# ---- clean up large objects ----
rm(solar_gpkg, wind_gpkg, gpkg_attr, worldmap_plot, lat_bands)
