# preconstruction_gipt_analysis.R
#
# Sensitivity analysis: does including GIPT "pre-construction" projects change
# conclusions about geographic bias (studies vs facilities)?
#
# Three main questions:
#  1. How much does adding pre-construction change the facility baseline?
#  2. Do the study-vs-facility bias ratios change materially?
#  3. Are pre-construction records themselves geographically representative,
#     or are they patchily curated (which would be a reason to exclude them)?
#
# Outputs: printed tables + figures saved to figures/preconstruction_*
# Standalone: loads all data internally; does not depend on sysreview_analysis.R

# ---- packages ----
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggpubr)
library(patchwork)

source("scripts/my_colours.R")

# ---- 1. Load GIPT data (two variants) ----

#* 1a. Wind ----
wind_raw1 <- readxl::read_xlsx(
  "data_input/Global-Wind-Power-Tracker-February-2025.xlsx", sheet = 2
)
wind_raw2 <- readxl::read_xlsx(
  "data_input/Global-Wind-Power-Tracker-February-2025.xlsx", sheet = 3
)

wind_all <- dplyr::bind_rows(wind_raw1, wind_raw2) |>
  dplyr::filter(`Capacity (MW)` >= 3,
                `Installation Type` == "Onshore") |>
  dplyr::select(
    country_area    = `Country/Area`,
    project_name    = `Project Name`,
    capacity_mw     = `Capacity (MW)`,
    status          = Status,
    start_year      = `Start year`,
    lat             = Latitude,
    lon             = Longitude,
    location_accuracy = `Location accuracy`,
    region          = Region
  ) |>
  dplyr::mutate(type = "wind")
rm(wind_raw1, wind_raw2)

#* 1b. Solar ----
solar_raw1 <- readxl::read_xlsx(
  "data_input/Global-Solar-Power-Tracker-February-2025.xlsx", sheet = 2
)
solar_raw2 <- readxl::read_xlsx(
  "data_input/Global-Solar-Power-Tracker-February-2025.xlsx", sheet = 3
)

solar_all <- dplyr::bind_rows(solar_raw1, solar_raw2) |>
  dplyr::filter(`Capacity (MW)` >= 3) |>
  dplyr::select(
    country_area    = `Country/Area`,
    project_name    = `Project Name`,
    capacity_mw     = `Capacity (MW)`,
    status          = Status,
    start_year      = `Start year`,
    lat             = Latitude,
    lon             = Longitude,
    location_accuracy = `Location accuracy`,
    region          = Region
  ) |>
  dplyr::mutate(
    type = "solar",
    lat  = as.numeric(lat),
    lon  = as.numeric(lon)
  )
rm(solar_raw1, solar_raw2)

#* 1c. Combine and create two variants ----
gipt_all <- dplyr::bind_rows(wind_all, solar_all)
rm(wind_all, solar_all)

# Variant A: operating / construction / retired (original analysis)
gipt_orig <- gipt_all |>
  dplyr::filter(status %in% c("operating", "construction", "retired"))

# Variant B: adds pre-construction
gipt_precon <- gipt_all |>
  dplyr::filter(status %in% c("operating", "construction", "retired", "pre-construction"))

cat("=== Record counts ===\n")
cat("Original (operating + construction + retired):\n")
print(dplyr::count(gipt_orig, type))
cat("\nWith pre-construction added:\n")
print(dplyr::count(gipt_precon, type))
cat("\nPre-construction only:\n")
print(dplyr::count(dplyr::filter(gipt_all, status == "pre-construction"), type))

# ---- 2. Load systematic review data ----
sysrevdata <- readr::read_csv(
  "data_input/wind_solar_biodiversity_sysreview_v8_20260602.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(exclude == FALSE)

sysrevlatlong <- sysrevdata |>
  dplyr::select(title, wind_solar, latitude, longitude) |>
  dplyr::filter(!is.na(latitude)) |>
  dplyr::distinct()

# ---- 3. Make spatial objects ----
sf::sf_use_s2(FALSE)   # avoid s2 issues with worldmap geometries (matches main analysis)

make_spat <- function(df) {
  df |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
}

spat_orig   <- make_spat(gipt_orig)
spat_precon <- make_spat(gipt_precon)

sysrev_sf <- sf::st_as_sf(
  sysrevlatlong, coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# ---- 4. Pre-construction data quality checks ----
# Before asking whether pre-construction changes bias, check whether the
# pre-construction records themselves are plausibly representative.

precon_only <- gipt_all |>
  dplyr::filter(status == "pre-construction")

cat("\n\n=== Pre-construction data completeness ===\n")
cat("\nMissing coordinates:\n")
precon_only |>
  dplyr::summarise(
    n_total      = n(),
    n_missing_lat = sum(is.na(lat)),
    pct_missing  = round(mean(is.na(lat)) * 100, 1)
  ) |>
  print()

cat("\nMissing coordinates by type:\n")
precon_only |>
  dplyr::group_by(type) |>
  dplyr::summarise(
    n_total      = n(),
    n_missing_lat = sum(is.na(lat)),
    pct_missing  = round(mean(is.na(lat)) * 100, 1),
    .groups = "drop"
  ) |>
  print()

cat("\nLocation accuracy breakdown (pre-construction vs operating):\n")
gipt_all |>
  dplyr::filter(status %in% c("operating", "pre-construction")) |>
  dplyr::count(status, location_accuracy) |>
  dplyr::group_by(status) |>
  dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
  dplyr::arrange(status, dplyr::desc(n)) |>
  print(n = 30)

# ---- 5. Geographic distribution of pre-construction vs operating ----
# Compare continent proportions — is pre-construction geographically skewed
# compared to the operating fleet?  Large divergence = uneven curation.

worldmap <- rnaturalearth::ne_countries(returnclass = "sf") |>
  dplyr::filter(sovereignt != "Antarctica") |>
  dplyr::select(continent, admin, geometry)

# helper: spatial join + nearest-feature fallback for coastal points
join_continent <- function(spat_df, worldmap) {
  joined <- sf::st_join(spat_df, worldmap)
  na_idx <- which(is.na(joined$continent))
  if (length(na_idx) > 0) {
    nn <- sf::st_nearest_feature(spat_df[na_idx, ], worldmap)
    joined[na_idx, c("continent", "admin")] <-
      sf::st_drop_geometry(worldmap)[nn, c("continent", "admin")]
  }
  joined
}

# continent for operating records
spat_operating <- gipt_all |>
  dplyr::filter(status == "operating") |>
  make_spat()

spat_precon_only <- precon_only |>
  make_spat()

cont_operating <- join_continent(spat_operating, worldmap)
cont_precon_q  <- join_continent(spat_precon_only, worldmap)

cat("\n\n=== Continent distribution: operating vs pre-construction ===\n")
cont_compare_raw <- dplyr::bind_rows(
  cont_operating |>
    sf::st_drop_geometry() |>
    dplyr::count(type, continent) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop = n / sum(n), dataset = "operating") |>
    dplyr::ungroup(),
  cont_precon_q |>
    sf::st_drop_geometry() |>
    dplyr::count(type, continent) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop = n / sum(n), dataset = "pre-construction") |>
    dplyr::ungroup()
)

cont_compare_raw |>
  dplyr::mutate(pct = round(prop * 100, 1)) |>
  dplyr::select(dataset, type, continent, n, pct) |>
  dplyr::arrange(type, continent, dataset) |>
  print(n = Inf)

# bar chart comparing proportions
cont_compare_raw |>
  dplyr::filter(!is.na(continent)) |>
  ggplot(aes(x = continent, y = prop, fill = dataset)) +
  geom_col(position = "dodge") +
  facet_wrap(~type) +
  scale_fill_manual(values = c("operating" = "#2166AC", "pre-construction" = "#F4A582")) +
  ggpubr::theme_pubr(legend = "right") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(
    title = "Geographic distribution: operating vs pre-construction facilities",
    x = NULL, y = "Proportion of facilities", fill = "Status"
  )
ggsave("figures/preconstruction_continent_distribution.pdf", width = 9, height = 5)

# Country-level: top countries in pre-construction pipeline
cat("\n=== Top 20 countries by pre-construction facility count ===\n")
cont_precon_q |>
  sf::st_drop_geometry() |>
  dplyr::count(type, admin, country_area) |>
  dplyr::arrange(type, dplyr::desc(n)) |>
  dplyr::group_by(type) |>
  dplyr::slice_head(n = 20) |>
  dplyr::ungroup() |>
  print(n = 40)

rm(spat_operating, spat_precon_only, cont_operating, cont_precon_q, cont_compare_raw)

# ---- 6. Core bias analysis: studies vs facilities ----
# Run hemisphere, latitude-band, and continent bias for both GIPT variants.
# Report ratio = prop_studies / prop_facilities (~1 = no bias).

# Helper functions -----------------------------------------------------------

study_hemi_counts <- function(sysrevlatlong) {
  sysrevlatlong |>
    dplyr::mutate(hemisphere = dplyr::if_else(latitude > 0, "North", "South")) |>
    dplyr::distinct(title, wind_solar, hemisphere) |>
    dplyr::count(wind_solar, hemisphere) |>
    dplyr::group_by(wind_solar) |>
    dplyr::mutate(prop_studies = n / sum(n)) |>
    dplyr::rename(n_studies = n) |>
    dplyr::ungroup()
}

fac_hemi_counts <- function(spat_df, suffix) {
  spat_df |>
    sf::st_drop_geometry() |>
    dplyr::mutate(hemisphere = dplyr::if_else(lat > 0, "North", "South")) |>
    dplyr::count(type, hemisphere) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop_fac = n / sum(n)) |>
    dplyr::rename(wind_solar = type,
                  !!paste0("n_fac_", suffix)    := n,
                  !!paste0("prop_fac_", suffix) := prop_fac) |>
    dplyr::ungroup()
}

study_lat_counts <- function(sysrevlatlong) {
  sysrevlatlong |>
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
}

fac_lat_counts <- function(spat_df, suffix) {
  spat_df |>
    sf::st_drop_geometry() |>
    dplyr::mutate(lat_band = dplyr::case_when(
      abs(lat) < 23.5 ~ "Tropical",
      abs(lat) < 66.5 ~ "Temperate",
      TRUE            ~ "Polar"
    )) |>
    dplyr::count(type, lat_band) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop_fac = n / sum(n)) |>
    dplyr::rename(wind_solar = type,
                  !!paste0("n_fac_", suffix)    := n,
                  !!paste0("prop_fac_", suffix) := prop_fac) |>
    dplyr::ungroup()
}

# helper: continent from pre-built sf join
study_cont_counts <- function(sysrev_sf, worldmap) {
  joined <- join_continent(sysrev_sf, worldmap)
  joined |>
    sf::st_drop_geometry() |>
    dplyr::distinct(title, wind_solar, continent) |>
    dplyr::count(wind_solar, continent) |>
    dplyr::group_by(wind_solar) |>
    dplyr::mutate(prop_studies = n / sum(n)) |>
    dplyr::rename(n_studies = n) |>
    dplyr::ungroup()
}

fac_cont_counts <- function(spat_df, worldmap, suffix) {
  joined <- join_continent(spat_df, worldmap)
  joined |>
    sf::st_drop_geometry() |>
    dplyr::count(type, continent) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop_fac = n / sum(n)) |>
    dplyr::rename(wind_solar = type,
                  !!paste0("n_fac_", suffix)    := n,
                  !!paste0("prop_fac_", suffix) := prop_fac) |>
    dplyr::ungroup()
}

# ---------------------------------------------------------------------------

stud_hemi  <- study_hemi_counts(sysrevlatlong)
fac_hemi_a <- fac_hemi_counts(spat_orig,   "orig")
fac_hemi_b <- fac_hemi_counts(spat_precon, "precon")

hemi_compare <- stud_hemi |>
  dplyr::left_join(fac_hemi_a, by = c("wind_solar", "hemisphere")) |>
  dplyr::left_join(fac_hemi_b, by = c("wind_solar", "hemisphere")) |>
  dplyr::mutate(
    ratio_orig   = round(prop_studies / prop_fac_orig,   3),
    ratio_precon = round(prop_studies / prop_fac_precon, 3)
  )

cat("\n\n=== Hemisphere bias ratios: original vs with pre-construction ===\n")
hemi_compare |>
  dplyr::select(wind_solar, hemisphere, n_studies,
                n_fac_orig, ratio_orig,
                n_fac_precon, ratio_precon) |>
  print()

# latitude bands
stud_lat  <- study_lat_counts(sysrevlatlong)
fac_lat_a <- fac_lat_counts(spat_orig,   "orig")
fac_lat_b <- fac_lat_counts(spat_precon, "precon")

lat_compare <- stud_lat |>
  dplyr::left_join(fac_lat_a, by = c("wind_solar", "lat_band")) |>
  dplyr::left_join(fac_lat_b, by = c("wind_solar", "lat_band")) |>
  dplyr::mutate(
    ratio_orig   = round(prop_studies / prop_fac_orig,   3),
    ratio_precon = round(prop_studies / prop_fac_precon, 3)
  )

cat("\n=== Latitude-band bias ratios: original vs with pre-construction ===\n")
lat_compare |>
  dplyr::select(wind_solar, lat_band, n_studies,
                n_fac_orig, ratio_orig,
                n_fac_precon, ratio_precon) |>
  print()

# continent
stud_cont  <- study_cont_counts(sysrev_sf, worldmap)
fac_cont_a <- fac_cont_counts(spat_orig,   worldmap, "orig")
fac_cont_b <- fac_cont_counts(spat_precon, worldmap, "precon")

cont_compare <- stud_cont |>
  dplyr::full_join(fac_cont_a, by = c("wind_solar", "continent")) |>
  dplyr::full_join(fac_cont_b, by = c("wind_solar", "continent")) |>
  tidyr::replace_na(list(n_studies = 0L, prop_studies = 0)) |>
  dplyr::mutate(
    ratio_orig   = round(prop_studies / prop_fac_orig,   3),
    ratio_precon = round(prop_studies / prop_fac_precon, 3)
  ) |>
  dplyr::arrange(wind_solar, continent)

cat("\n=== Continent bias ratios: original vs with pre-construction ===\n")
cont_compare |>
  dplyr::select(wind_solar, continent, n_studies,
                n_fac_orig, ratio_orig,
                n_fac_precon, ratio_precon) |>
  print(n = Inf)

# ---- 7. Visualise: how much do ratios change? ----
# Dumbbell plot comparing ratios under both GIPT variants.

ratio_long <- dplyr::bind_rows(
  hemi_compare |>
    dplyr::select(wind_solar, strata = hemisphere,
                  ratio_orig, ratio_precon) |>
    dplyr::mutate(strata_type = "Hemisphere"),
  lat_compare |>
    dplyr::select(wind_solar, strata = lat_band,
                  ratio_orig, ratio_precon) |>
    dplyr::mutate(strata_type = "Latitude band"),
  cont_compare |>
    dplyr::filter(!is.na(continent)) |>
    dplyr::select(wind_solar, strata = continent,
                  ratio_orig, ratio_precon) |>
    dplyr::mutate(strata_type = "Continent")
) |>
  tidyr::pivot_longer(
    cols      = c(ratio_orig, ratio_precon),
    names_to  = "dataset",
    names_prefix = "ratio_",
    values_to = "ratio"
  ) |>
  dplyr::mutate(
    dataset     = dplyr::if_else(
      dataset == "orig",
      "Operating + construction + retired",
      "Adds pre-construction"
    ),
    strata_type = factor(strata_type, levels = c("Hemisphere", "Latitude band", "Continent"))
  )

ggplot(ratio_long,
  aes(x = strata, y = ratio,
      colour = wind_solar, shape = dataset)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_point(
    size     = 2.5,
    position = position_dodge(width = 0.5)
  ) +
  facet_wrap(~ strata_type + wind_solar, scales = "free_x", nrow = 3) +
  scale_colour_manual(values = my_colours$windsolar) +
  scale_shape_manual(values = c(
    "Operating + construction + retired" = 16,
    "Adds pre-construction"              = 17
  )) +
  labs(
    title    = "Sensitivity of geographic bias ratios to including pre-construction facilities",
    subtitle = "ratio = prop_studies / prop_facilities; dashed line = no bias (ratio = 1)",
    x = NULL, y = "Ratio (studies / facilities)",
    colour = "Energy type", shape = "GIPT variant"
  ) +
  ggpubr::theme_pubr() +
  theme(
    strip.text  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
ggsave("figures/preconstruction_bias_ratio_sensitivity.pdf", width = 11, height = 10)

# ---- 8. Australia focus: does adding pre-construction change the picture? ----
# The review paper specifically argues Australia is understudied relative to
# its pipeline; does the pre-construction data reinforce this?

cat("\n\n=== Australia: study count vs GIPT facility count ===\n")

# admin_prop for both variants
fac_admin_counts <- function(spat_df, suffix) {
  joined <- join_continent(spat_df, worldmap)
  joined |>
    sf::st_drop_geometry() |>
    dplyr::count(type, admin) |>
    dplyr::group_by(type) |>
    dplyr::mutate(prop_fac = n / sum(n)) |>
    dplyr::rename(wind_solar = type,
                  !!paste0("n_fac_", suffix)    := n,
                  !!paste0("prop_fac_", suffix) := prop_fac) |>
    dplyr::ungroup()
}

stud_admin_join <- join_continent(sysrev_sf, worldmap)
stud_admin <- stud_admin_join |>
  sf::st_drop_geometry() |>
  dplyr::distinct(title, wind_solar, admin) |>
  dplyr::count(wind_solar, admin) |>
  dplyr::group_by(wind_solar) |>
  dplyr::mutate(prop_studies = n / sum(n)) |>
  dplyr::rename(n_studies = n) |>
  dplyr::ungroup()
rm(stud_admin_join)

fac_admin_a <- fac_admin_counts(spat_orig,   "orig")
fac_admin_b <- fac_admin_counts(spat_precon, "precon")

admin_compare <- stud_admin |>
  dplyr::full_join(fac_admin_a, by = c("wind_solar", "admin")) |>
  dplyr::full_join(fac_admin_b, by = c("wind_solar", "admin")) |>
  tidyr::replace_na(list(n_studies = 0L, prop_studies = 0)) |>
  dplyr::mutate(
    ratio_orig   = round(prop_studies / prop_fac_orig,   3),
    ratio_precon = round(prop_studies / prop_fac_precon, 3)
  )

dplyr::filter(admin_compare, admin == "Australia") |>
  dplyr::select(wind_solar, admin, n_studies, n_fac_orig, ratio_orig,
                n_fac_precon, ratio_precon) |>
  print()

# ---- 9. Figure: continent-level ratios side by side ----
# Clean publication-quality dot plot comparing the two variants at continent level

cont_plot <- cont_compare |>
  dplyr::filter(!is.na(continent)) |>
  tidyr::pivot_longer(
    cols      = c(ratio_orig, ratio_precon),
    names_to  = "dataset",
    names_prefix = "ratio_",
    values_to = "ratio"
  ) |>
  dplyr::mutate(dataset = dplyr::if_else(
    dataset == "orig",
    "Operating + construction + retired",
    "Adds pre-construction"
  ))

ggplot(cont_plot,
  aes(x = ratio, y = continent, colour = wind_solar, shape = dataset)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(
    aes(group = interaction(continent, wind_solar)),
    colour = "grey70", linewidth = 0.5
  ) +
  geom_point(size = 3) +
  scale_colour_manual(values = my_colours$windsolar) +
  scale_shape_manual(values = c(
    "Operating + construction + retired" = 16,
    "Adds pre-construction"              = 17
  )) +
  facet_wrap(~wind_solar) +
  labs(
    x      = "Ratio (prop. studies / prop. facilities)",
    y      = NULL,
    colour = "Energy type",
    shape  = "Facility baseline",
    title  = "Continent-level geographic bias",
    subtitle = "Dashed line = no bias (ratio = 1); lines connect same continent across GIPT variants"
  ) +
  ggpubr::theme_pubr()
ggsave("figures/preconstruction_continent_ratio_dumbbell.pdf", width = 9, height = 5)

# ---- 10. Summary table for reporting ----
cat("\n\n=== SUMMARY: does adding pre-construction change conclusions? ===\n")
cat("Key comparisons where ratio shifts notably (> 0.15 absolute change):\n")

summary_shifts <- dplyr::bind_rows(
  hemi_compare |>
    dplyr::transmute(wind_solar, strata = hemisphere, strata_type = "Hemisphere",
                     ratio_orig, ratio_precon),
  lat_compare |>
    dplyr::transmute(wind_solar, strata = lat_band, strata_type = "Latitude band",
                     ratio_orig, ratio_precon),
  cont_compare |>
    dplyr::filter(!is.na(continent)) |>
    dplyr::transmute(wind_solar, strata = continent, strata_type = "Continent",
                     ratio_orig, ratio_precon)
) |>
  dplyr::mutate(
    delta = abs(ratio_precon - ratio_orig)
  ) |>
  dplyr::filter(!is.na(ratio_orig), !is.na(ratio_precon)) |>
  dplyr::arrange(dplyr::desc(delta))

print(summary_shifts, n = 20)

# ---- clean up ----
rm(stud_hemi, stud_lat, stud_cont, stud_admin,
   fac_hemi_a, fac_hemi_b,
   fac_lat_a,  fac_lat_b,
   fac_cont_a, fac_cont_b,
   fac_admin_a, fac_admin_b,
   hemi_compare, lat_compare, cont_compare, admin_compare,
   cont_plot, ratio_long, summary_shifts,
   spat_orig, spat_precon,
   sysrev_sf, sysrevlatlong, sysrevdata,
   gipt_all, gipt_orig, gipt_precon, precon_only,
   worldmap)
