# biodiversity-wind-solar

Data and analysis code for:

**From global evidence to local action: biodiversity impacts and mitigation for utility-scale wind and solar farms, with an Australian case study**

Ruby E. Stephens, Malan Bothma, Neil R. Jordan, Will Cornwell

Centre for Ecosystem Science and Evolution & Ecology Research Centre, University of New South Wales, Sydney, Australia

*Paper submitted; citation and DOI will be added here when published.*

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXXX)

## Overview

This repository contains two linked analyses:

1. **Systematic map** of the global literature on the biodiversity impacts of utility-scale onshore wind and solar farms and their mitigation (331 studies). It covers where and when studies were done compared with where facilities have been built (using Global Energy Monitor's wind and solar power trackers), which organisms and biodiversity measures were studied, the direction of effects, and suggested and tested mitigation.
2. **Spatial overview** of Australian utility-scale wind and solar farms (262 facilities), describing the biomes, climate, and present-day and pre-1750 vegetation of the land they occupy, and how much tree cover was lost within their footprints. Footprints come from Global Renewables Watch.

## Re-running the analysis

1. Clone or download this repository and open `biodiversity-wind-solar.Rproj` in RStudio, so that all file paths are relative to the project folder.
2. Install the packages needed by running `scripts/install_packages.R`.
3. Run `main.R`, which loads the packages and runs the analysis scripts in order.

The analysis was last run with R 4.6.0 on macOS; package versions are recorded in `sessionInfo.txt`.

Slow or data-heavy steps are cached. `scripts/functions/cache_csv.R` reads a prepared csv from `data_output/` if it exists, and only re-runs the code that creates it if the csv is missing. With the cached files in this repository, `main.R` runs without the large spatial data in `data_cache/`, except for:

- the climate density plots in `spatial_figures.R` (Figure 5b, c), which need the CHELSA rasters, and
- the biome comparison of studies and facilities in `sysreview_analysis.R` (Figure S1), which needs the Ecoregions2017 shapefile.

To rebuild the cached spatial data (`data_output/windsolar_environments.csv` and `data_output/footprint_classes.csv`), delete them and download the files listed in `data_cache/data_cache_readme` into `data_cache/`. These are not included here because of their size (up to 3 GB each) and are freely available from their sources.

## Repository structure

| Folder or file | Contents |
|---|---|
| `main.R` | Runs the whole analysis |
| `data_input/` | Raw data, described in `data_input/data_input_readme` |
| `data_cache/` | Large spatial data not included in the repository; sources in `data_cache/data_cache_readme` |
| `data_output/` | Data prepared by the scripts (cached) and tables for the paper |
| `figures/` | Figures produced by the scripts |
| `scripts/` | Analysis scripts, described below |
| `sessionInfo.txt` | R and package versions used |

### Scripts

| Script | What it does |
|---|---|
| `install_packages.R` | Installs the R packages used |
| `functions/cache_csv.R` | Function to cache prepared data as csv |
| `my_colours.R` | Colours used in figures |
| `sysreview_dataprep.R` | Splits the Rayyan screening export into columns as a starting point for data extraction (not re-run by `main.R`) |
| `sysreview_analysis.R` | Systematic map analyses: publication and development years, study locations vs facility locations by continent and biome, biodiversity categories and measures, effects, and mitigation |
| `gipt_data.R` | Reads and filters Global Energy Monitor wind and solar tracker data (February 2025) for global facility locations |
| `global_windsolar_map.R` | Global maps of wind and solar facilities |
| `spatial_dataprep.R` | Prepares the Australian facility list (Global Energy Monitor, June 2024, checked manually), matches facilities to Global Renewables Watch footprints, and extracts biome, climate, vegetation, land capability and tree cover for each facility |
| `spatial_figures.R` | Figures and summary tables for the Australian spatial overview |
| `supplementary_analyses/` | Checks of data choices, not run by `main.R`: `compare_windsolar_datasources.R` (Global Energy Monitor vs National Map data for Australia), `compare_gpkg_gipt.R` (Global Energy Monitor points vs Global Renewables Watch footprints for the global comparison; needs `data_cache/`), `preconstruction_gipt_analysis.R` (effect of including pre-construction projects in the study vs facility comparison) |

### Figures and tables in the paper

| Paper | File |
|---|---|
| Figure 1 | `figures/global_map_wind_gipt.pdf`, `figures/global_map_solar_gipt.pdf`, `figures/map_study_locations.pdf` |
| Figure 2 | `figures/yearpub_yeardev_hist_combined.pdf` |
| Figure 3 | `figures/biodiversity_catmeas_combined.pdf` |
| Figure 4 | `figures/mitigation_combined.pdf` |
| Figure 5 | `figures/map_windsolar_aus_biomes.pdf` (a), `figures/temp_precip_density_plots.pdf` (b, c) |
| Figure S1 | `figures/biome_prop_studiesvGIPT.pdf` |
| Figure S2 | `figures/aus_area_by_mvg_combined.pdf` |
| Table 1 | `data_output/studies_vs_facilities_by_continent.csv` |

Other figures in `figures/` are exploratory and not used in the paper.

## Data sources

The systematic map data (`data_input/wind_solar_biodiversity_sysreview_v8_20260602.csv`) were extracted from the included studies by Ruby Stephens and Malan Bothma. Other data come from:

- Global Energy Monitor, Global Wind Power Tracker and Global Solar Power Tracker (with TransitionZero), June 2024 and February 2025 releases (CC BY 4.0)
- Global Renewables Watch (Robinson et al. 2025)
- National Vegetation Information System v7.0 (Australian Government DCCEEW)
- CHELSA v2.1 climatologies (Karger et al. 2017)
- Global Forest Change v1.11 (Hansen et al. 2013)
- Ecoregions2017 (Dinerstein et al. 2017)
- IBRA v7 bioregions (Australian Government)
- Mapped Agricultural Land Capability for Australia (Adams & Engert 2023)
- National Map (nationalmap.gov.au)

Full details, versions, links and citations are in `data_input/data_input_readme` and `data_cache/data_cache_readme`. Please cite the original sources if you reuse these data.

## Citation

If you use this code or data, please cite the preprint:

Stephens, R.E., Bothma, M., Jordan, N.R. & Cornwell, W. (2026) From global evidence to local action: biodiversity impacts and mitigation for utility-scale wind and solar farms, with an Australian case study. Zenodo. https://doi.org/10.5281/zenodo.XXXXXXX

## Licence

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/) (see `LICENSE`). Third-party data in `data_input/` remain under their original licences.
