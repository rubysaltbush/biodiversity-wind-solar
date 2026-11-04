# script to produce figures from pre-prepared (cached)  data on Australian 
# wind and solar farms and their environments

# aus biome map ----

# read in shapefile of major Australian biomes, borders = IBRA subregions
biomes <- sf::st_read("data_input/ibra7_biomes_background.shp")

# make sure all crs the same
windsolar <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
sf::st_crs(biomes) <- "EPSG:4326"

# make map!
ggplot() +
  geom_sf(data = biomes, aes(fill = biome), linewidth = 0.01) +
  scale_fill_grey(start = 1, end = 0) +
  geom_sf(data = windsolar, aes(shape = type, colour = type, size = totalcapacity_mw)) +
  scale_colour_discrete(type = my_colours$windsolar, name = "facility type") +
  scale_shape(solid = FALSE, name = "facility type") +
  scale_size(name = "total capacity (mw)") +
  theme_void()
ggsave("figures/map_windsolar_aus_biomes.pdf", width = 8, height = 7)

# drop windsolar geometry, not needed from here
windsolar <- sf::st_drop_geometry(windsolar)

# simplify to a df of biome with #s of wind and solar facilities in them
# also summed maximum capacity (mw) of facilities
windsolar_biomesummary <- windsolar %>%
  dplyr::select(biome, type, totalcapacity_mw) %>%
  dplyr::group_by(biome, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup()
windsolar_biomesummary
# biome                                                        type  number sumcapmw
# <chr>                                                        <chr>  <int>    <dbl>
# 1 Deserts and Xeric Shrublands                                 solar     17     825 
# 2 Deserts and Xeric Shrublands                                 wind      14    1865 
# 3 Mediterranean Forests, Woodlands and Scrub                   solar     31    1917.
# 4 Mediterranean Forests, Woodlands and Scrub                   wind      30    2748.
# 5 Temperate Broadleaf and Mixed Forest                         solar     42    3615.
# 6 Temperate Broadleaf and Mixed Forest                         wind      61    8564.
# 7 Temperate Grasslands, Savannas and Shrublands                solar     14    1196.
# 8 Tropical and Subtropical Grasslands, Savannas and Shrublands solar     45    3640.
# 9 Tropical and Subtropical Grasslands, Savannas and Shrublands wind       6    1556 
# 10 Tropical and Subtropical Moist Broadleaf Forests            wind       2     169 

# wind/solar number facilities by biome stacked barchart
windsolar_biomesummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(biome = forcats::fct_inorder(biome)) %>%
  ggplot(aes(x = biome, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Biome") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_biome.pdf", width = 9, height = 7)
# interesting that temperate forest is most represented biome
# given deserts is our biggest biome by land area
# bias of population centres, also concentrated in temp forest biome

# stacked barchart wind/solar max capacity by biome
windsolar_biomesummary %>%
  dplyr::arrange(-sumcapmw) %>%
  dplyr::mutate(biome = forcats::fct_inorder(biome)) %>%
  ggplot(aes(x = biome, y = sumcapmw, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Biome") +
  ylab("Total maximum capacity (MW)") +
  coord_flip()
ggsave("figures/aus_capacitymw_by_biome.pdf", width = 9, height = 7)
# overall wind power has bigger share when measured by maximum capacity rather 
# than just number of facilities, but wind has a smaller footprint per MW than
# solar so capacity isn't entirely useful when thinking about area affected

# enough of biomes, let's use better veg mapping
# clean up df not needed anymore
rm(windsolar_biomesummary, biomes)

# footprint area ----

# number, total area and median area per MW of GRW footprints by facility type
windsolar %>%
  dplyr::filter(footprint_source == "GRW") %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(number = n(), total_area_ha = sum(footprint_ha),
                   median_ha_per_mw = median(footprint_ha / totalcapacity_mw))
# type  number total_area_ha median_ha_per_mw
# solar    132        14763.            1.77 
# wind     100         3105.            0.263
# so yes, solar uses more land area per MW than wind

# NVIS vegetation ----

## present day ----
# rasters too large to produce map without difficulty, maybe not needed?

# simplify to a df of MVG with #s and capacity of wind and solar facilities by
# majority MVG in each footprint, and total area of GRW footprints in each MVG
mvg_windsolarsummary <- windsolar %>%
  dplyr::select(mvg_fp_simplified, type, totalcapacity_mw) %>%
  dplyr::group_by(mvg_fp_simplified, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(footprint_classes %>%
                     dplyr::filter(layer == "mvg", footprint_source == "GRW") %>%
                     dplyr::group_by(mvg_fp_simplified = class_name, type) %>%
                     dplyr::summarise(area_ha = sum(area_ha)) %>%
                     dplyr::ungroup(),
                   by = c("mvg_fp_simplified", "type")) %>%
  tidyr::replace_na(list(number = 0, sumcapmw = 0, area_ha = 0))
mvg_windsolarsummary
# mvg_fp_simplified                              type  number sumcapmw    area_ha
# 1 Bare ground, modified native vegetation, other solar      1      60    127.    
# 2 Bare ground, modified native vegetation, other wind       1     118     40.7   
# 3 Cleared, non-native vegetation, buildings      solar    127    9710  13238.    
# 4 Cleared, non-native vegetation, buildings      wind      93   12566.  2400.    
# 5 Eucalypt forests and woodlands                 solar      4     224.   223.    
# 6 Eucalypt forests and woodlands                 wind      10    1381    393.    
# 7 Grasslands                                     solar      7     560.   455.    
# 8 Grasslands                                     wind       2     257     77.4   
# 9 Non-eucalypt forests and woodlands             solar      4     179.   242.    
# 10 Non-eucalypt forests and woodlands             wind       1      66    27.6   
# 11 Shrublands                                     solar      6     460   478.    
# 12 Shrublands                                     wind       6     514.  166.    
# 13 Aquatic vegetation and salt lakes              wind       0       0     0.0851
# 14 Rainforests and Vine Thickets                  wind       0       0     1.01  
# 15 NA                                             wind       0       0     0.0292

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::filter(number >0) %>%
  dplyr::mutate(mvg_fp_simplified = forcats::fct_inorder(mvg_fp_simplified)) %>%
  ggplot(aes(x = mvg_fp_simplified, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group - 2024") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_mvg.pdf", width = 9, height = 7)

# total footprint area by present-day vegetation
plotmvgarea <- mvg_windsolarsummary %>%
  dplyr::filter(number >0) %>%
  dplyr::mutate(mvg_fp_simplified = forcats::fct_reorder(mvg_fp_simplified, 
                                                         area_ha, .fun = sum,
                                                         .desc = TRUE)) %>%
  ggplot(aes(x = mvg_fp_simplified, y = area_ha, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group - 2024") +
  ylab("Total footprint area (ha)") +
  coord_flip() +
  scale_x_discrete(labels = scales::label_wrap(width = 25))
plotmvgarea
ggsave("figures/aus_area_by_mvg.pdf", width = 9, height = 7)

## pre-1750 ----
# try same with pre-1750 mapping

# simplify to a df of MVG with #s and capacity of wind and solar facilities by
# majority MVG in each footprint, and total area of GRW footprints in each MVG
mvg_windsolarsummary <- windsolar %>%
  dplyr::select(mvgpre1750_fp_simplified, type, totalcapacity_mw) %>%
  dplyr::group_by(mvgpre1750_fp_simplified, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(footprint_classes %>%
                     dplyr::filter(layer == "mvgpre1750", footprint_source == "GRW") %>%
                     dplyr::group_by(mvgpre1750_fp_simplified = class_name, type) %>%
                     dplyr::summarise(area_ha = sum(area_ha)) %>%
                     dplyr::ungroup(),
                   by = c("mvgpre1750_fp_simplified", "type")) %>%
  tidyr::replace_na(list(number = 0, sumcapmw = 0, area_ha = 0))
mvg_windsolarsummary
# mvgpre1750_fp_simplified                       type  number sumcapmw    area_ha
# 1 Bare ground, modified native vegetation, other wind       1     107     25.0   
# 2 Eucalypt forests and woodlands                 solar    110    8253. 11267.    
# 3 Eucalypt forests and woodlands                 wind      69    9336.  1701.    
# 4 Grasslands                                     solar     14     800.  1005.    
# 5 Grasslands                                     wind      17    2814    592.    
# 6 Non-eucalypt forests and woodlands             solar     13    1182.  1520.    
# 7 Non-eucalypt forests and woodlands             wind       4     531     98.4   
# 8 Rainforests and Vine Thickets                  solar      1      40      0     
# 9 Rainforests and Vine Thickets                  wind       2     124     63.4   
# 10 Shrublands                                     solar     11     918.   964.    
# 11 Shrublands                                     wind      20    1990.   600.    
# 12 Aquatic vegetation and salt lakes              solar      0       0      6.59  
# 13 Aquatic vegetation and salt lakes              wind       0       0     24.0   
# 14 NA                                             solar      0       0      0.101 
# 15 NA                                             wind       0       0      0.0292

# 1 solar facility (Kingaroy solar farm) in rainforest and vine thicket but 0 area_ha
# because no footprint for it in GRW, built in 2024

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvgpre1750_fp_simplified = forcats::fct_inorder(mvgpre1750_fp_simplified)) %>%
  dplyr::filter(number >0) %>% # remove area where no. facilities = 0
  ggplot(aes(x = mvgpre1750_fp_simplified, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group - pre-1750") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_mvgpre1750.pdf", width = 9, height = 7)


# total footprint area by pre-1750 vegetation
plotpre1750area <- mvg_windsolarsummary %>%
  dplyr::mutate(mvgpre1750_fp_simplified = forcats::fct_reorder(mvgpre1750_fp_simplified,
                                                                area_ha, .fun = sum,
                                                                .desc = TRUE)) %>%
  dplyr::filter(number >0) %>% # remove area where no. facilities = 0
  ggplot(aes(x = mvgpre1750_fp_simplified, y = area_ha, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group - pre-1750") +
  ylab("Total footprint area (ha)") +
  coord_flip() +
  scale_x_discrete(labels = scales::label_wrap(width = 25))
plotpre1750area
ggsave("figures/aus_area_by_mvgpre1750.pdf", width = 9, height = 7)

# combine pre-1750 and present-day vegetation area plots
combined <- plotpre1750area / plotmvgarea + 
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")
combined
ggsave("figures/aus_area_by_mvg_combined.pdf", combined, width = 5, height = 7)
rm(plotpre1750area, plotmvgarea, mvg_windsolarsummary, combined)

# land capability mapping (Adams & Engert 2023) ----

## bar charts ----

# simplify to a df of land capability with #s of wind and solar facilities in them
landcap_windsolarsummary <- windsolar %>%
  dplyr::select(land_capability_fp, type, totalcapacity_mw) %>%
  dplyr::group_by(land_capability_fp, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup() %>%
  dplyr::full_join(footprint_classes %>%
                     dplyr::filter(layer == "land_capability", footprint_source == "GRW") %>%
                     dplyr::group_by(land_capability_fp = class, type) %>%
                     dplyr::summarise(area_ha = sum(area_ha)) %>%
                     dplyr::ungroup(),
                   by = c("land_capability_fp", "type")) %>%
  tidyr::replace_na(list(number = 0, sumcapmw = 0, area_ha = 0))
landcap_windsolarsummary
#   land_capability_fp type  number sumcapmw area_ha
#                   1 solar      3     370   504.  
#                   1 wind      10     927   314.  
#                   2 solar     33    1826. 2933.  
#                   2 wind      25    3496   592.  
#                   3 solar     32    2552. 3007.  
#                   3 wind      18    2129.  471.  
#                   4 solar     21    1613. 2084.  
#                   4 wind      24    2976.  736.  
#                   5 solar     25    2356. 2397.  
#                   5 wind      13    1341   292.  
#                   6 solar     23    1788. 3025.  
#                   6 wind      17    2897.  565.  
#                   7 solar      9     573.  665.  
#                   7 wind       4     827.   86.6 
#                   8 solar      3     115.  147.  
#                   8 wind       1     197    14.9 
#                   9 wind       1     112    33.3 
#                   9 solar      0       0     3.22

# now display in stacked bar chart
landcap_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  ggplot(aes(x = land_capability_fp, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Agricultural land capability") +
  ylab("Number of facilities") +
  scale_x_continuous(breaks = c(1:9)) +
  coord_flip()
ggsave("figures/aus_facilities_by_landcapability.pdf", width = 9, height = 7)

# total footprint area by land capability class
landcap_windsolarsummary %>%
  ggplot(aes(x = land_capability_fp, y = area_ha, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Agricultural land capability") +
  ylab("Total footprint area (ha)") +
  scale_x_continuous(breaks = c(1:9)) +
  coord_flip()
ggsave("figures/aus_area_by_landcapability.pdf", width = 9, height = 7)

rm(landcap_windsolarsummary)

# climate density graphs ----

# below adapted from Will's code, took out of function so can retain
# climate data with wind/solar data

# load rasters of mean annual temperature and rainfall (crs WGS1984, same as windsolar df)
temp_raster <- terra::rast("data_cache/CHELSA_bio1_1981-2010_V.2.1.tif")
rain_raster <- terra::rast("data_cache/CHELSA_bio12_1981-2010_V.2.1.tif")

# Load the region polygon
region_polygon <- rnaturalearth::ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

# convert region to SpatVector for terra operations
region_polygon <- terra::vect(region_polygon)

# crop rasters to the region extent
temp_raster <- terra::crop(temp_raster, terra::ext(region_polygon))
rain_raster <- terra::crop(rain_raster, terra::ext(region_polygon))

# Mask the rasters with the region shape
temp_raster <- terra::mask(temp_raster, region_polygon)
rain_raster <- terra::mask(rain_raster, region_polygon)
rm(region_polygon)

# Convert the full rasters to a data frame
temp_all <- terra::as.data.frame(temp_raster)
rain_all <- terra::as.data.frame(rain_raster)
rm(temp_raster, rain_raster)
temp_all <- temp_all %>%
  dplyr::rename(MAT = `CHELSA_bio1_1981-2010_V.2.1`) %>%
  dplyr::mutate(type = "All Australia")
rain_all <- rain_all %>%
  dplyr::rename(MAP = `CHELSA_bio12_1981-2010_V.2.1`) %>%
  dplyr::mutate(type = "All Australia")
# append wind and solar data
temp_all <- windsolar %>%
  dplyr::select(type, MAT) %>%
  dplyr::bind_rows(temp_all)
rain_all <- windsolar %>%
  dplyr::select(type, MAP) %>%
  dplyr::bind_rows(rain_all)

# now create density plots of these data
# temperature first
temp_plot <- ggplot(temp_all, aes(x = MAT, colour = type, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean annual temperature (ºC)",
       y = "Density",
       colour = "",
       fill = "") +
  scale_fill_discrete(type = c("grey", "#FACB0F", "#377EB8"),
                      labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  scale_colour_discrete(type = c("grey", "#FACB0F", "#377EB8"),
                        labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  ggpubr::theme_pubr(legend = "right")

# then precipitation
rain_plot <- ggplot(rain_all, aes(x = MAP, colour = type, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean annual precipitation (mm)",
       y = "Density",
       colour = "",
       fill = "") +
  scale_x_log10() +
  scale_fill_discrete(type = c("grey", "#FACB0F", "#377EB8"),
                      labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  scale_colour_discrete(type = c("grey", "#FACB0F", "#377EB8"),
                        labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  ggpubr::theme_pubr(legend = "none")

# use patchwork package to stitch plots together, and export
temp_plot / rain_plot + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")
ggsave("figures/temp_precip_density_plots.pdf", width = 5, height = 8)
# plots slow to draw because 10million+ points in density graph
rm(rain_plot, rain_all, temp_plot, temp_all)

# hansen tree cover ----

# mean tree cover in 2000 and tree cover loss 2000-2023 within footprints of
# facilities first operating from 2001
windsolar %>%
  dplyr::filter(earliest_start >= 2001) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(number = n(),
                   mean_treecover2000 = mean(treecover2000_fp),
                   mean_pct_treeloss = 100 * mean(frac_treeloss_fp),
                   total_treeloss_ha = sum(frac_treeloss_fp * footprint_ha),
                   n_with_treeloss = sum(frac_treeloss_fp > 0))
#    type  number mean_treecover2000 mean_pct_treeloss total_treeloss_ha n_with_treeloss
# 1 solar    149               1.29              1.79              359.              42
# 2 wind     111               4.41              4.24              186.              67
# solar tends to have been built more on areas with less tree cover in yr 2000
# therefore less tree loss for solar than for wind to date

# what was tree cover at locations in 2000?
# ridgeline plot
ggplot(windsolar, aes(x = treecover2000_fp, y = type, fill = type)) +
  ggridges::geom_density_ridges() +
  ggridges::theme_ridges() + 
  scale_fill_discrete(type = my_colours$windsolar) +
  ggpubr::theme_pubr(legend = "none") +
  xlab("Tree cover within footprint in year 2000 (%)") +
  ylab("Facility type")
ggsave("figures/fraction_treecover2000_ridgeline.pdf", width = 6, height = 5)
# so wind facilities have more tree cover than solar in surrounding area
# to start with

# prepare fraction cleared column
windsolar$percent_cleared <- windsolar$frac_treeloss_fp*100

# ridgeline plot
ggplot(windsolar, aes(x = percent_cleared, y = type, fill = type)) +
  ggridges::geom_density_ridges() +
  ggridges::theme_ridges() + 
  scale_fill_discrete(type = my_colours$windsolar) +
  ggpubr::theme_pubr(legend = "none") +
  xlab("Tree cover loss within footprint 2000-2023 (%)") +
  ylab("Facility type")
ggsave("figures/fraction_cleared_ridgeline.pdf", width = 6, height = 5) 
# so, doesn't look like solar or wind farms are associated with significant clearing
# of tree cover in surrounding area

rm(windsolar, footprint_classes)

