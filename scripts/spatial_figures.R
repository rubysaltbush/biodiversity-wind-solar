# script to produce figures from cached spatial data from pre-prepared
# spatial data on Australian wind and solar farm environments


# aus outline ----

# read in shapefile of Aus outline
aus <- terra::vect("data_input/Aus_outline.shp")
# bit slow and over complex, simplify to make faster
aus <- terra::simplifyGeom(aus, tolerance = 0.01)
# crop to remove Norfolk and Christmas Islands from outline
aus <- terra::crop(aus, terra::ext(c(xmin = 112, xmax = 155, ymin = -43.74, ymax = -9.14)))
# convert aus to sf for easy plotting
aus <- sf::st_as_sf(aus)

# biomes ----
# read in shapefile of Australian biomes, borders = IBRA subregions
biomes <- sf::st_read("data_input/ibra7_biomes_background.shp")

# make sure all crs the same
windsolar <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
sf::st_crs(aus) <- "EPSG:4326"
sf::st_crs(biomes) <- "EPSG:4326"

# make map!
ggplot() +
  geom_sf(data = aus, fill = NA, linewidth = 0.1) +
  geom_sf(data = biomes, aes(fill = biome), linewidth = 0.01) +
  scale_fill_grey(start = 1, end = 0) +
  geom_sf(data = windsolar, aes(shape = type, colour = type, size = totalcapacity_mw)) +
  scale_colour_discrete(type = my_colours$windsolar, name = "facility type") +
  scale_shape(name = "facility type") +
  scale_size(name = "total capacity (mw)") +
  theme_void()
ggsave("figures/map_windsolar_aus_biomes.png", width = 8, height = 7)

# simplify to a df of biome with #s of wind and solar facilities in them
# also summed maximum capacity (mw) of facilities
windsolar_biomesummary <- windsolar %>%
  sf::st_drop_geometry() %>%
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

# now display in stacked bar chart
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
ggsave("figures/aus_facilities_by_biome.png", width = 9, height = 7)
# interesting that temperate forest is most represented biome
# given deserts is our biggest biome by land area
# bias of population centres, also concentrated in temp forest biome

# now display in stacked bar chart
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
ggsave("figures/aus_capacitymw_by_biome.png", width = 9, height = 7)
# overall wind power has bigger share when measured by maximum capacity rather 
# than just number of facilities

# enough of biomes, let's use better veg mapping
# clean up df not needed anymore
rm(windsolar_biomesummary, biomes)

# NVIS vegetation ----

## present day ----

# simplify to a df of MVG with #s of wind and solar facilities in them
mvg_windsolarsummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mvg_simplified, type, totalcapacity_mw) %>%
  dplyr::group_by(mvg_simplified, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup()
mvg_windsolarsummary
# mvg_simplified                            type  number sumcapmw
# <chr>                                     <chr>  <int>    <dbl>
#   1 Cleared, non-native vegetation, buildings solar    126    9706.
# 2 Cleared, non-native vegetation, buildings wind      91   12034.
# 3 Eucalypt forests and woodlands            solar      5     228.
# 4 Eucalypt forests and woodlands            wind      12    1913 
# 5 Grasslands                                solar      7     560.
# 6 Grasslands                                wind       1     113 
# 7 Non-eucalypt forests and woodlands        solar      4     179.
# 8 Other                                     solar      1      60 
# 9 Other                                     wind       3     328 
# 10 Shrublands                                solar      6     460 
# 11 Shrublands                                wind       6     514.

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvg_simplified = forcats::fct_inorder(mvg_simplified)) %>%
  ggplot(aes(x = mvg_simplified, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_mvg.png", width = 9, height = 7)

# now display capacity in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-sumcapmw) %>%
  dplyr::mutate(mvg_simplified = forcats::fct_inorder(mvg_simplified)) %>%
  ggplot(aes(x = mvg_simplified, y = sumcapmw, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group") +
  ylab("Total maximum capacity (MW)") +
  coord_flip()
ggsave("figures/aus_capacity_by_mvg.png", width = 9, height = 7)
# looking at it by capacity changes the order of vegetation groups affected
# though all are still dwarfed by the cleared category

## pre-1750 ----
# try same with pre-1750 mapping

# simplify to a df of MVG with #s of wind and solar facilities in them
mvg_windsolarsummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mvgpre1750_simplified, type, totalcapacity_mw) %>%
  dplyr::group_by(mvgpre1750_simplified, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup()
mvg_windsolarsummary
# mvgpre1750_simplified              type  number sumcapmw
# <chr>                              <chr>  <int>    <dbl>
#   1 Eucalypt forests and woodlands     solar    110    8931.
# 2 Eucalypt forests and woodlands     wind      68    9118.
# 3 Grasslands                         solar     15     806.
# 4 Grasslands                         wind      21    3467 
# 5 Non-eucalypt forests and woodlands solar     13     847.
# 6 Non-eucalypt forests and woodlands wind       2      96 
# 7 Other                              wind       2     173 
# 8 Rainforests and Vine Thickets      solar      1      40 
# 9 Rainforests and Vine Thickets      wind       3     376 
# 10 Shrublands                         solar     10     570.
# 11 Shrublands                         wind      17    1672.

# now display in stacked bar chart
mvg_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  dplyr::mutate(mvgpre1750_simplified = forcats::fct_inorder(mvgpre1750_simplified)) %>%
  ggplot(aes(x = mvgpre1750_simplified, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group (pre-1750)") +
  ylab("Number of facilities") +
  coord_flip()
ggsave("figures/aus_facilities_by_mvgpre1750.png", width = 9, height = 7)

# and stacked bar chart by total capacity
mvg_windsolarsummary %>%
  dplyr::arrange(-sumcapmw) %>%
  dplyr::mutate(mvgpre1750_simplified = forcats::fct_inorder(mvgpre1750_simplified)) %>%
  ggplot(aes(x = mvgpre1750_simplified, y = sumcapmw, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Major Vegetation Group (pre-1750)") +
  ylab("Total maximum capacity (MW)") +
  coord_flip()
ggsave("figures/aus_capacity_by_mvgpre1750.png", width = 9, height = 7)

# again using capacity increases the dominance of wind (implying that there are
# fewer but larger capacity wind farms vs solar farms)
# shifts ranking of smaller cateogries but eucalypt woodlands still predominate

rm(mvg_windsolarsummary)

# land capability mapping (Adams & Engert 2023) ----

# simplify to a df of land capability with #s of wind and solar facilities in them
landcap_windsolarsummary <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(land_capability, type, totalcapacity_mw) %>%
  dplyr::group_by(land_capability, type) %>%
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup()

# now display in stacked bar chart
landcap_windsolarsummary %>%
  dplyr::arrange(-number) %>%
  ggplot(aes(x = land_capability, y = number, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Agricultural land capability") +
  ylab("Number of facilities") +
  scale_x_continuous(breaks = c(1:9)) +
  coord_flip()
ggsave("figures/aus_facilities_by_landcapability.png", width = 9, height = 7)

# now display in stacked bar chart, with capacity instead
landcap_windsolarsummary %>%
  dplyr::arrange(-sumcapmw) %>%
  ggplot(aes(x = land_capability, y = sumcapmw, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Agricultural land capability") +
  ylab("Total maximum capacity (MW)") +
  scale_x_continuous(breaks = c(1:9)) +
  coord_flip()
ggsave("figures/aus_facilities_by_landcapability.png", width = 9, height = 7)
# again increases proportion of wind, interesting that on the best 
# agricultural land it is mostly wind farms not solar, which is probably more
# compatible with continuing agriculture

rm(landcap_windsolarsummary)

# climate density graphs ----

# below adapted from Will's code, took out of function so can retain
# climate data with wind/solar data
# MIGHT HAVE TO REWRITE TO SEPARATE INTO PREP/FIGURE

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

# Extract raster values at the points
windsolar_temp <- terra::extract(temp_raster, windsolar)
windsolar_rain <- terra::extract(rain_raster, windsolar)

# join these values back to windsolar df
# join back onto data
windsolar <- windsolar %>%
  dplyr::bind_cols(windsolar_temp) %>%
  dplyr::select(-ID) %>%
  dplyr::bind_cols(windsolar_rain) %>%
  dplyr::select(-ID)
rm(windsolar_temp, windsolar_rain)

# Convert the full rasters to a data frame
temp_all <- terra::as.data.frame(temp_raster)
rain_all <- terra::as.data.frame(rain_raster)
rm(temp_raster, rain_raster)
temp_all <- dplyr::mutate(temp_all, type = "All Australia")
rain_all <- dplyr::mutate(rain_all, type = "All Australia")
# append wind and solar data
temp_all <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(type, `CHELSA_bio1_1981-2010_V.2.1`) %>%
  dplyr::bind_rows(temp_all) %>%
  dplyr::rename(MAT = `CHELSA_bio1_1981-2010_V.2.1`)
rain_all <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(type, `CHELSA_bio12_1981-2010_V.2.1`) %>%
  dplyr::bind_rows(rain_all) %>%
  dplyr::rename(MAP = `CHELSA_bio12_1981-2010_V.2.1`)

# now create density plots of these data
# temperature first
temp_plot <- ggplot(temp_all, aes(x = MAT, colour = type, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean annual temperature (ºC)",
       y = "Density",
       colour = "",
       fill = "") +
  scale_fill_discrete(labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  scale_colour_discrete(labels = c('All Australia', 'Solar farms', 'Wind farms')) +
  ggpubr::theme_pubr(legend = "right")

# then precipitation
rain_plot <- ggplot(rain_all, aes(x = MAP, colour = type, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(x = "Mean annual precipitation (mm)",
       y = "Density",
       colour = "",
       fill = "") +
  scale_x_log10() +
  ggpubr::theme_pubr(legend = "none")

# use patchwork package to stitch plots together, and export
temp_plot + rain_plot
ggsave("figures/temp_precip_density_plots.pdf", width = 9, height = 4.5)
# plots slow to draw because 10million+ points in density graph
rm(rain_plot, rain_all, temp_plot, temp_all)

# rename columns for future use
windsolar <- dplyr::rename(windsolar, MAT = `CHELSA_bio1_1981-2010_V.2.1`,
                           MAP = `CHELSA_bio12_1981-2010_V.2.1`)

# sankey graph showing pre1750 to today change in veg for solar locations? ----

# need to work out source (pre-1750 veg), target (extant veg), value (MW)
sankeylinks <- windsolar %>%
  sf::st_drop_geometry() %>%
  dplyr::select(mvgpre1750_simplified, mvg_simplified, type, totalcapacity_mw) %>% 
  dplyr::group_by(mvgpre1750_simplified, mvg_simplified) %>% # leave out solar/wind distinction for now
  dplyr::summarise(number = n(), sumcapmw = sum(totalcapacity_mw)) %>%
  dplyr::ungroup()

# add dummy spaces in one column so sankey diagram won't lump categories
sankeylinks$mvgpre1750_simplified <- gsub("(^[A-Z][a-z]*[ ,\\/]?)", " \\1", sankeylinks$mvgpre1750_simplified)

# create nodes df for sankey
sankeynodes <- data.frame(
  name = c(as.character(sankeylinks$mvgpre1750_simplified), 
           as.character(sankeylinks$mvg_simplified)) %>% unique()
)

# reformat name to id for networkD3 connection
sankeylinks$IDsource <- match(sankeylinks$mvgpre1750_simplified, sankeynodes$name)-1
sankeylinks$IDtarget <- match(sankeylinks$mvg_simplified, sankeynodes$name)-1

# prepare colour scale: rename below with categories and hex codes once ready to make final graph
# i.e. once have lumped all vegetation categories
#my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

# Make the Network
p <- networkD3::sankeyNetwork(Links = sankeylinks, Nodes = sankeynodes,
                              Source = "IDsource", Target = "IDtarget",
                              Value = "sumcapmw", NodeID = "name", 
                              sinksRight = FALSE)
p

# looks much better with simplified veg categories!

# TO DO - separate sankeys for wind and solar
#       - colour veg groups (and lines?) consistently
rm(p, sankeylinks, sankeynodes)

# LAI unfinished ----

# load in pre-summarised Leaf Area Index rasters from GLOBMAP (doi.org/10.5281/zenodo.12698637)
# for 2001-2003 and 2021-2023
# can then compare pre- and post- development LAI for wind/solar locations?
# theoretically should indicate change in density of vegetation
# may want to BUFFER POINTS to pick up area
# but have checked that all points are currently in the middle of the impact area
# which differs in magnitude for wind and solar - dif buffer sizes maybe?

mean_LAI_0103 <- terra::rast("data_output/tiff2001_2003.tif")
mean_LAI_2123 <- terra::rast("data_output/tiff2021_2023.tif")

# extract values (should buffer first but just trying it for now)
extraction <- terra::extract(mean_LAI_0103, windsolar)
hist(extraction$lyr.1) # mostly low LAI values
extraction2 <- terra::extract(mean_LAI_2123, windsolar)
hist(extraction2$lyr.1) # mostly low still, some higher

# join this data back to main df
names(extraction) <- c("ID", "mean_LAI_0103")
names(extraction2) <- c("ID", "mean_LAI_2123")
windsolar <- windsolar %>%
  dplyr::bind_cols(extraction) %>%
  dplyr::select(-ID) %>%
  dplyr::bind_cols(extraction2) %>%
  dplyr::select(-ID) %>%
  dplyr::mutate(difLAI = mean_LAI_2123 - mean_LAI_0103) # dif between LAI times?
rm(extraction, extraction2, mean_LAI_0103, mean_LAI_2123)

hist(windsolar$difLAI) # mostly close to 0, some -ve but most +ve

# TO DOs
#   - try buffering (prop to MW?) with leaf area index and Hansen forest cover mapping, from ~1998 (earliest start year) to most recent, to see change in cover
#   - cache all data as csv, make more graphs and a nice map to export

# write data output for now

readr::write_csv(windsolar, "data_output/windsolar_environments.csv")

# read back in

windsolar <- readr::read_csv("data_output/windsolar_environments.csv")
