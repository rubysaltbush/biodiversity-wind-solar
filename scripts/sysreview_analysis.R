# final review data ----

# get data
sysrevdata <- readr::read_csv("data_input/wind_solar_biodiversity_sysreview_v8_20260602.csv")

# how many papers included currently?
length(unique(sysrevdata$title))
# 351

# drop excluded papers and 1 NA (paper for which full text couldn't be found)
sysrevdata <- sysrevdata %>%
  dplyr::filter(exclude == FALSE)

# how many papers without excluded?
length(unique(sysrevdata$title))
# 331

# describe data ----

# filter to just unique papers, then count # wind, # solar
sysrevdata %>%
  dplyr::select(title, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::select(wind_solar) %>%
  table()
# wind_solar
# solar      wind 
# 49          283 

# filter to just unique papers, then count # impact, # mitigation
sysrevdata %>%
  dplyr::select(title, impact_mitigation) %>%
  dplyr::distinct() %>%
  dplyr::select(impact_mitigation) %>%
  table()
# impact_mitigation
# impact impactandmitigation          mitigation 
# 282                  27                  22 

# filter to just unique papers, then count wind/solar vs impact/mitigation
sysrevdata %>%
  dplyr::select(title, wind_solar, impact_mitigation) %>%
  dplyr::distinct() %>%
  dplyr::group_by(wind_solar, impact_mitigation) %>%
  dplyr::summarise(count_by_windsolarimpactmitigation = n())
# wind_solar   impact_mitigation          count_by_windsolarimpactmitigation
# <chr>        <chr>                                          <int>
# 1 solar      impact                                            34
# 2 solar      impact_mitigation                                  6
# 3 solar      mitigation                                         9
# 4 wind       impact                                           249
# 5 wind       impact_mitigation                                 21
# 6 wind       mitigation                                        13

# when ----
# make histogram of year of publication, coloured by wind/solar
yearpub <- sysrevdata %>%
  dplyr::select(title, year, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year, wind_solar) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = c(0.3,0.8)) +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Year of publication") +
  ylab("Number of studies") +
  labs(fill = "Facility type")
yearpub
ggsave("figures/year_pub_histogram.pdf", width = 5, height = 4)

# add in no. wind and solar developments globally from GIPT data for some context
source("scripts/gipt_data.R")

yeardev <- windsolar_gipt %>%
  dplyr::select(project_name, start_year, type) %>%
  dplyr::distinct() %>%
  dplyr::filter(start_year <= 2024) %>% # filter to date range of publications studied (1970-2024) 
  dplyr::group_by(start_year, type) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = start_year, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = c(0.3,0.8)) +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("First year of operation") +
  ylab("Number of facilities") +
  labs(fill = "Facility type")
yeardev
ggsave("figures/gipt_yeardev_histogram.pdf", width = 5, height = 4)

yearpub + yeardev + plot_annotation(tag_levels = "a")
ggsave("figures/yearpub_yeardev_hist_combined.pdf", width = 10, height = 4)
rm(yearpub, yeardev, windsolar_gipt)

# where ----

# count continent where studies located from sys review categorisation
sysrevdata %>%
  dplyr::select(title, region) %>%
  dplyr::distinct() %>%
  dplyr::select(region) %>%
  table()
# region
# Africa       Asia         Europe       North America   Oceania   South America 
# 8            21           137           152             6            7 

# count country where studies located from sys review categorisation
sysrevdata %>%
  dplyr::select(title, country) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(country, delim = ", ") %>%
  dplyr::select(country) %>%
  table()
# country
# Australia        Austria        Belgium         Brazil       Bulgaria         Canada 
# 4              3              3              5              2             16 
# Chile          China        Croatia Czech Republic        Denmark          Egypt 
# 2             12              1              1              2              1 
# Ethiopia        Finland         France        Germany         Greece        Hungary 
# 1              3             12             18              1              4 
# India        Ireland         Israel          Italy          Japan          Korea 
# 1              2              2              7              2              1 
# Mexico    Netherlands  New Caledonia    New Zealand         Norway         Poland 
# 6              4              1              1             11              7 
# Portugal    Puerto Rico        Romania       Slovakia   South Africa          Spain 
# 2              1              3              1              6             25 
# Sweden    Switzerland         Taiwan         Turkey United Kingdom  United States 
# 11              1              2              1             18            130 

# create table of country count data
counts <- sysrevdata %>%
  dplyr::select(title, country, region, wind_solar) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(country, delim = ", ") %>%
  dplyr::group_by(country, region, wind_solar) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()
# add country total count
countrycount <- counts %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(country_total = sum(count))
# add region total count
regioncount <- counts %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(region_total = sum(count))
counts <- counts %>%
  dplyr::left_join(countrycount, by = "country") %>%
  dplyr::left_join(regioncount, by = "region")
rm(countrycount, regioncount)

# make histogram of country coloured by wind/solar, sorted by region+counts
counts %>%
  dplyr::arrange(-region_total, -country_total) %>%
  dplyr::mutate(country = forcats::fct_inorder(country)) %>%
  ggplot(aes(x = country, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Country") +
  ylab("Number of studies") +
  labs(fill = "Facility type") +
  coord_flip()
ggsave("figures/countries_by_region_histogram_windsolar.pdf", width = 8, height = 7)

## map ----
# make map of locations of study
# first get world map data without Antarctica (not included)
worldmap <- rnaturalearth::ne_countries() %>% 
  filter(sovereignt != "Antarctica")

# then get spatial version of sys review data where lat/long available
sysrevlatlong <- sysrevdata %>%
  dplyr::select(title, wind_solar, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::distinct()

# checking my countries against "admin" column of world map, need to change:
# United States -> United States of America
counts$country <- gsub("United States", "United States of America", counts$country)

# and summarise counts back down to one per country
counts <- counts %>%
  dplyr::select(country, country_total) %>%
  dplyr::distinct()

# join data together
worldmapcount <- worldmap %>%
  dplyr::select(country = admin, geometry) %>%
  dplyr::left_join(counts, by = "country") %>%
  dplyr::mutate(countlog = log(replace_na(country_total, 0)))

# then plot background world map and points coloured by wind or solar
ggplot() +
  geom_sf(data = worldmapcount, aes(fill = countlog)) +
  theme_void() +
  geom_point(data = sysrevlatlong, aes(x = longitude, y = latitude, colour = wind_solar), size = 0.9, alpha = 0.9) +
  labs(colour = "Facility type") +
  scale_colour_discrete(type = my_colours$windsolar) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "#F0F0F0") +
  labs(fill = "No. studies (log)")
ggsave("figures/map_study_locations.pdf", width = 6, height = 4)
rm(worldmapcount, counts)

# source script to create world map of facilities from GIPT for comparison
source("scripts/global_windsolar_map.R")

## GIPT vs studies ----
### hemisphere ----
# count how many studies in north hemisphere
sysrevlatlong %>%
  dplyr::group_by(title) %>%
  dplyr::summarise(meanlat = mean(latitude)) %>%
  dplyr::filter(meanlat > 0) %>% 
  nrow()
# 309

# vs south hemisphere
sysrevlatlong %>%
  dplyr::group_by(title) %>%
  dplyr::summarise(meanlat = mean(latitude)) %>%
  dplyr::filter(meanlat < 0) %>% 
  nrow()
# 19

# how many wind and solar facilities in northern hemisphere 
# (numbers a bit wrong but hopefully proportion similar)
windsolar_gipt_spat %>%
  dplyr::filter(lat >0) %>%
  nrow()
# 38920

# vs southern hemisphere for facilities
windsolar_gipt_spat %>%
  dplyr::filter(lat <0) %>%
  nrow()
# 3045

# % studies in northern hemisphere
309/(309+19)*100
# 94.2%

# % facilities in northern hemisphere
38920/(38920+3045)*100
# 92.7

# huh actually pretty proportional for hemispheres, checks out

### continent ----
# to get continent - need to intersect with worldmap
# first make sysrevlatlong an sf object
sysrevlatlong <- sf::st_as_sf(sysrevlatlong, coords = c("longitude","latitude"), 
                              remove = FALSE, crs = 4326)
# and make windsolar_gipt_spat an sf object
windsolar_gipt_spat <- sf::st_as_sf(windsolar_gipt_spat, coords = c("lon","lat"), 
                              remove = FALSE, crs = 4326)

# then intersect both with worldmap to get country and continent
# first simplify worldmap down a bit
worldmap <- worldmap %>%
  dplyr::select(continent, admin, geounit, geometry)

# worldmap has invalid geometry but can't fix with simple methods
# turning strict s2 spherical geometry off as not important here
# Disable s2 spherical geometry
sf::sf_use_s2(FALSE)

# then extract continent and country name for each study location
extract1 <- sf::st_join(sysrevlatlong, worldmap)
table(extract1$continent)
# Africa          Asia        Europe North America       Oceania South America 
# 6            17           139           161             2             8

# a bit different from our manual categorisation, some because some studies lack lat/longs
# but also looks like a lot of NAs in extraction, which mostly look close to the coast

# match NA values to nearest feature
# first index where NA values are
na_idx <- which(is.na(extract1$continent))

# then extract nearest feature and patch back into main data
if (length(na_idx) > 0) {
  nearest_idx <- sf::st_nearest_feature(sysrevlatlong[na_idx, ], worldmap)
  extract1[na_idx, c("continent", "admin", "geounit")] <-
    sf::st_drop_geometry(worldmap)[nearest_idx, c("continent", "admin", "geounit")]
}
rm(na_idx, nearest_idx)

table(extract1$continent)
# Africa          Asia        Europe North America       Oceania South America 
# 8            23           149           161             8             8 

#hmm interesting still quite different to manual categorisation, look at conflicts?
region <- sysrevdata %>%
  dplyr::select(title, latitude, longitude, region, country) %>%
  dplyr::distinct()
extract1 <- extract1 %>%
  dplyr::left_join(region, by = c("title", "latitude", "longitude"))
  
# filter to country conflicts
conflicts <- extract1 %>%
  dplyr::filter(admin != country)
table(conflicts$admin)
# have manually checked these conflicts, most are just typographic differences
# only one (serbia vs romania) appears to be a small error in projection for a 
# wind farm right on the border
rm(conflicts, region)

# and for each GIPT facility location
extract2 <- sf::st_join(windsolar_gipt_spat, worldmap)

# match NA values to nearest feature
# first index where NA values are
na_idx <- which(is.na(extract2$continent))

# then extract nearest feature and patch back into main data
if (length(na_idx) > 0) {
  nearest_idx <- sf::st_nearest_feature(sysrevlatlong[na_idx, ], worldmap)
  extract2[na_idx, c("continent", "admin", "geounit")] <-
    sf::st_drop_geometry(worldmap)[nearest_idx, c("continent", "admin", "geounit")]
}
rm(na_idx, nearest_idx)

sum(is.na(extract2$continent))
# matched only 16 points, others must be real odd will leave

table(extract2$continent)
# Africa          Asia        Europe North America       Oceania South America 
# 485         17117         14694          5608           437          2353 

# compare proportions
continent_prop <- extract1 %>%
  dplyr::group_by(continent) %>%
  st_drop_geometry() %>%
  summarise(sysreview = n()) %>%
  dplyr::mutate(sysrev_prop = sysreview / sum(sysreview, na.rm = TRUE))

temp <- extract2 %>%
  dplyr::group_by(continent) %>%
  st_drop_geometry() %>%
  summarise(gipt = n()) %>%
  dplyr::mutate(gipt_prop = gipt / sum(gipt, na.rm = TRUE))
rm(extract1, extract2)

continent_prop <- continent_prop %>%
  dplyr::full_join(temp, by = "continent") 
rm(temp)

continent_prop
# continent       sysreview   sysrev_prop  gipt gipt_prop
# <chr>             <int>       <dbl> <int>     <dbl>
# 1 Africa                8      0.0224   485    0.0116
# 2 Asia                 23      0.0644 17117    0.408 
# 3 Europe              149      0.417  14694    0.350 
# 4 North America       161      0.451   5608    0.134 
# 5 Oceania               8      0.0224   437    0.0104
# 6 South America         8      0.0224  2353    0.0561
# 7 NA                   NA     NA       1271    0.0303

# so north america way overrepresented, Asia way underrepresented in studies 
# by rough # facilities at least
rm(continent_prop)

### biomes ----

# try making table of study habitat (might be all over the place)
sysrevdata %>%
  dplyr::select(title, habitat) %>%
  dplyr::distinct() %>%
  dplyr::select(habitat) %>%
  table()
# too many categories to be useful, could manually reduce but 
# might be easier to extract biomes from lat/longs

# read in global biome data from Dinerstein et al. (2017)
# rather large (150MB) shapefile in data cache
biomes <- sf::st_read("data_cache/Ecoregions2017/Ecoregions2017.shp")
# plot to check data out (large shapefile, takes a while to render)
ggplot() +
  geom_sf(data = biomes, aes(fill = BIOME_NAME), lwd = 0.02) +
  theme_void()

# try making biomes shapefile valid to remove bugs
biomes <- sf::st_make_valid(biomes)

# calculate total size of each biome type, using pre-calculated area
# which is in square degrees but will do for proportion of different biomes globally
biome_areas <- biomes %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(BIOME_NAME) %>%
  dplyr::summarise(total_area = sum(SHAPE_AREA)) %>%
  dplyr::filter(BIOME_NAME != "N/A") %>%
  dplyr::mutate(proportion = total_area/sum(total_area))

# extract data from biomes for each study location
extract_sysrev <- sf::st_join(sysrevlatlong, biomes)
# and for each facility location
extract_gipt <- sf::st_join(windsolar_gipt_spat, biomes)

# check it out
table(extract_sysrev$BIOME_NAME)
# Boreal Forests/Taiga                               Deserts & Xeric Shrublands 
# 14                                                       52 
# Mangroves                 Mediterranean Forests, Woodlands & Scrub 
# 1                                                       56 
# Montane Grasslands & Shrublands                      Temperate Broadleaf & Mixed Forests 
# 3                                                      107 
# Temperate Conifer Forests              Temperate Grasslands, Savannas & Shrublands 
# 16                                                       79 
# Tropical & Subtropical Dry Broadleaf Forests Tropical & Subtropical Grasslands, Savannas & Shrublands 
# 7                                                        4 
# Tropical & Subtropical Moist Broadleaf Forests            Tundra 
# 6                                                        3 

table(extract_gipt$BIOME_NAME)
# Boreal Forests/Taiga                               Deserts & Xeric Shrublands 
# 334                                                     4166 
# Flooded Grasslands & Savannas                                                Mangroves 
# 285                                                       93 
# Mediterranean Forests, Woodlands & Scrub                          Montane Grasslands & Shrublands 
# 4243                                                     1137 
# Temperate Broadleaf & Mixed Forests                                Temperate Conifer Forests 
# 19241                                                      597 
# Temperate Grasslands, Savannas & Shrublands                Tropical & Subtropical Coniferous Forests 
# 3831                                                       56 
# Tropical & Subtropical Dry Broadleaf Forests Tropical & Subtropical Grasslands, Savannas & Shrublands 
# 2863                                                      677 
# Tropical & Subtropical Moist Broadleaf Forests                                                   Tundra 
# 3883                                                       19 

# add onto proportional area of each biome to make mega table of proportions
biomes <- extract_sysrev %>%
  dplyr::group_by(BIOME_NAME) %>%
  st_drop_geometry() %>%
  summarise(sysreview = n()) %>%
  dplyr::mutate(sysrev_prop = sysreview / sum(sysreview, na.rm = TRUE))

biomes_gipt <- extract_gipt %>%
  dplyr::group_by(BIOME_NAME) %>%
  st_drop_geometry() %>%
  summarise(gipt = n()) %>%
  dplyr::mutate(gipt_prop = gipt / sum(gipt, na.rm = TRUE))

biomes <- biomes_gipt %>%
  dplyr::left_join(biomes, by = "BIOME_NAME") %>%
  dplyr::left_join(biome_areas, by = "BIOME_NAME")
rm(biome_areas, biomes_gipt)
biomes
# BIOME_NAME                                                gipt gipt_prop sysreview sysrev_prop total_area proportion
# <chr>                                                    <int>     <dbl>     <int>       <dbl>      <dbl>      <dbl>
# 1 Boreal Forests/Taiga                                       334  0.00796         14     0.0392      2523.     0.170  
# 2 Deserts & Xeric Shrublands                                4166  0.0993          52     0.146       2489.     0.168  
# 3 Flooded Grasslands & Savannas                              285  0.00679         NA    NA            106.     0.00711
# 4 Mangroves                                                   93  0.00222          1     0.00280       27.7    0.00186
# 5 Mediterranean Forests, Woodlands & Scrub                  4243  0.101           56     0.157        329.     0.0222 
# 6 Montane Grasslands & Shrublands                           1137  0.0271           3     0.00840      471.     0.0318 
# 7 Temperate Broadleaf & Mixed Forests                      19241  0.459          107     0.300       1445.     0.0973 
# 8 Temperate Conifer Forests                                  597  0.0142          16     0.0448       445.     0.0300 
# 9 Temperate Grasslands, Savannas & Shrublands               3831  0.0913          79     0.221       1198.     0.0807 
# 10 Tropical & Subtropical Coniferous Forests                   56  0.00133         NA    NA             60.2    0.00406
# 11 Tropical & Subtropical Dry Broadleaf Forests              2863  0.0682           7     0.0196       327.     0.0220 
# 12 Tropical & Subtropical Grasslands, Savannas & Shrublands   677  0.0161           4     0.0112      1796.     0.121  
# 13 Tropical & Subtropical Moist Broadleaf Forests            3883  0.0925           6     0.0168      1632.     0.110  
# 14 Tundra                                                      19  0.000453         3     0.00840     1993.     0.134  
# 15 NA                                                         540  0.0129           9     0.0252        NA     NA      

# so broadly, most studies and most facilities in temperate forests, 
# Temperate Grasslands somewhat overrepresented for studies vs how many facilities they have
# Tropical forests somewhat underrepresented for studies vs how many facilities they have

# TO DO BELOW ####

# calculate more wind/solar specific proportions (for hemisphere, continent, biomes)
# try to make figure or table summarising, are studies in systematic review
# proportional to the number of facilities in dif hemisphere, continent, biome?

# # it worked! now join back to sys rev data by title, latitude and longitude
# extract <- extract %>%
#   dplyr::select(title, latitude, longitude, eco_name = ECO_NAME, biome = BIOME_NAME) %>%
#   sf::st_drop_geometry() %>%
#   dplyr::distinct()
# sysrevdata <- sysrevdata %>%
#   dplyr::left_join(extract, by = c("title", "latitude", "longitude"))
# # some NAs but this will do for broad picture
# rm(biomes, extract, sysrevlatlong)
# 
# # table
# sysrevdata %>%
#   dplyr::select(title, biome) %>%
#   dplyr::distinct() %>%
#   dplyr::select(biome) %>%
#   table()
# # biome
# # Boreal Forests/Taiga                               Deserts & Xeric Shrublands 
# # 14                                                       52 
# # Mangroves                 Mediterranean Forests, Woodlands & Scrub 
# # 1                                                       53 
# # Montane Grasslands & Shrublands                      Temperate Broadleaf & Mixed Forests 
# # 3                                                       99 
# # Temperate Conifer Forests              Temperate Grasslands, Savannas & Shrublands 
# # 16                                                       70 
# # Tropical & Subtropical Dry Broadleaf Forests Tropical & Subtropical Grasslands, Savannas & Shrublands 
# # 7                                                        4 
# # Tropical & Subtropical Moist Broadleaf Forests          Tundra 
# # 6                                                        3 
# 
# # plot systematic review study biomes, sorted by global proportion of each biome 
# sysrevdata %>%
#   dplyr::select(title, biome, wind_solar) %>%
#   dplyr::filter(!(is.na(biome))) %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(biome, wind_solar) %>%
#   dplyr::summarise(count = n()) %>%
#   dplyr::ungroup() %>%
#   dplyr::left_join(biome_areas, by = c("biome" = "BIOME_NAME")) %>%
#   dplyr::arrange(-proportion) %>%
#   dplyr::mutate(biome = forcats::fct_inorder(biome)) %>%
#   ggplot(aes(x = biome, y = count, fill = wind_solar)) +
#   geom_bar(stat = "identity") +
#   ggpubr::theme_pubr() +
#   scale_fill_discrete(type = my_colours$windsolar) +
#   xlab("Biome") +
#   coord_flip() +
#   ylab("Number of studies") +
#   labs(fill = "Facility type")
# ggsave("figures/biome_histogram.pdf", width = 8, height = 5)
# 
# rm(biome_areas)

# who/what ----
# table of biodiversity categories
sysrevdata %>%
  dplyr::select(title, biodiversity_category) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(biodiversity_category, delim = ", ") %>%
  dplyr::select(biodiversity_category) %>% 
  table()
# biodiversity_category
# amphibians   bats      birds   fish  insects invertebrates other mammals 
# 3            99        175     1     19             7            28 
# plants      reptiles 
# 23            14 

# make histogram of biodiversity category, counting some pubs with multiple taxa in multiple bars
# coloured by wind or solar, arranged by # studies
plotcat <- sysrevdata %>%
  dplyr::select(title, biodiversity_category, wind_solar) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(biodiversity_category, delim = ", ") %>%
  dplyr::group_by(biodiversity_category, wind_solar) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(-count) %>%
  dplyr::mutate(biodiversity_category = forcats::fct_inorder(biodiversity_category)) %>%
  ggplot(aes(x = biodiversity_category, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr() +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Biodiversity category") +
  coord_flip() +
  ylab("Number of studies") +
  labs(fill = "Facility type") +
  scale_y_continuous(limits = c(0, 192))
plotcat
ggsave("figures/biodiversity_cat_histogram.pdf", width = 5, height = 5)

# table of biodiversity measure
sysrevdata %>%
  dplyr::select(title, biodiversity_metric, wind_solar) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(biodiversity_metric, delim = ", ") %>%
  dplyr::group_by(biodiversity_metric, wind_solar) %>%
  dplyr::summarise(count = n())
# biodiversity_metric wind_solar count
# <chr>               <chr>      <int>
# 1 abundance           solar          4
# 2 abundance           wind          19
# 3 behaviour           solar          6
# 4 behaviour           wind          63
# 5 distribution        solar          4
# 6 distribution        wind          44
# 7 diversity           solar         24
# 8 diversity           wind          18
# 9 genetics            solar          1
# 10 health              solar          2
# 11 health              wind           7
# 12 mortality           solar          6
# 13 mortality           wind         127
# 14 native cover        solar          2
# 15 predation           wind           2
# 16 reproduction        wind          21
# 17 survival            solar          5
# 18 survival            wind           7

# make histogram of biodiversity measure, counting some pubs with multiple cats in multiple bars
plotmeas <- sysrevdata %>%
  dplyr::select(title, biodiversity_metric, wind_solar) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(biodiversity_metric, delim = ", ") %>%
  dplyr::group_by(biodiversity_metric, wind_solar) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = reorder(biodiversity_metric, -count), y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr() +
  xlab("Biodiversity metric") +
  ylab("Number of studies") +
  scale_fill_discrete(type = my_colours$windsolar) +
  labs(fill = "Facility type") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 192))
plotmeas
ggsave("figures/biodiversity_metric_histogram.pdf", width = 5, height = 5)

# combine vertically with patchwork
combined <- plotcat / plotmeas + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")
combined

ggsave("figures/biodiversity_catmeas_combined.pdf", combined, width = 6, height = 9)
rm(combined)
# keep two other plots to try combining with effect figure

# outcome ----

# table of effects
effect <- sysrevdata %>%
  dplyr::select(title, wind_solar, effect_wind, effect_solar) %>%
  dplyr::distinct() %>%
  dplyr::filter_at(vars(effect_wind, effect_solar), any_vars(!is.na(.))) %>%
  tidyr::unite(col = "effect", effect_wind:effect_solar, na.rm = TRUE) %>%
  dplyr::group_by(effect, wind_solar) %>%
  dplyr::summarise(count = n())

# make column with total number of studies for wind/solar
totals <- effect %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::summarise(total = sum(count))
totals

# join back on and calculate proportion
effect <- effect %>%
  dplyr::left_join(totals, by = "wind_solar") %>%
  dplyr::mutate(proportion = count/total)
rm(totals)
effect
# effect   wind_solar count total proportion
# <chr>    <chr>      <int> <int>      <dbl>
# 1 negative solar         17    49     0.347 
# 2 negative wind         192   299     0.642 
# 3 neutral  solar         26    49     0.531 
# 4 neutral  wind          94   299     0.314 
# 5 positive solar          6    49     0.122 
# 6 positive wind          13   299     0.0435


# make histogram of effect of wind and solar by proportion publications
effect %>%
  ggplot(aes(x = effect, y = proportion, fill = wind_solar)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggpubr::theme_pubr() +
  xlab("Impact on biodiversity") +
  ylab("Proportion of studies") +
  scale_fill_discrete(name = "Facility", type = my_colours$windsolar)
ggsave("figures/effect_development_barchart_proportion.pdf", width = 5, height = 4)

# histogram of number of studies to match above flipped coordinate histograms
ploteffect <- effect %>%
  ggplot(aes(x = effect, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggpubr::theme_pubr() +
  xlab("Impact on biodiversity") +
  ylab("Number of studies") +
  scale_fill_discrete(type = my_colours$windsolar) +
  labs(fill = "Facility type") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 192))
ploteffect
ggsave("figures/effect_development_histogram.pdf", width = 5, height = 5)  
  
# combine vertically with patchwork
combined <- plotcat / plotmeas / ploteffect + 
  patchwork::plot_layout(guides = "collect") & theme(legend.position = "top")
combined

ggsave("figures/biodiversity_catmeaseffect_combined.pdf", combined, width = 6, height = 13)
rm(plotcat, plotmeas, ploteffect, combined, effect)

# mitigation ----

# how many studies suggested any form of mitigation
sysrevdata %>%
  dplyr::select(title, mitigation_suggested) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_suggested)) %>% 
  dplyr::select(mitigation_suggested) %>% 
  table()
# mitigation_suggested
# FALSE  TRUE 
# 110   221 

# what sort of mitigation suggested
mitigation_cat <- sysrevdata %>%
  dplyr::select(title, mitigation_category, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_category)) %>%
  # Split the categories and unnest them
  dplyr::mutate(mitigation_category = stringr::str_split(mitigation_category, ", ")) %>%
  tidyr::unnest(mitigation_category) %>%
  # Count occurrences of each category
  dplyr::group_by(mitigation_category, wind_solar) %>%
  dplyr::summarise(count = n(), .groups = 'drop')
totals <- mitigation_cat %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::summarise(total = sum(count))
mitigation_cat <- mitigation_cat %>%
  dplyr::left_join(totals, by = "wind_solar") %>%
  dplyr::mutate(proportion = count/total)
rm(totals)
mitigation_cat
# mitigation_category         wind_solar count total proportion
# <chr>                       <chr>      <int> <int>      <dbl>
# 1 curtailment                 wind          60   244     0.246 
# 2 management decisions        solar         10    47     0.213 
# 3 management decisions        wind          21   244     0.0861
# 4 offsetting                  solar          4    47     0.0851
# 5 offsetting                  wind          15   244     0.0615
# 6 on-site restoration         solar         14    47     0.298 
# 7 on-site restoration         wind           7   244     0.0287
# 8 siting                      solar         12    47     0.255 
# 9 siting                      wind         106   244     0.434 
# 10 technological modifications solar          7    47     0.149 
# 11 technological modifications wind          35   244     0.143 

# now a bar chart showing mitigations suggested, and mitigation category
mitbarchart <- mitigation_cat %>%
  dplyr::mutate(mitigation_category = factor(mitigation_category, levels = c("siting", 
                                                                             "curtailment", 
                                                                             "technological modifications", 
                                                                             "management decisions", 
                                                                             "on-site restoration", 
                                                                             "offsetting"))) %>%
  ggplot(aes(x = mitigation_category, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr() +
  xlab("Suggested mitigation") +
  labs(fill = "Facility type") +
  scale_fill_discrete(type = my_colours$windsolar) +
  ylab("Number of studies") +
  coord_flip()
mitbarchart
ggsave("figures/mitigation_cat_barplot_proportion.pdf", width = 7, height = 5)
rm(mitigation_cat)

# how many studies conducted explicit tests of suggested mitigations' effectiveness
sysrevdata %>%
  dplyr::select(title, mitigation_suggested, mitigation_tested) %>%
  dplyr::distinct() %>%
  dplyr::filter(mitigation_suggested == TRUE) %>% 
  dplyr::select(mitigation_tested) %>% 
  table()
# mitigation_tested
# FALSE  TRUE 
# 166    53 

# how many explicit tests support mitigation method
sysrevdata %>%
  dplyr::filter(mitigation_tested == "TRUE") %>%
  dplyr::select(title, mitigation_supported_by_explicit_test) %>%
  dplyr::distinct() %>%
  dplyr::select(mitigation_supported_by_explicit_test) %>% 
  table()
# mitigation_supported_by_explicit_test
# FALSE  TRUE 
# 7    47

# MAKE GRAPH OF ABOVE, SPLIT BY MITIGATION CATEGORY
mittestedchart <- sysrevdata %>%
  dplyr::filter(mitigation_tested == "TRUE") %>%
  dplyr::select(title, mitigation_category, mitigation_supported_by_explicit_test) %>%
  dplyr::distinct() %>%
  # Split the categories and unnest them
  dplyr::mutate(mitigation_category = stringr::str_split(mitigation_category, ", ")) %>%
  tidyr::unnest(mitigation_category) %>%
  # Count occurrences of each category
  dplyr::group_by(mitigation_category, mitigation_supported_by_explicit_test) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(mitigation_category = factor(mitigation_category, levels = c("siting", 
                                                                               "curtailment", 
                                                                               "technological modifications", 
                                                                               "management decisions", 
                                                                               "on-site restoration", 
                                                                               "offsetting"))) %>%
  ggplot(aes(x = mitigation_category, y = count, fill = mitigation_supported_by_explicit_test)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr() +
  xlab("Tested mitigation") +
  labs(fill = "Mitigation supported") +
  scale_fill_manual(values = c("#ef8a62","#67a9cf")) +
  ylab("Number of studies") +
  coord_flip()
mittestedchart
ggsave("figures/mitigation_support_barchart.pdf", width = 7, height = 5)

# combine mitigation graphs with patchwork
mitbarchart | mittestedchart
ggsave("figures/mitigation_combined.pdf", width = 12, height = 5)

rm(mitbarchart, mittestedchart, sysrevdata)
