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
# count how many studies in north vs south hemisphere, by wind/solar with proportions
proportions <- sysrevlatlong %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(hemisphere = if_else(latitude > 0, "North", "South")) %>%
  dplyr::distinct(title, wind_solar, hemisphere, .keep_all = TRUE) %>%
  dplyr::count(wind_solar, hemisphere) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_studies = sum(n),
    prop_studies  = n / total_studies
  ) %>%
  dplyr::rename(n_studies = n) %>%
  ungroup()

# count how many wind and solar facilities in north vs south hemisphere 
# (numbers a bit wrong but hopefully proportion similar)
fac_prop <- windsolar_gipt_spat %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(wind_solar = type) %>%
  dplyr::mutate(hemisphere = if_else(lat > 0, "North", "South")) %>%
  dplyr::distinct() %>%
  dplyr::count(wind_solar, hemisphere) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_facilities = sum(n),
    prop_facilities  = n / total_facilities
  ) %>%
  dplyr::rename(n_facilities = n) %>%
  ungroup()

# join together to compare studies vs facilities
proportions <- proportions %>%
  dplyr::left_join(fac_prop, by = c("wind_solar", "hemisphere")) %>%
  dplyr::mutate(ratio = prop_studies / prop_facilities)
proportions

# wind_solar hemisphere n_studies total_studies prop_studies n_facilities total_facilities prop_facilities ratio
# <chr>      <chr>          <int>         <int>        <dbl>        <int>            <int>           <dbl> <dbl>
# 1 solar      North             46            49       0.939         24131            25670          0.940  0.999
# 2 solar      South              3            49       0.0612         1539            25670          0.0600 1.02 
# 3 wind       North            264           280       0.943         14789            16295          0.908  1.04 
# 4 wind       South             16           280       0.0571         1506            16295          0.0924 0.618

# huh actually pretty proportional for hemispheres for solar, though wind
# appears understudied in southern hemisphere compared to the number of facilities
rm(fac_prop, proportions)

### latitude ----
# count how many studies in north vs south hemisphere, by wind/solar with proportions
proportions <- sysrevlatlong %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(lat_band = dplyr::case_when(
    abs(latitude) < 23.5  ~ "Tropical",
    abs(latitude) < 66.5  ~ "Temperate",
    TRUE                  ~ "Polar"
  )) %>%
  dplyr::distinct(title, wind_solar, lat_band, .keep_all = TRUE) %>%
  dplyr::count(wind_solar, lat_band) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_studies = sum(n),
    prop_studies  = n / total_studies
  ) %>%
  dplyr::rename(n_studies = n) %>%
  ungroup()

# count how many wind and solar facilities in north vs south hemisphere 
# (numbers a bit wrong but hopefully proportion similar)
fac_prop <- windsolar_gipt_spat %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(wind_solar = type) %>%
  dplyr::mutate(lat_band = dplyr::case_when(
    abs(lat) < 23.5  ~ "Tropical",
    abs(lat) < 66.5  ~ "Temperate",
    TRUE                  ~ "Polar"
  )) %>%
  dplyr::distinct() %>%
  dplyr::count(wind_solar, lat_band) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_facilities = sum(n),
    prop_facilities  = n / total_facilities
  ) %>%
  dplyr::rename(n_facilities = n) %>%
  ungroup()

# join together to compare studies vs facilities
proportions <- proportions %>%
  dplyr::left_join(fac_prop, by = c("wind_solar", "lat_band")) %>%
  dplyr::mutate(ratio = prop_studies / prop_facilities)
proportions
# wind_solar lat_band  n_studies total_studies prop_studies n_facilities total_facilities prop_facilities ratio
# <chr>      <chr>         <int>         <int>        <dbl>        <int>            <int>           <dbl> <dbl>
# 1 solar      Temperate        49            50       0.98          21417            25670         0.834   1.17 
# 2 solar      Tropical          1            50       0.02           4253            25670         0.166   0.121
# 3 wind       Polar             3           280       0.0107           19            16295         0.00117 9.19 
# 4 wind       Temperate       264           280       0.943         14506            16295         0.890   1.06 
# 5 wind       Tropical         13           280       0.0464         1770            16295         0.109   0.427

rm(fac_prop, proportions)

### region ----
#### extract study regions ----
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

# so all good to use this now

#### extract facility regions ----

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
rm(na_idx, nearest_idx, worldmap)

sum(is.na(extract2$continent))
# matched only 16 points, others must be real odd will leave

table(extract2$continent)
# Africa          Asia        Europe North America       Oceania South America 
# 485         17117         14694          5608           437          2353 

#### continent ----

# compare proportions, split by wind and solar
continent_prop <- extract1 %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(title, wind_solar, continent, .keep_all = TRUE) %>%
  dplyr::count(wind_solar, continent) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_studies = sum(n),
    prop_studies  = n / total_studies
  ) %>%
  dplyr::rename(n_studies = n) %>%
  ungroup()

temp <- extract2 %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(wind_solar = type) %>%
  dplyr::count(wind_solar, continent) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_facilities = sum(n),
    prop_facilities  = n / total_facilities
  ) %>%
  dplyr::rename(n_facilities = n) %>%
  ungroup()

continent_prop <- continent_prop %>%
  dplyr::full_join(temp, by = c("wind_solar","continent")) %>%
  tidyr::replace_na(list(n_studies = 0, total_studies = 280, prop_studies = 0)) %>%
  dplyr::mutate(ratio = prop_studies / prop_facilities) %>%
  dplyr::arrange(continent)

continent_prop
# wind_solar continent     n_studies total_studies prop_studies n_facilities total_facilities prop_facilities ratio
# <chr>      <chr>             <int>         <int>        <dbl>        <int>            <int>           <dbl> <dbl>
# 1 solar      Africa                2            49       0.0408          404            25670         0.0157  2.59 
# 2 wind       Africa                6           280       0.0214           81            16295         0.00497 4.31 
# 3 solar      Asia                  5            49       0.102         11463            25670         0.447   0.229
# 4 wind       Asia                 16           280       0.0571         5654            16295         0.347   0.165
# 5 solar      Europe               16            49       0.327          7775            25670         0.303   1.08 
# 6 wind       Europe              121           280       0.432          6919            16295         0.425   1.02 
# 7 solar      North America        25            49       0.510          3983            25670         0.155   3.29 
# 8 wind       North America       125           280       0.446          1625            16295         0.0997  4.48 
# 9 wind       Oceania               6           280       0.0214          130            16295         0.00798 2.69 
# 10 solar      Oceania               0           280       0               307            25670         0.0120  0    
# 11 solar      South America         1            49       0.0204         1138            25670         0.0443  0.460
# 12 wind       South America         6           280       0.0214         1215            16295         0.0746  0.287

# so north america way overrepresented, Asia way underrepresented in studies 
# south america also underrepresented
# by rough # facilities at least, for wind and solar
# africa "overrepresented", must be due to low # facilities there
# europe roughly proportional
# oceania understudied for solar, overstudied for wind?

# what do total wind+solar numbers look like?
cont_all_prop <- extract1 %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(title, continent, .keep_all = TRUE) %>%
  dplyr::mutate(wind_solar = "all") %>%
  dplyr::count(continent, wind_solar) %>%
  dplyr::mutate(
    total_studies = sum(n),
    prop_studies  = n / total_studies
  ) %>%
  dplyr::rename(n_studies = n) %>%
  ungroup()

temp <- extract2 %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(wind_solar = "all") %>%
  dplyr::count(continent, wind_solar) %>%
  dplyr::mutate(
    total_facilities = sum(n),
    prop_facilities  = n / total_facilities
  ) %>%
  dplyr::rename(n_facilities = n) %>%
  ungroup()

cont_all_prop <- cont_all_prop %>%
  dplyr::full_join(temp, by = c("continent", "wind_solar")) %>%
  dplyr::mutate(ratio = prop_studies / prop_facilities) %>%
  dplyr::arrange(continent)
cont_all_prop
# continent     n_studies total_studies prop_studies n_facilities total_facilities prop_facilities  ratio
# <chr>             <int>         <int>        <dbl>        <int>            <int>           <dbl>  <dbl>
# 1 Africa                8           328       0.0244          485            41965          0.0116  2.11 
# 2 Asia                 21           328       0.0640        17117            41965          0.408   0.157
# 3 Europe              137           328       0.418         14694            41965          0.350   1.19 
# 4 North America       149           328       0.454          5608            41965          0.134   3.40 
# 5 Oceania               6           328       0.0183          437            41965          0.0104  1.76 
# 6 South America         7           328       0.0213         2353            41965          0.0561  0.381

# Asia and South America both strongly underrepresented
# is this a genuine bias in studies or a bias of our English-language review?
# hard to know, not convinced this is the best way to look at this

# compile into one table
cont_all_prop <- cont_all_prop %>%
  dplyr::bind_rows(continent_prop) %>%
  dplyr::arrange(continent)
rm(continent_prop)
#  write output to make table for main text
readr::write_csv(cont_all_prop, "data_output/studies_vs_facilities_by_continent.csv")

# how well is Australia represented specifically?
# compare proportions, split by wind and solar
admin_prop <- extract1 %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(title, admin, wind_solar, .keep_all = TRUE) %>%
  dplyr::count(wind_solar, admin) %>%
  dplyr::mutate(
    total_studies = sum(n),
    prop_studies  = n / total_studies
  ) %>%
  dplyr::rename(n_studies = n) %>%
  ungroup()

temp <- extract2 %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(wind_solar = type) %>%
  dplyr::count(wind_solar, admin) %>%
  dplyr::mutate(
    total_facilities = sum(n),
    prop_facilities  = n / total_facilities
  ) %>%
  dplyr::rename(n_facilities = n) %>%
  ungroup()

admin_prop <- admin_prop %>%
  dplyr::full_join(temp, by = c("wind_solar","admin")) %>%
  tidyr::replace_na(list(n_studies = 0, prop_studies = 0)) %>%
  dplyr::mutate(ratio = prop_studies / prop_facilities) %>%
  dplyr::arrange(admin)
dplyr::filter(admin_prop, admin == "Australia")
# wind_solar admin   n_studies total_studies prop_studies n_facilities total_facilities prop_facilities ratio
# <chr>      <chr>       <int>         <int>        <dbl>        <int>            <int>           <dbl> <dbl>
# 1 wind      Austra…         4           285       0.0140          111            16295         0.00681  2.06
# 2 solar     Austra…        0            NA        0               284            25670         0.0111   0   

# so Australia understudied for solar by # facilities, overstudied for wind
# BUT none of these #s account for PLANNED facilities which in the pipeline now
# so still fair to say we need more studies to guide development

rm(cont_all_prop, temp, extract1, extract2)

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
# plot to check data out (large shapefile takes a while to render, uncomment to run)
# ggplot() +
#   geom_sf(data = biomes, aes(fill = BIOME_NAME), lwd = 0.02) +
#   theme_void()

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
# split by wind and solar
biomes <- extract_sysrev %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(title, wind_solar, BIOME_NAME, .keep_all = TRUE) %>%
  dplyr::count(wind_solar, BIOME_NAME) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_studies = sum(n),
    prop_studies  = n / total_studies
  ) %>%
  dplyr::rename(n_studies = n) %>%
  ungroup()

biomes_gipt <- extract_gipt %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(wind_solar = type) %>%
  dplyr::count(wind_solar, BIOME_NAME) %>%
  dplyr::group_by(wind_solar) %>%
  dplyr::mutate(
    total_facilities = sum(n),
    prop_facilities  = n / total_facilities
  ) %>%
  dplyr::rename(n_facilities = n) %>%
  ungroup()

biomes <- biomes %>%
  dplyr::full_join(biomes_gipt, by = c("wind_solar","BIOME_NAME")) %>%
  dplyr::left_join(biome_areas, by = "BIOME_NAME") %>%
  tidyr::replace_na(list(n_studies = 0, prop_studies = 0)) %>%
  dplyr::mutate(ratio = prop_studies / prop_facilities) %>%
  dplyr::arrange(BIOME_NAME, wind_solar)
rm(biome_areas, biomes_gipt, extract_gipt, extract_sysrev)
print(biomes, n = 30)
# wind_solar BIOME_NAME               n_studies total_studies prop_studies n_facilities total_facilities prop_facilities total_area proportion ratio
# <chr>      <chr>                        <int>         <int>        <dbl>        <int>            <int>           <dbl>      <dbl>      <dbl> <dbl>
# 1 solar      Boreal Forests/Taiga             0            NA      0                 28            25670       0.00109       2523.     0.170   0    
# 2 wind       Boreal Forests/Taiga            14           286      0.0490           306            16295       0.0188        2523.     0.170   2.61 
# 3 solar      Deserts & Xeric Shrubla…        21            51      0.412           3096            25670       0.121         2489.     0.168   3.41 
# 4 wind       Deserts & Xeric Shrubla…        30           286      0.105           1070            16295       0.0657        2489.     0.168   1.60 
# 5 solar      Flooded Grasslands & Sa…         0            NA      0                126            25670       0.00491        106.     0.00711 0    
# 6 wind       Flooded Grasslands & Sa…         0            NA      0                159            16295       0.00976        106.     0.00711 0    
# 7 solar      Mangroves                        0            NA      0                 63            25670       0.00245         27.7    0.00186 0    
# 8 wind       Mangroves                        1           286      0.00350           30            16295       0.00184         27.7    0.00186 1.90 
# 9 solar      Mediterranean Forests, …        10            51      0.196           2383            25670       0.0928         329.     0.0222  2.11 
# 10 wind       Mediterranean Forests, …        43           286      0.150           1860            16295       0.114          329.     0.0222  1.32 
# 11 solar      Montane Grasslands & Sh…         2            51      0.0392           649            25670       0.0253         471.     0.0318  1.55 
# 12 wind       Montane Grasslands & Sh…         1           286      0.00350          488            16295       0.0299         471.     0.0318  0.117
# 13 solar      Temperate Broadleaf & M…        13            51      0.255          11646            25670       0.454         1445.     0.0973  0.562
# 14 wind       Temperate Broadleaf & M…        86           286      0.301           7595            16295       0.466         1445.     0.0973  0.645
# 15 solar      Temperate Conifer Fores…         1            51      0.0196           368            25670       0.0143         445.     0.0300  1.37 
# 16 wind       Temperate Conifer Fores…        15           286      0.0524           229            16295       0.0141         445.     0.0300  3.73 
# 17 solar      Temperate Grasslands, S…         4            51      0.0784          2043            25670       0.0796        1198.     0.0807  0.985
# 18 wind       Temperate Grasslands, S…        67           286      0.234           1788            16295       0.110         1198.     0.0807  2.13 
# 19 solar      Tropical & Subtropical …         0            NA      0                 46            25670       0.00179         60.2    0.00406 0    
# 20 wind       Tropical & Subtropical …         0            NA      0                 10            16295       0.000614        60.2    0.00406 0    
# 21 solar      Tropical & Subtropical …         0            NA      0               1685            25670       0.0656         327.     0.0220  0    
# 22 wind       Tropical & Subtropical …         7           286      0.0245          1178            16295       0.0723         327.     0.0220  0.339
# 23 solar      Tropical & Subtropical …         0            NA      0                527            25670       0.0205        1796.     0.121   0    
# 24 wind       Tropical & Subtropical …         4           286      0.0140           150            16295       0.00921       1796.     0.121   1.52 
# 25 solar      Tropical & Subtropical …         0            NA      0               2689            25670       0.105         1632.     0.110   0    
# 26 wind       Tropical & Subtropical …         6           286      0.0210          1194            16295       0.0733        1632.     0.110   0.286
# 27 solar      Tundra                           0            NA      0                  1            25670       0.0000390     1993.     0.134   0    
# 28 wind       Tundra                           3           286      0.0105            18            16295       0.00110       1993.     0.134   9.50 
# 29 solar      NA                               0            NA      0                320            25670       0.0125          NA     NA       0    
# 30 wind       NA                               9           286      0.0315           220            16295       0.0135          NA     NA       2.33 

# so broadly, most studies and most facilities in temperate forests, 
# Temperate Grasslands somewhat overrepresented for studies vs how many facilities they have
# Tropical forests somewhat underrepresented for studies vs how many facilities they have

# for wind, tundra, temperate conifer forests, boreal forests and temperate grasslands all appear overstudied relative to facility #s
# for solar deserts and xeric shrublands appear overstudied relative to facility #s

# tropical forests fairly understudied

#### biomes figure ----

# recompute proportions out of the combined wind+solar total so the two types
# are directly comparable within each panel (each panel's bars sum to 1 overall)
biome_plot_data <- biomes |>
  dplyr::filter(!is.na(BIOME_NAME)) |>
  dplyr::mutate(
    n_studies    = tidyr::replace_na(n_studies, 0L),
    n_facilities = tidyr::replace_na(n_facilities, 0L),
    prop_s       = n_studies    / sum(n_studies),
    prop_f       = n_facilities / sum(n_facilities),
     short_name   = str_remove(BIOME_NAME, " & Shrublands"),
    # # order by global land area: high proportion = top of chart
    short_name   = forcats::fct_reorder(short_name, prop_f)
  )

# a) study locations — bars extend rightward
p_studies <- ggplot(biome_plot_data,
                    aes(y = short_name, x = prop_s, fill = wind_solar)) +
  geom_col() +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.05)),
    limits = c(0, 0.5)
  ) +
  scale_fill_discrete(type = my_colours$windsolar) +
  labs(x = "Proportion of studies", y = NULL,
       title = "a)", fill = NULL) +
  ggpubr::theme_pubr() +
  theme(legend.position = "none")

# b) GIPT facilities — x reversed so bars extend leftward, mirroring panel a
p_facilities <- ggplot(biome_plot_data,
                       aes(y = short_name, x = prop_f, fill = wind_solar)) +
  geom_col() +
  scale_x_reverse(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0)),
    limits = c(0, 0.5)
  ) +
  scale_fill_discrete(type = my_colours$windsolar) +
  labs(x = "Proportion of facilities", y = NULL,
       title = "b)", fill = NULL) +
  ggpubr::theme_pubr() +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )

p_studies + p_facilities +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("figures/biome_prop_studiesvGIPT.pdf", width = 9, height = 5)

rm(biome_plot_data, p_studies, p_facilities, biomes, sysrevlatlong, 
   windsolar_gipt, windsolar_gipt_spat)

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
