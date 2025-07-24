# figures from final review data ----

# get data
sysrevdata <- readr::read_csv("data_input/wind_solar_biodiversity_sysreview_v8_20250723.csv")

# how many papers included currently?
length(unique(sysrevdata$title))
# 351

# drop excluded papers and 1 NA (paper for which full text couldn't be found)
sysrevdata <- sysrevdata %>%
  dplyr::filter(exclude == FALSE)

# how many papers without excluded?
length(unique(sysrevdata$title))
# 331

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

# locations ----

# count where studies located
sysrevdata %>%
  dplyr::select(title, region) %>%
  dplyr::distinct() %>%
  dplyr::select(region) %>%
  table()
# region
# Africa       Asia         Europe       North America   Oceania   South America 
# 8            21           137           151             6            8 

# make map of locations of study
# first get world map data without Antarctica (not included)
worldmap <- rnaturalearth::ne_countries() %>% 
  filter(sovereignt != "Antarctica")

# then get spatial version of sys review data where lat/long available
sysrevlatlong <- sysrevdata %>%
  dplyr::select(title, wind_solar, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::distinct()

# then plot background world map and points coloured by wind or solar
ggplot() +
  geom_sf(data = worldmap, fill = "grey", colour = "white", linewidth = 0.1) +
  theme_void() +
  geom_point(data = sysrevlatlong, aes(x = longitude, y = latitude, colour = wind_solar)) +
  labs(colour = "Facility type") +
  scale_colour_discrete(type = my_colours$windsolar)
ggsave("figures/map_study_locations.png", width = 6, height = 4)
rm(worldmap, sysrevlatlong)
# COULD combine these points with coloured background map??

# table of biodiversity categories
sysrevdata %>%
  dplyr::select(title, biodiversity_category) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$biodiversity_category)) %>% 
  dplyr::select(biodiversity_category) %>% 
  table()
# biodiversity_category
# amphibians                    animals                       bats 
# 2                          1                         94 
# bats, birds bats, birds, invertebrates        bats, invertebrates 
# 10                          1                          1 
# birds                birds, bats birds, fish, invertebrates 
# 159                          1                          1 
# birds, mammals            birds, reptiles              invertebrates 
# 3                          1                         14 
# mammals                     plants            plants, animals 
# 22                         19                          2 
# pollinators, invertebrates                   reptiles 
# 3                         10 

# make histogram of effect of wind and solar
# MAYBE WANT TO MAKE THIS PROPORTIONAL SOMEHOW SO WIND DOESN"T DWARF SOLAR???
sysrevdata %>%
  dplyr::select(title, wind_solar, effect_wind, effect_solar) %>%
  dplyr::distinct() %>%
  dplyr::filter_at(vars(effect_wind, effect_solar), any_vars(!is.na(.))) %>%
  tidyr::unite(col = "effect", effect_wind:effect_solar, na.rm = TRUE) %>%
  ggplot(aes(x = effect, fill = wind_solar)) +
  geom_bar(position = "dodge") +
  ggpubr::theme_pubr() +
  xlab("Impact on Biodiversity") +
  ylab("Number of Publications") +
  scale_fill_discrete(name = "Facility", type = my_colours$windsolar)
ggsave("figures/effect_development_barchart.png", width = 5, height = 4)

# mitigation ----

# now a bar chart showing mitigations suggested, and mitigation heirarchy
# given some have multiple suggestions have to summarise data a bit
# AS ABOVE MAKE PROPORTIONAL
sysrevdata %>%
  dplyr::select(title, mitigation_category, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_category)) %>%
  # Split the categories and unnest them
  dplyr::mutate(mitigation_category = stringr::str_split(mitigation_category, ", ")) %>%
  tidyr::unnest(mitigation_category) %>%
  # Count occurrences of each category
  dplyr::group_by(mitigation_category, wind_solar) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = reorder(mitigation_category, -count), y = count, fill = wind_solar)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggpubr::theme_pubr() +
  xlab("Mitigation Category") +
  labs(fill = "Facility type") +
  scale_fill_discrete(type = my_colours$windsolar) +
  ggpubr::rotate_x_text(angle = 15, hjust = 0.7, vjust = 0.9) +
  ylab("Number of Publications")
ggsave("figures/mitigation_cat_barplot.png", width = 5, height = 4)

# tables for numbers
sysrevdata %>%
  dplyr::select(title, mitigation_suggested) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_suggested)) %>% 
  dplyr::select(mitigation_suggested) %>% 
  table()
# mitigation_suggested
# FALSE  TRUE 
# 110   221 

# mitigation category
sysrevdata %>%
  dplyr::select(title, mitigation_category, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_category)) %>%
  # Split the categories and unnest them
  dplyr::mutate(mitigation_category = stringr::str_split(mitigation_category, ", ")) %>%
  tidyr::unnest(mitigation_category) %>%
  # Count occurrences of each category
  dplyr::group_by(mitigation_category, wind_solar) %>%
  dplyr::summarise(count = n(), .groups = 'drop')
# mitigation_category         wind_solar    count
# <chr>                       <chr>      <int>
# 1 curtailment                 wind          60
# 2 management decisions        solar         10
# 3 management decisions        wind          21
# 4 offsetting                  solar          4
# 5 offsetting                  wind          15
# 6 on-site restoration         solar         14
# 7 on-site restoration         wind           7
# 8 siting                      solar         12
# 9 siting                      wind         106
# 10 technological modifications solar          7
# 11 technological modifications wind          35

# explicit tests of mitigation
sysrevdata %>%
  dplyr::select(title, mitigation_tested) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_tested)) %>% 
  dplyr::select(mitigation_tested) %>% 
  table()
# mitigation_tested
# FALSE  TRUE 
# 180    53 

# explicit tests of mitigation
sysrevdata %>%
  dplyr::filter(mitigation_tested == "TRUE") %>%
  dplyr::select(title, mitigation_supported_by_explicit_test) %>%
  dplyr::distinct() %>%
  dplyr::select(mitigation_supported_by_explicit_test) %>% 
  table()
# mitigation_supported_by_explicit_test
# FALSE  TRUE 
# 7    47

# make histogram of year of publication
sysrevdata %>%
  dplyr::select(title, year) %>%
  dplyr::distinct() %>%
  ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1) +
  ggpubr::theme_pubr() +
  xlab("Year of Publication") +
  ylab("Number of Publications")
ggsave("figures/year_pub_histogram.png", width = 5, height = 4)

# want to make map of country/region data
# first get world map data without Antarctica (not included)
worldmap <- rnaturalearth::ne_countries() %>% 
  filter(sovereignt != "Antarctica")

# create table of country count data
counts <- sysrevdata %>%
  dplyr::select(title, country, region) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(country, delim = ", ") %>%
  dplyr::group_by(country, region) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()
# add regional count
regioncount <- counts %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(regioncount = sum(count))
counts <- counts %>%
  dplyr::left_join(regioncount, by = "region")
rm(regioncount)

# make histogram of country coloured by region, sorted by region+counts, to match map
counts %>%
  dplyr::arrange(-regioncount, -count) %>%
  dplyr::mutate(country = forcats::fct_inorder(country)) %>%
  ggplot(aes(x = country, y = count, fill = region)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_brewer(palette = "YlGnBu") +
  xlab("Country") +
  ylab("Number of Publications") +
  coord_flip()
ggsave("figures/countries_by_region_histogram.pdf", width = 8, height = 7)

# checking my countries against "admin" column of world map, need to change:
# United States -> United States of America
counts$country <- gsub("United States", "United States of America", counts$country)

# join data together
worldmapcount <- worldmap %>%
  dplyr::select(country = admin, geometry) %>%
  dplyr::left_join(counts, by = "country") %>%
  dplyr::mutate(countlog = log(replace_na(count, 0)))

# then plot world map coloured by number of studies
ggplot(worldmapcount) +
  geom_sf(aes(fill = countlog), linewidth = 0.1) +
  theme_void() +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "#F0F0F0") +
  labs(fill = "No. studies (log)")
ggsave("figures/map_countrycounts.pdf", width = 6, height = 4)


# make histogram of biodiversity category, counting some pubs with multiple cats in multiple bars
# coloured by wind or solar, arranged by # studies??
# create table of country count data
sysrevdata %>%
  dplyr::select(title, biodiversity_category, wind_solar) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(biodiversity_category, delim = ", ") %>%
  tidyr::separate_longer_delim(wind_solar, delim = "_") %>%
  dplyr::group_by(biodiversity_category, wind_solar) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(-count) %>%
  dplyr::mutate(biodiversity_category = forcats::fct_inorder(biodiversity_category)) %>%
  ggplot(aes(x = biodiversity_category, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Biodiversity Category") +
  coord_flip() +
  ylab("Number of Publications") +
  labs(fill = "Facility type")
ggsave("figures/biodiversity_cat_histogram.pdf", width = 6, height = 2.5)

# make histogram of biodiversity measure, counting some pubs with multiple cats in multiple bars
sysrevdata %>%
  dplyr::select(title, biodiversity_metric) %>%
  dplyr::distinct() %>%
  tidyr::separate_longer_delim(biodiversity_metric, delim = ", ") %>%
  ggplot(aes(x = biodiversity_metric, fill = biodiversity_metric)) +
  geom_bar() +
  ggpubr::theme_pubr(legend = "none") +
  xlab("Biodiversity Metric") +
  ggpubr::rotate_x_text(angle = 20) +
  ylab("Number of Publications") +
  scale_fill_discrete(type = colorRampPalette(RColorBrewer::brewer.pal(9, name = 'YlGnBu'))(11))
ggsave("figures/biodiversity_metric_histogram.png", width = 5, height = 4)

