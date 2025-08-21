# final review data ----

# get data
sysrevdata <- readr::read_csv("data_input/wind_solar_biodiversity_sysreview_v8_20250821.csv")

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
sysrevdata %>%
  dplyr::select(title, year, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::group_by(year, wind_solar) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = year, y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Year of publication") +
  ylab("Number of studies") +
  labs(fill = "Facility type")
ggsave("figures/year_pub_histogram.pdf", width = 5, height = 4)

# where ----

# count where studies located
sysrevdata %>%
  dplyr::select(title, region) %>%
  dplyr::distinct() %>%
  dplyr::select(region) %>%
  table()
# region
# Africa       Asia         Europe       North America   Oceania   South America 
# 8            21           137           152             6            7 

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
  geom_point(data = sysrevlatlong, aes(x = longitude, y = latitude, colour = wind_solar)) +
  labs(colour = "Facility type") +
  scale_colour_discrete(type = my_colours$windsolar) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "#F0F0F0") +
  labs(fill = "No. studies (log)")
ggsave("figures/map_study_locations.pdf", width = 6, height = 4)
rm(sysrevlatlong, worldmap, worldmapcount, counts)

# who/what ----
# table of biodiversity categories
sysrevdata %>%
  dplyr::select(title, biodiversity_category) %>%
  dplyr::distinct() %>%
  dplyr::select(biodiversity_category) %>% 
  table()
# biodiversity_category
# amphibians                                                 bats 
# 2                                                          85 
# bats, birds                                        bats, birds, insects 
# 11                                                           1 
# bats, insects                                             birds 
# 1                                                         155 
# birds, bats                            birds, fish, other invertebrates 
# 1                                                           1 
# birds, other mammals                                   birds, reptiles 
# 3                                                           1 
# insects                                insects, other invertebrates 
# 11                                                           4 
# other invertebrates                                     other mammals 
# 1                                                          22 
# other mammals, amphibians                                plants 
# 1                                                          19 
# plants, insects                      plants, other mammals, birds, reptiles 
# 2                                                           1 
# plants, other mammals, birds, reptiles, other invertebrates      reptiles 
# 1                                                                   11 

# make histogram of biodiversity category, counting some pubs with multiple taxa in multiple bars
# coloured by wind or solar, arranged by # studies
sysrevdata %>%
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
  ggpubr::theme_pubr(legend = "right") +
  scale_fill_discrete(type = my_colours$windsolar) +
  xlab("Biodiversity category") +
  coord_flip() +
  ylab("Number of studies") +
  labs(fill = "Facility type")
ggsave("figures/biodiversity_cat_histogram.pdf", width = 7, height = 5)

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
sysrevdata %>%
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
  coord_flip()
ggsave("figures/biodiversity_metric_histogram.pdf", width = 7, height = 5)

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
rm(effect)

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

# now a bar chart showing mitigations suggested, and mitigation heirarchy
mitigation_cat %>%
  ggplot(aes(x = reorder(mitigation_category, -count), y = count, fill = wind_solar)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr() +
  xlab("Mitigation category") +
  labs(fill = "Facility type") +
  scale_fill_discrete(type = my_colours$windsolar) +
  ylab("Number of studies") +
  coord_flip()
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
sysrevdata %>%
  dplyr::filter(mitigation_tested == "TRUE") %>%
  dplyr::select(title, mitigation_category, mitigation_supported_by_explicit_test) %>%
  dplyr::distinct() %>%
  # Split the categories and unnest them
  dplyr::mutate(mitigation_category = stringr::str_split(mitigation_category, ", ")) %>%
  tidyr::unnest(mitigation_category) %>%
  # Count occurrences of each category
  dplyr::group_by(mitigation_category, mitigation_supported_by_explicit_test) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(mitigation_supported_by_explicit_test = factor(mitigation_supported_by_explicit_test, levels = c("TRUE", "FALSE"))) %>%
  ggplot(aes(x = reorder(mitigation_category, -count), y = count, fill = mitigation_supported_by_explicit_test)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr() +
  xlab("Mitigation category") +
  labs(fill = "Mitigation supported") +
  scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
  ylab("Number of studies") +
  coord_flip()
ggsave("figures/mitigation_support_barchart.pdf", width = 7, height = 5)


