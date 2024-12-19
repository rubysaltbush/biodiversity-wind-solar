# figures from final review data ----

# get data
sysrevdata <- readr::read_csv("data_input/wind_solar_biodiversity_sysreview.csv")

# how many papers included currently?

length(unique(sysrevdata$title))
# 351

# filter to just unique papers, then count # wind, # solar
sysrevdata %>%
  dplyr::select(title, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::select(wind_solar) %>%
  table()
# wind_solar
# solar         wind windandsolar 
# 50          300            1 

# filter to just unique papers, then count # impact, # mitigation
sysrevdata %>%
  dplyr::select(title, impact_mitigation) %>%
  dplyr::distinct() %>%
  dplyr::select(impact_mitigation) %>%
  table()
# impact_mitigation
# impact impactandmitigation          mitigation 
# 301                  30                  20 

# filter to just unique papers, then count wind/solar vs impact/mitigation
sysrevdata %>%
  dplyr::select(title, wind_solar, impact_mitigation) %>%
  dplyr::distinct() %>%
  dplyr::group_by(wind_solar, impact_mitigation) %>%
  dplyr::summarise(count_by_windsolarimpactmitigation = n())
# wind_solar   impact_mitigation               count_by_windsolarimpactmitigation
# <chr>        <chr>                                            <int>
# 1 solar      impact                                            36
# 2 solar      impact_mitigation                                  5
# 3 solar      mitigation                                         9
# 4 wind       impact                                           264
# 5 wind       impact_mitigation                                 25
# 6 wind       mitigation                                        11
# 7 wind_solar impact                                             1


# locations ----

# count where studies located
sysrevdata %>%
  dplyr::select(title, region) %>%
  dplyr::distinct() %>%
  dplyr::select(region) %>%
  table()
# region
# Africa          Asia        Europe North America       Oceania South America 
# 8            22           141           164             6            10 

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
  labs(colour = "Facility type")
ggsave("figures/map_study_locations.png", width = 6, height = 4)
rm(worldmap, sysrevlatlong)

# and table for specific numbers
sysrevdata %>%
  dplyr::select(title, biodiversity_category) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$biodiversity_category)) %>% 
  dplyr::select(biodiversity_category) %>% 
  table()
# biodiversity_category
# amphibians        animals      bats       bats, birds    bats, birds, invertebrates 
# 2                 1            94          12                1 
# bats, invertebrates     birds     birds, fish, invertebrates    birds, mammals    birds, reptiles 
# 1                        164          1                          4                1 
# invertebrates      mammals        plants       plants, animals     pollinators, invertebrates 
# 15                  21            19             2                   3 
# reptiles 
# 10 

# make histogram of effect of wind and solar
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
  scale_fill_discrete(name = "Facility")
ggsave("figures/effect_development_barchart.png", width = 5, height = 4)

# mitigation ----

# now a bar chart showing mitigations suggested, and mitigation heirarchy
# given some have multiple suggestions have to summarise data a bit
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
#   no  yes 
#   9   19 

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
# mitigation_category           wind_solar    count
# <chr>                         <chr>         <int>
# 1 curtailment                 wind           4
# 2 offsetting                  solar          1
# 3 offsetting                  wind           2
# 4 on-site restoration         solar          5
# 5 on-site restoration         wind           1
# 6 siting                      solar          2
# 7 siting                      wind           3
# 8 technological modifications solar          1
# 9 technological modifications wind           5

# explicit tests of mitigation
sysrevdata %>%
  dplyr::select(title, mitigation_tested) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_tested)) %>% 
  dplyr::select(mitigation_tested) %>% 
  table()
# mitigation_tested
# no yes 
# 20   8 

# explicit tests of mitigation
sysrevdata %>%
  dplyr::select(title, mitigation_supported_by_explicit_test) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_supported_by_explicit_test)) %>% 
  dplyr::select(mitigation_supported_by_explicit_test) %>% 
  table()
# mitigation_supported_by_explicit_test
# no yes 
# 1   8 

