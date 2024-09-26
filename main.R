# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(ggpubr)

# get data
sysrevdata <- readr::read_csv("data_input/wind_solar_biodiversity_sysreview_v2_20240926.csv")

# how many papers included currently?

length(unique(sysrevdata$title))
# 314

# filter to just unique papers, then count # wind, # solar
sysrevdata %>%
  dplyr::select(title, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::select(wind_solar) %>%
  table()
# wind_solar
# solar         wind windandsolar 
# 44          269            1 

# filter to just unique papers, then count # impact, # mitigation
sysrevdata %>%
  dplyr::select(title, topic) %>%
  dplyr::distinct() %>%
  dplyr::select(topic) %>%
  table()
# topic
# impact impactandmitigation          mitigation 
# 275                  19                  20 

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

# make histogram of biodiversity category (not many so far)
sysrevdata %>%
  dplyr::select(title, biodiversity_category) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$biodiversity_category)) %>%
  ggplot(aes(x = biodiversity_category, fill = biodiversity_category)) +
  geom_bar() +
  ggpubr::theme_pubr(legend = "none") +
  xlab("Biodiversity Category") +
  ggpubr::rotate_x_text(angle = 20) +
  ylab("Number of Publications")
ggsave("figures/biodiversity_cat_histogram.png", width = 5, height = 4)

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

