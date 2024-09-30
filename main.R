# source(scripts/install_packages.R) # uncomment and run to install necessary packages

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggpubr)
install.packages("wordcloud")
library(wordcloud)
install.packages("tm")
library(tm)

# get data
sysrevdata <- readr::read_csv("data_input/wind_solar_biodiversity_sysreview_v2_20240930.csv")

# how many papers included currently?

length(unique(sysrevdata$title))
# 315

# filter to just unique papers, then count # wind, # solar
sysrevdata %>%
  dplyr::select(title, wind_solar) %>%
  dplyr::distinct() %>%
  dplyr::select(wind_solar) %>%
  table()
# wind_solar
# solar         wind windandsolar 
# 44          270            1 

# filter to just unique papers, then count # impact, # mitigation
sysrevdata %>%
  dplyr::select(title, topic) %>%
  dplyr::distinct() %>%
  dplyr::select(topic) %>%
  table()
# topic
# impact impactandmitigation          mitigation 
# 275                  20                  20 

# filter to just unique papers, then count wind/solar vs impact/mitigation
sysrevdata %>%
  dplyr::select(title, wind_solar, topic) %>%
  dplyr::distinct() %>%
  dplyr::group_by(wind_solar, topic) %>%
  dplyr::summarise(count_by_windsolarimpactmitigation = n())
# wind_solar   topic               count_by_windsolarimpactmitigation
# <chr>        <chr>                                            <int>
# 1 solar        impact                                              32
# 2 solar        impactandmitigation                                  4
# 3 solar        mitigation                                           8
# 4 wind         impact                                             242
# 5 wind         impactandmitigation                                 16
# 6 wind         mitigation                                          12
# 7 windandsolar impact                                               1

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

# locations ----

# count where studies located
sysrevdata %>%
  dplyr::select(title, region) %>%
  dplyr::distinct() %>%
  dplyr::select(region) %>%
  table()
# region
# Asia        Europe North America 
# 6            13            11 

# make map of locations of study
# first get world map data without Antarctica (not included)
worldmap <- rnaturalearth::ne_countries() %>% 
  filter(sovereignt != "Antarctica")

# then get spatial version of sys review data where lat/long available
sysrevlatlong <- sysrevdata %>%
  dplyr::select(title, wind_solar, latitude, longitude) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::distinct() #%>%
  #sf::st_as_sf(coords = c("latitude", "longitude"))

# then plot background world map and points coloured by wind or solar
ggplot() +
  geom_sf(data = worldmap, fill = "grey", colour = "white", linewidth = 0.1) +
  theme_void() +
  geom_point(data = sysrevlatlong, aes(x = longitude, y = latitude, colour = wind_solar)) +
  labs(colour = "Facility type")
ggsave("figures/map_study_locations.png", width = 6, height = 4)
rm(worldmap, sysrevlatlong)

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

# and table for specific numbers
sysrevdata %>%
  dplyr::select(title, biodiversity_category) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$biodiversity_category)) %>% 
  dplyr::select(biodiversity_category) %>% 
  table()
# biodiversity_category
# Bats                       Birds  Invertebrates - Arthropods Invertebrates - Pollinators 
# 4                          11                           2                           1 
# Mammals                      Plants                    Reptiles 
# 2                           9                           1 

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

# now a bar chart showing mitigations suggested, and mitigation heirarchy
# given some have multiple suggestions have to summarise data a bit
sysrevdata %>%
  dplyr::select(title, mitigation_category) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(.$mitigation_category)) %>%
  # Split the categories and unnest them
  dplyr::mutate(mitigation_category = stringr::str_split(mitigation_category, ", ")) %>%
  tidyr::unnest(mitigation_category) %>%
  # Count occurrences of each category
  dplyr::group_by(mitigation_category) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  ggplot(aes(x = reorder(mitigation_category, -count), y = count, fill = mitigation_category)) +
  geom_bar(stat = "identity") +
  ggpubr::theme_pubr(legend = "none") +
  scale_fill_brewer(palette = "Set2") +
  xlab("Mitigation Category") +
  ggpubr::rotate_x_text(angle = 15, hjust = 0.7, vjust = 0.9) +
  ylab("Number of Publications")
ggsave("figures/mitigation_cat_barplot.png", width = 5, height = 4)


# okay now let's try to make a word cloud from the abstracts
# Step 1: Create a corpus from the abstract column
corpus <- sysrevdata %>%
  dplyr::select(abstract) %>%
  dplyr::distinct()

corpus <-  tm::Corpus(tm::VectorSource(corpus))

# Step 2: Preprocess the data (convert to lower case, remove punctuation, etc.)
corpus <- tm_map(corpus, content_transformer(tolower))         # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)                    # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                        # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english"))    # Remove common stopwords
corpus <- tm_map(corpus, stripWhitespace)                      # Remove extra white spaces

# Step 3: Create a Document-Term Matrix
dtm <- tm::TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)  # Sum term frequencies
df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)

# Step 4: Generate the word cloud
set.seed(1234)  # For reproducibility
png("figures/wordcloud_abstract.png", width = 800, height = 800, res = 300)
wordcloud::wordcloud(words = df_word_freqs$word, freq = df_word_freqs$freq, 
          min.freq = 3,                              # Minimum frequency to include
          max.words = 200,                           # Maximum number of words in the cloud
          random.order = FALSE,                      # Arrange largest words centrally
          colors = brewer.pal(8, "Dark2"))           # Color palette
dev.off()

# needs lots more cleaning, ah well. what about mitigation text?

# Step 1: Create a corpus from the abstract column
corpus <- sysrevdata %>%
  dplyr::select(mitigation_text) %>%
  dplyr::distinct()

corpus <-  tm::Corpus(tm::VectorSource(corpus))

# Step 2: Preprocess the data (convert to lower case, remove punctuation, etc.)
corpus <- tm_map(corpus, content_transformer(tolower))         # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)                    # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)                        # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english"))    # Remove common stopwords
corpus <- tm_map(corpus, stripWhitespace)                      # Remove extra white spaces

# Step 3: Create a Document-Term Matrix
dtm <- tm::TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)  # Sum term frequencies
df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)

# Step 4: Generate the word cloud
set.seed(1234)  # For reproducibility
png("figures/wordcloud_mitigationtext.png", width = 800, height = 800, res = 300)
wordcloud::wordcloud(words = df_word_freqs$word, freq = df_word_freqs$freq, 
                     min.freq = 3,                              # Minimum frequency to include
                     max.words = 200,                           # Maximum number of words in the cloud
                     random.order = FALSE,                      # Arrange largest words centrally
                     colors = brewer.pal(8, "Dark2"))           # Color palette
dev.off()

# okay yes word clouds maybe are just a bit too random to generate much of any
# insight, and need too much cleaning

rm(word_freqs, matrix, dtm, df_word_freqs, corpus)
