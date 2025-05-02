# script for preprocessing of systematic review data, as exported by Rayyan

##### data processing ####

# import data directly from Rayyan download
rayyanexport <- readr::read_csv("data_input/Rayyan_take3_export_articles.csv")

# need to get notes out and into their own columns
# which will be: wind_solar, topic (impact/mitigation), construction_operation, 
# region, country, biodiversity_category, biodiversity_metric

# extract wind/solar
rayyanexport$wind_solar <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                        pattern = "wind|solar"))
# only one with both wind and solar, replace
rayyanexport$wind_solar <- gsub("c\\(\"wind\", \"solar\"\\)", "wind_solar", 
                                rayyanexport$wind_solar)
table(rayyanexport$wind_solar)
# solar       wind wind_solar 
# 50        301          1 

# extract topic (impact/mitigation)
rayyanexport$impact_mitigation <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                               pattern = "impact|mitigation"))
# many with impact and mitigation, replace with new category
rayyanexport$impact_mitigation <- gsub("c\\(\"impact\", \"mitigation\"\\)", "impact_mitigation", 
                                       rayyanexport$impact_mitigation)
table(rayyanexport$impact_mitigation)
# impact impact_mitigation        mitigation 
# 301                30                20 

# extract construction_operation
rayyanexport$construction_operation <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                                    pattern = "construction|operation"))
# some with construction and operation, replace with new category
rayyanexport$construction_operation <- gsub("c\\(\"operation\", \"construction\"\\)", "construction_operation", 
                                            rayyanexport$construction_operation)
table(rayyanexport$construction_operation)
# construction construction_operation              operation 
# 8                     24                    319 

# extract region
rayyanexport$region <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                    pattern = "Asia|Africa|North America|South America|Europe|Oceania"))
# South African countries get two "Africa", replace with only one
rayyanexport$region <- gsub("c\\(\"Africa\", \"Africa\"\\)", "Africa",
                            rayyanexport$region)
table(rayyanexport$region)
# Africa          Asia        Europe North America       Oceania South America 
# 8            22           141           164             6            10 

# extract country
rayyanexport$country <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                     pattern = "Australia|Argentina|Austria|Belgium|Brazil|Bulgaria|Canada|Chile|China|Croatia|Czech Republic|Denmark|Egypt|Ethiopia|Finland|France|Galapagos|Germany|Greece|Hungary|India|Ireland|Israel|Italy|Japan|Korea|Mexico|Netherlands|New Caledonia|New Zealand|Norway|Poland|Portugal|Puerto Rico|Romania|Slovakia|South Africa|Spain|Sweden|Switzerland|Taiwan|Turkey|United Kingdom|United States"))
# a few studies in multiple countries, replace as needed
rayyanexport$country <- gsub("c\\(\"Canada\", \"United States\"\\)", "Canada, United States",
                             rayyanexport$country)
rayyanexport$country <- gsub("c\\(\"France\", \"Belgium\"\\)", "France, Belgium",
                             rayyanexport$country)
rayyanexport$country <- gsub("c\\(\"Germany\", \"Netherlands\"\\)", "Germany, Netherlands",
                             rayyanexport$country)
rayyanexport$country <- gsub("c\\(\"Sweden\", \"Austria\", \"Germany\"\\)", "Sweden, Austria, Germany",
                             rayyanexport$country)
# a few countries have more accurate names, replace these
rayyanexport$country <- gsub("Czech Republic", "Czechia", rayyanexport$country)
rayyanexport$country <- gsub("Korea", "South Korea", rayyanexport$country)
table(rayyanexport$country)
# Australia               Austria                  Belgium 
# 4                        1                        3 
# Brazil                 Bulgaria                   Canada 
# 7                        2                       15 
# Canada, United States   Chile                    China 
# 4                        2                       12 
# Croatia                  Czechia                  Denmark 
# 1                        1                        2 
# Egypt                 Ethiopia                  Finland 
# 1                        1                        3 
# France          France, Belgium                  Germany 
# 12                        1                       15 
# Germany, Netherlands      Greece                  Hungary 
# 1                        1                        4 
# India                  Ireland                   Israel 
# 1                        2                        2 
# Italy                    Japan                   Mexico 
# 8                        2                        7 
# Netherlands            New Caledonia              New Zealand 
# 3                        1                        1 
# Norway                   Poland                 Portugal 
# 12                        7                        2 
# Puerto Rico               Romania                 Slovakia 
# 1                        3                        1 
# South Africa            South Korea               Spain 
# 6                        1                       25 
# Sweden Sweden, Austria, Germany              Switzerland 
# 10                        2                        1 
# Taiwan                   Turkey           United Kingdom 
# 3                        1                       18 
# United States 
# 138

# extract biodiversity_category
rayyanexport$biodiversity_category <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                                   pattern = "amphibian|animals|bats|bird|fish|invertebrates|mammal|plants|pollinators|reptile"))
# simplify multiple categories
rayyanexport$biodiversity_category <- gsub("c\\(\"bats\", \"bird\", \"invertebrates\"\\)", "bats, bird, invertebrates",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"bats\", \"invertebrates\"\\)", "bats, invertebrates",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"bats\", \"bird\"\\)", "bats, bird",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"bird\", \"fish\", \"invertebrates\"\\)", "bird, fish, invertebrates",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"bird\", \"mammal\"\\)", "bird, mammal",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"bird\", \"reptile\"\\)", "bird, reptile",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"plants\", \"animals\"\\)", "plants, animals",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("c\\(\"pollinators\", \"invertebrates\"\\)", "pollinators, invertebrates",
                                           rayyanexport$biodiversity_category)
# make pluralisation consistent
rayyanexport$biodiversity_category <- gsub("bird", "birds",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("amphibian", "amphibians",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("mammal", "mammals",
                                           rayyanexport$biodiversity_category)
rayyanexport$biodiversity_category <- gsub("reptile", "reptiles",
                                           rayyanexport$biodiversity_category)
table(rayyanexport$biodiversity_category)
# amphibians                    animals                       bats                bats, birds bats, birds, invertebrates 
# 2                          1                         94                         12                          1 
# bats, invertebrates                      birds birds, fish, invertebrates             birds, mammals            birds, reptiles 
# 1                        164                          1                          4                          1 
# invertebrates                    mammals                     plants            plants, animals pollinators, invertebrates 
# 15                         21                         19                          2                          3 
# reptiles 
# 10 

# extract biodiversity_metric
rayyanexport$biodiversity_metric <- as.character(str_extract_all(string = rayyanexport$notes, 
                                                                 pattern = "abundance|behaviour|distribution|diversity|genetics|habitat|health|mortality|native cover|reproduction|survival"))
# simplify multiple categories
rayyanexport$biodiversity_metric <- gsub("c\\(\"abundance\", \"behaviour\", \"mortality\"\\)", "abundance, behaviour, mortality",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"abundance\", \"behaviour\"\\)", "abundance, behaviour",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"abundance\", \"distribution\"\\)", "abundance, distribution",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"abundance\", \"diversity\"\\)", "abundance, diversity",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"abundance\", \"reproduction\"\\)", "abundance, reproduction",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"behaviour\", \"distribution\"\\)", "behaviour, distribution",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"behaviour\", \"diversity\"\\)", "behaviour, diversity",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"behaviour\", \"mortality\"\\)", "behaviour, mortality",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"distribution\", \"reproduction\"\\)", "distribution, reproduction",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"health\", \"distribution\"\\)", "health, distribution",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"health\", \"mortality\"\\)", "health, mortality",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"mortality\", \"distribution\", \"reproduction\"\\)", "mortality, distribution, reproduction",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"mortality\", \"reproduction\"\\)", "mortality, reproduction",
                                         rayyanexport$biodiversity_metric)
rayyanexport$biodiversity_metric <- gsub("c\\(\"survival\", \"behaviour\"\\)", "survival, behaviour",
                                         rayyanexport$biodiversity_metric)
table(rayyanexport$biodiversity_metric)
# abundance                  abundance, behaviour       abundance, behaviour, mortality 
# 15                                     1                                     1 
# abundance, distribution                  abundance, diversity               abundance, reproduction 
# 1                                     4                                     1 
# behaviour               behaviour, distribution                  behaviour, diversity 
# 94                                     1                                     1 
# behaviour, mortality                          distribution            distribution, reproduction 
# 8                                    13                                     1 
# diversity                              genetics                               habitat 
# 39                                     1                                     1 
# health                  health, distribution                     health, mortality 
# 6                                     2                                     1 
# mortality mortality, distribution, reproduction               mortality, reproduction 
# 129                                     1                                     2 
# native cover                          reproduction                              survival 
# 2                                    16                                     9 
# survival, behaviour 
# 1 

#### export processed data ####
# export this data now for further processing and additional data entry in excel
readr::write_csv(rayyanexport, "data_output/rayyan_export_processed.csv")

# from here exported data manually filled in and refined, some records removed
# as deeper reading of the papers showed that they were not relevant

