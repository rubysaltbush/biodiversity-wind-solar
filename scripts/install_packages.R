# below will install all necessary packages to run this project

install.packages("tidyverse")
install.packages("ggpubr")
install.packages("sf")
install.packages("terra")
install.packages("rnaturalearth")
install.packages("jsonlite") # reading json data
install.packages("networkD3") # Sankey diagram
install.packages("htmlwidgets") # saving Sankey diagram
install.packages("webshot") # converting Sankey html to pdf
webshot::install_phantomjs() # needed for above package to work