# scratch script trying to use Hansen global forest cover mapping with wind and solar
# farm locations

# okay have downloaded tiles from Hansen et al v1.11 to cover all Australia
# could stitch together into single raster???

files <- list.files("data_cache/Hansen_forest_lossyear")

lossyear <- terra::sprc(paste0("data_cache/Hansen_forest_lossyear/", files))

lossyearall <- terra::merge(lossyear)

writeRaster(lossyearall, "data_cache/Hansen_GFC_lossyear_aus.tif")

rm(lossyear, files)

# now create buffer of points
windsolarV <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
# buffer points by 1km
windsolarVbuff <- sf::st_buffer(windsolarV, dist = 1000)
plot(windsolarVbuff)

# now extract proportion of 0 (no clearing) values per buffer area from 
# forest loss data

test <- exactextractr::exact_extract(lossyearall, windsolarVbuff, 
                     fun = "frac")
# it worked! 

# join output to OG data, then export so can read back in in future
windsolar <- dplyr::bind_cols(windsolar, test)

write_csv(windsolar, "data_output/windsolar_with_hansenetalGFCfractionofforestloss.csv")

# hmm but how to visualise this data??
# boxplot by wind/solar of how much not cleared?
ggplot(windsolar, aes(x = type, y = frac_0)) +
  geom_boxplot() +
  theme_pubr()

# hard to assess significance of this. hmm.