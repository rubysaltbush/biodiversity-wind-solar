# script to buffer wind/solar points and then analyse what change in LAI
# and forest cover pre and post construction

# LAI unfinished ----

# can then compare pre- and post- development LAI for wind/solar locations?
# theoretically should indicate change in density of vegetation
# may want to BUFFER POINTS to pick up area
# but have checked that all points are currently in the middle of the impact area
# which differs in magnitude for wind and solar - dif buffer sizes maybe?

# load in pre-summarised Leaf Area Index rasters from GLOBMAP (doi.org/10.5281/zenodo.12698637)
# for 2001-2003 and 2021-2023
mean_LAI_0103 <- terra::rast("data_output/tiff2001_2003.tif")
mean_LAI_2123 <- terra::rast("data_output/tiff2021_2023.tif")

mean_LAI_0103


# get spatial points
windsolarV <- sf::st_as_sf(windsolar, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
windsolarV

hist(extraction$lyr.1) # mostly low LAI values
hist(extraction2$lyr.1) # mostly low still, some higher
hist(windsolar$difLAI) # mostly close to 0, some -ve but most +ve

# TO DOs
#   - try buffering (prop to MW?) with leaf area index and Hansen forest cover mapping, from ~1998 (earliest start year) to most recent, to see change in cover