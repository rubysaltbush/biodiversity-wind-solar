# script to pre-process GLOBMAP LAI rasters, creating mean raster for global LAI
# from 2001-2003 and 2021-2023 (to even out intra- and inter-annual variation in LAI)

# decision point - could get e.g. 1995-1998 LAI data (pre-any of our wind farms)
# but this is from AVHRR while post-2000 is from MODIS (better quality)

LAI_rasters <- list()

LAI_rasters$tiff2001_2003 <- terra::rast(list.files(path = "data_cache/2001-2003_TIFF", 
                                                    pattern='.tif$', 
                                                    all.files = TRUE, 
                                                    full.names = TRUE))

LAI_rasters$tiff2021_2023 <- terra::rast(list.files(path = "data_cache/2021-2023_TIFF", 
                                                    pattern='.tif$', 
                                                    all.files = TRUE, 
                                                    full.names = TRUE))

# loop through rasters, calculate mean raster for each period and
# divide by 100 to make mean Leaf Area Index from 0-10
mean_LAI <- list()
for(name in names(LAI_rasters)){
  mean_LAI[[name]] <- terra::app(x = LAI_rasters[[name]],
                                 fun = function(x){mean(x)/100},
                                 filename = paste("data_output/", name, ".tif", sep = ""),
                                 #na.rm = TRUE,
                                 overwrite = TRUE)
}

rm(mean_LAI, LAI_rasters)
