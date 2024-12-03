library(terra)
library(sf)
library(tidyverse)
library(rnaturalearth)

generate_density_plot <- function(raster_map, solar_data, wind_data, region = "Australia",log=FALSE) {
  # Load the region polygon
  region_polygon <- rnaturalearth::ne_countries(scale = "medium", country = region, returnclass = "sf")
  
  # Convert region to SpatVector for terra operations
  region_terra <- vect(region_polygon)
  
  # Get the extent and crop the raster to the region
  region_ext <- ext(region_terra)
  region_ext_new <- ext(135, region_ext$xmax, region_ext$ymin, region_ext$ymax)
  raster_cropped <- crop(raster_map, region_ext_new)
  region_cropped <- crop(region_terra, region_ext_new)
  
  # Mask the raster with the region shape
  raster_masked <- mask(raster_cropped, region_cropped)
  
  # Convert the wind and solar data to sf points
  wind_points_sf <- st_as_sf(data.frame(lon = wind_data$Lon, lat = wind_data$Lat), 
                             coords = c("lon", "lat"), crs = st_crs(raster_cropped))
  solar_points_sf <- st_as_sf(data.frame(lon = solar_data$Lon, lat = solar_data$Lat), 
                              coords = c("lon", "lat"), crs = st_crs(raster_cropped))
  
  # Extract raster values at the points
  wind_temp <- extract(raster_masked, wind_points_sf)
  solar_temp <- extract(raster_masked, solar_points_sf)
  
  # Convert the full raster to a data frame
  all_temp <- terra::as.data.frame(raster_masked)
  
  # Add a source column to each data frame
  wind_temp <- wind_temp %>% mutate(source = "Wind")
  solar_temp <- solar_temp %>% mutate(source = "Solar")
  all_temp <- all_temp %>% 
    mutate(ID = NA,  # Add an ID column for consistency
           source = "All of Eastern Australia")
  
  # Rename the temperature column for consistency
  col_name <- names(all_temp)[1]
  all_temp <- all_temp %>% rename(temperature = !!col_name)
  wind_temp <- wind_temp %>% rename(temperature = !!col_name)
  solar_temp <- solar_temp %>% rename(temperature = !!col_name)
  
  # Combine all temperature data frames
  combined_temp <- bind_rows(wind_temp, solar_temp, all_temp)
  
  # Create the density plot
  plot <- ggplot(combined_temp, aes(x = temperature, color = source, fill = source)) +
    geom_density(alpha = 0.5) +
    labs(
      title = "Density Comparison of Temperature Values",
      x = "Temperature",
      y = "Density",
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal()
  if(log){
    # a bit hacky here
    plot <- ggplot(combined_temp, aes(x = temperature, color = source, fill = source)) +
      geom_density(alpha = 0.5) +
      labs(
        title = "Density Comparison of Precip Values",
        x = "Mean annual precip",
        y = "Density",
        color = "Group",
        fill = "Group"
      ) +scale_x_log10()+
      theme_minimal()+
      theme(legend.position = "none")
  }
  return(plot)
}

# Example usage:
solar <- filter(read_csv("data_input/solar_nationalmap_20241203.csv"), !is.na(Lon))
wind <- filter(read_csv("data_input/wind_nationalmap_20241203.csv"), !is.na(Lon))
temp_raster <- rast("maps/CHELSA_bio1_1981-2010_V.2.1.tif")
rain_raster <- rast("maps/CHELSA_bio12_1981-2010_V.2.1.tif")


temp_plot <- generate_density_plot(temp_raster, solar, wind)
rain_plot <- generate_density_plot(rain_raster, solar, wind,log=TRUE)


library(patchwork)
temp_plot+rain_plot
