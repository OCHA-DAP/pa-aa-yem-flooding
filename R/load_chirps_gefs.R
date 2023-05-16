load_chirpsgefs <- function(run_date=Sys.Date(),n_days=7){
    base_url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/"
    start_date <- run_date -1
    end_date <- end_date+n_days
    
}



library(raster)

# url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/chirps-gefs_precip_v12_20230516.tif"
url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/2023/05/16/data.2023.0516.tif"
# Download the raster metadata.
raster <- raster(url)

# Create a new raster object with the desired extent.
extent <- c(-122.5, 37.5, -122.0, 38.0)
new_raster <- crop(raster, extent)

# Write the new raster to a file.
writeRaster(new_raster, "my_raster.tif")