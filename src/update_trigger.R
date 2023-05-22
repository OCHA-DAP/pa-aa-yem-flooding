library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(exactextractr)
library(sf)
library(terra)
library(purrr)
library(googledrive)
source("R/load_chirps_gefs.R")

# authorize drive access
drive_auth(
  path = Sys.getenv("YEM_TRIG_MONITORNG_JSON")
)

# load AOI
aoi_drive <- drive_get(id = "1-2JIxDekilSor0YoNySLUdfTfQ9JgQRH")
drive_download(aoi_drive, path = f <- tempfile(fileext = ".rds"))
aoi <- read_rds(f)


# extract chirps-gefs -----------------------------------------------------
# get zonal mean by governorate for each leadtime
# will use output object in memory for next processing steps
# but raster tifs and zonal stats are also written to gdrive.
chirps_gefs_zonal <- load_chirps_gefs_cropped(
  run_date = Sys.Date(),
  leadtime = c(1:10),
  mask = aoi,
  write_outputs = T
)


# rolling sums ------------------------------------------------------------



# viz ---------------------------------------------------------------------


