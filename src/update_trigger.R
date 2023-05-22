library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(exactextractr)
library(sf)
library(terra)
library(purrr)
library(googledrive)



# authorize drive access
drive_auth(
    path = Sys.getenv("YEM_TRIG_MONITORNG_JSON")
)

# load AOI
aoi_drive <- drive_get(id = "1-2JIxDekilSor0YoNySLUdfTfQ9JgQRH")
drive_download(aoi_drive,path = f <- tempfile(fileext = ".rds"))
aoi <- read_rds(f)

boom <- load_chirps_gefs_cropped(run_date = Sys.Date()-1,
                                 leadtime=1,
                                 mask=aoi,
                                 write_outputs = T)
