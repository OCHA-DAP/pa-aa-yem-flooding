library(lubridate)
library(tidyverse)
library(exactextractr)
library(sf)
library(terra)
library(googledrive)
source("R/load_chirps_gefs.R")
source("R/googledrive.R")



# authorize drive access
drive_auth(
    path = Sys.getenv("YEM_TRIG_MONITORNG_JSON")
)


# need to change these to googledrive functions
roi_fp <- file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","live_monitoring","inputs", "high_risk_hulls.rds")
roi <- read_rds(roi_fp)



# I don't get why terra::rast() sometimes takes so long to download urls
boom <- load_chirps_gefs_cropped(run_date = Sys.Date(),
                                 leadtime=1,
                                 mask=roi,
                                 write_outputs = T)
