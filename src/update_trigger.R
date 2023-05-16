library(lubridate)
library(tidyverse)
library(exact_extractr)
library(sf)
library(terra)
library(googledrive)
source("R/load_chirps_gefs")



# authorize drive access
drive_auth(
    path = Sys.getenv("YEM_TRIG_MONITORNG_JSON")
)


load_chirps_gefs_cropped(leadtime=1:2, mask=roi,write_outputs = T)