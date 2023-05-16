library(lubridate)
library(tidyverse)
library(exact_extractr)
library(sf)
library(terra)

source("R/load_chirps_gefs")


load_chirps_gefs_cropped(leadtime=1:2, mask=roi,write_outputs = T)