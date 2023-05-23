library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(exactextractr)
library(sf)
library(terra)
library(purrr)
library(googledrive)
library(blastula)
source("R/load_chirps_gefs.R")

# authorize drive access
drive_auth(
  path = Sys.getenv("YEM_TRIG_MONITORNG_JSON")
)


# access necessary GDRIVE paths -------------------------------------------
# get gdrive directories
drive_dribble <- drive_ls(
    corpus = "user"
)
# will store tifs here.
r_drib <- drive_dribble %>%
    filter(name == "chirps_gefs_rasters")

# will store zonal mean csvs here
zonal_stats_drib <- drive_dribble %>%
    filter(name == "chirps_gefs_zonal")

# where AOI file is (doesn't change)
aoi_drib <- drive_dribble %>% 
    filter(name=="high_risk_hulls.rds")

# load AOI rds as sf class
aoi_drive <- drive_get(id = aoi_drib$id)
drive_download(aoi_drive, path = f <- tempfile(fileext = ".rds"))
aoi <- read_rds(f)


# add some archiving operations?

# extract chirps-gefs -----------------------------------------------------
# get zonal mean by governorate for each leadtime
# will use output object in memory for next processing steps
# but raster tifs and zonal stats are also written to gdrive.


gefs_process_time <- system.time(
  gefs_processed <- load_chirps_gefs_cropped(
  run_date = Sys.Date(), 
  leadtime = c(1), 
  mask = aoi,
  write_outputs = T,
  raster_drive = r_drib,
  zonal_drive = zonal_stats_drib
)
)


chirps_gefs_zonal <- gefs_processed$zonal_means
chirps_gefs_rstack <-  gefs_processed$raster_stack

# email:

# Load in e-mail credentials
email_creds <- creds_envvar(
    user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
    pass_envvar = "CHD_DS_EMAIL_PASSWORD",
    host = Sys.getenv("CHD_DS_HOST"),
    port = Sys.getenv("CHD_DS_PORT"),
    use_ssl = TRUE
)


# load in recipients

receps_drive <- drive_get(id="10PkgaVJZhJIjoOd_31P55UWcRcZzS0Zo")
drive_download(receps_drive, path = f <- tempfile(fileext = ".csv"))
df_recipients <- read_csv(f)


render_email(input = "src/email/email.rmd",
    envir = parent.frame()
    ) %>% 
    smtp_send(
        to = df_recipients$to,
        # bcc = filter(df_recipients, !to)$email,
        from = "data.science@humdata.org",
        subject = paste0("Email Test: Yemen AA Rainfall Forecast Monitoring (",Sys.Date(),")"),
        credentials = email_creds
    )

