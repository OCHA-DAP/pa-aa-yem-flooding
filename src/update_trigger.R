library(lubridate)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(exactextractr)
library(sf)
library(terra)
library(purrr)
library(googledrive)
library(blastula)
source("R/load_chirps_gefs.R")
source("R/chirps_gefs_scraper.R")

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
  filter(name == "high_risk_district_hulls_buff.rds")

# load AOI rds as sf class
aoi_drive <- drive_get(id = aoi_drib$id)
drive_download(aoi_drive, path = f <- tempfile(fileext = ".rds"))
aoi <- read_rds(f)


# add some archiving operations?

# extract chirps-gefs -----------------------------------------------------
# get zonal mean by governorate for each leadtime
# will use output object in memory for next processing steps
# but raster tifs and zonal stats are also written to gdrive.

date_to_run <- Sys.Date()
is_post_monitoring <- ifelse(year(date_to_run)>=2024,TRUE,FALSE)

gefs_processed_time <- system.time(gefs_processed <- conditionally_load_latest_chirps_gefs(
        leadtime = 1:10,
        mask = aoi,
        write_outputs = T,
        gdrive_dribble=drive_dribble,
        raster_drive=r_drib,
        zonal_drive=zonal_stats_drib
)
)
meta_tbl <- latest_gefs_metadata()
dt_made<- unique(meta_tbl$forecast_made)
dt_made_chr <- gsub(" 0", " ", format(as_date(dt_made), " - %d %B %Y"))



if(!is.null(gefs_processed)){
    chirps_gefs_zonal <- gefs_processed$zonal_means
    chirps_gefs_rstack <- gefs_processed$raster_stack
    
    # email:
    
    # Load in e-mail credentials
    email_creds <- creds_envvar(
        user = Sys.getenv("CHD_DS_EMAIL_USERNAME"),
        pass_envvar = "CHD_DS_EMAIL_PASSWORD",
        host = Sys.getenv("CHD_DS_HOST"),
        port = Sys.getenv("CHD_DS_PORT"),
        use_ssl = TRUE
    )
    
    email_subj <- if_else(
        is_post_monitoring,
        paste0("Post Monitoring: Yemen AA Rainfall Forecast Monitoring", dt_made_chr),
        paste0("Email Test: Yemen AA Rainfall Forecast Monitoring", dt_made_chr),
        
    )
    
    email_to_col <- ifelse(is_post_monitoring,"to_post","to")
    receps_drive <- drive_get(id = "10PkgaVJZhJIjoOd_31P55UWcRcZzS0Zo")
    drive_download(receps_drive, path = f <- tempfile(fileext = ".csv"))
    email_to <- read_csv(f) %>% 
        # mutate(
        #     to = ifelse(str_detect(email_address,"zac"),T,F)
        # ) %>%
        filter(!!sym(email_to_col)) |> 
        pull(email_address)
    
    render_email(
        input = file.path(
            "src",
            "email",
            "email.Rmd"
        ),
        envir = parent.frame()
    ) %>%
        smtp_send(
            to = email_to,
            from = "data.science@humdata.org",
            subject = email_subj,
            credentials = email_creds
        )
}


