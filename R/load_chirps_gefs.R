#' load_chirps_gefs_cropped
#' @description
#' the `load_chirps_gefs_cropped` function is used to get the inputs required to monitor the available chirps-gefs forecast data
#' for the pilot AA Yemen trigger. The function extracts CHIRPS-GEFS data produced for any given day. This will
#' be set on a CRON job to run just after the data is uploaded to: https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/.
#' The function is utilized in `src/update_trigger.R` which is the script which is set to run as a GH actions workflow.
#'
#' @param run_date \code{character} date of run in YYYY-MM-DD format (default = Sys.Date())
#' @param leadtime \code{integer} vector indicating all leadtimes to download data for (default= c(1:10)),
#'   leadtime 1 = 24 hours.
#' @param mask spatial file in wgs84 projection to use for cropping rasters and calculating zonal stats
#' @param write_outputs \code{logical} if T (default) write outputs to gdrive.
#' @param raster_drive dribble (`{googledrive}` class object) containing the location to store the rasters on the drive 
#' @param zonal_drive dribble (`{googledrive}` class object) containing the location to store the zonal stats csvs.
#' @return
#' 1. CHIRPS-GEFS raster tifs clipped to mask bounding box.
#' 2. Zonal mean by governorate for each raster as in memory object and csv.
#' 
#' @examples \dontrun{
#' library(lubridate)
#' library(dplyr)
#' library(readr)
#' library(tidyr)
#' library(exactextractr)
#' library(sf)
#' library(terra)
#' library(purrr)
#' library(googledrive)
#' source("R/load_chirps_gefs.R")
#' # authorize drive access
#' drive_auth(
#'     path = Sys.getenv("YEM_TRIG_MONITORNG_JSON")
#'     )
#' # load AOI
#' aoi_drive <- drive_get(id = "1-2JIxDekilSor0YoNySLUdfTfQ9JgQRH")
#' drive_download(aoi_drive, path = f <- tempfile(fileext = ".rds"))
#' aoi <- read_rds(f)
#' chirps_gefs_zonal <- load_chirps_gefs_cropped(
#'     run_date = Sys.Date(),
#'     leadtime = c(1:10),
#'     mask = aoi,
#'     write_outputs = T
#'     )
#' }

load_chirps_gefs_cropped <- function(run_date = Sys.Date(),
                                     leadtime = 1:10,
                                     mask = roi,
                                     write_outputs = T,
                                     raster_drive=r_drib,
                                     zonal_drive=zonal_stats_drib) {


  # get chirps gefs url paths
  base_url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/"
  forecast_dir_url <- format(run_date, "%Y/%m/%d")
  url_dir <- paste0(base_url, forecast_dir_url)

  leadtime_adj <- leadtime - 1 # chirps-gefs stores first 24 hours forecast as Sys.Date()
  forecasted_dates <- run_date + leadtime_adj
  
  # format layer names for simple zonal mean extraction later
  lyr_names <- paste0(format(run_date, "%Y-%m-%d"), ".", leadtime)
  
  # output file names
  gdrive_file_name <- paste0("yem_aoi_chirps_gefs_", lyr_names, ".tif")

  url_file_names <- paste0("data.", format(forecasted_dates, "%Y.%m%d"), ".tif")
  url_downloads <- paste0(url_dir, "/", url_file_names)

  r_cropped_list <- url_downloads %>%
    map2(lyr_names, \(url, lyr_name){
      cat("downloading ", lyr_name, " to memory\n")
      r <- rast(url)
      cat("cropping ", lyr_name, " to mask\n")
      r_cropped <- crop(x = r, y = mask)
      terra::set.names(r_cropped, lyr_name)
      return(r_cropped)
    })

  temp_r_paths <- file.path(tempdir(), gdrive_file_name)
  if (write_outputs) {
    r_cropped_list %>%
      map2(temp_r_paths, \(r, fname){
        cat("TEMP writing ", fname, "\n")
        writeRaster(x = r, filename = fname, overwrite = T)
      })
    temp_r_paths %>%
      map2(gdrive_file_name, \(temp_path, fname){
        cat("Uploading ", fname, " to GDRIVE")
        drive_upload(
          media = temp_path,
          path = as_id(raster_drive$id),
          name = fname
        )
      })
  }
  
  cat("stacking raster collection \n")
  r_stack <- rast(r_cropped_list) # can stack and run zonal mean on entire stack
  
  # zonal means
  roi_means <- exact_extract(
    x = r_stack,
    y = mask,
    append_cols = "governorate_name",
    fun = c("mean"),
    force_df = T,
    full_colnames = T
  ) %>% # wrangle
    pivot_longer(-matches("governorate_name")) %>%
    separate(name, into = c("stat", "date", "leadtime"), sep = "\\.") %>%
    pivot_wider(names_from = "stat", values_from = "value") %>%
    mutate(
      date = as_date(date),
      leadtime = as.integer(leadtime),
      mean = as.numeric(mean)
    )

  temp_csv_name <- paste0(
    format(run_date, "%y%m%d"),
    "_chirps_gefs_zonal.csv"
  )
  temp_csv_file <- file.path(tempdir(), temp_csv_name)

  if (write_outputs) {
    write_csv(roi_means,
      file = temp_csv_file
    )
    drive_upload(
      media = temp_csv_file,
      path = as_id(zonal_drive$id),
      temp_csv_name
    )
  }
  ret <- list()
  ret$zonal_means <- roi_means # main output for analysis
  ret$rast_stack <- r_stack # could be useful for viz
  return(ret)
}
