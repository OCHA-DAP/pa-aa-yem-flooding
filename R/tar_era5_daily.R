
#' load_era_daily
#' @description
#'  convenience function to load era5 daily aggregates from 1981-2022 and 
#'  run zonal mean on each image for a the given poly
#' 
#' @param poly polygon for zonal stat 
#'
#' @return era5 precip data aggregated to polygon boundaries via weighted mean.

load_era_daily <- function(poly,ds=NULL){
    if(is.null(ds)){
        era <- ee$ImageCollection("ECMWF/ERA5/DAILY")    
        era_filt <- era$filterDate("1981-01-01","2023-01-01")$select("total_precipitation")
        scale_val <- 27830 
    }
    if(!is.null(ds)){
        era <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_RAW")
        era_filt <- era$filterDate("1981-01-01","2023-01-01")$select("total_precipitation_sum")
        scale_val = 11132
    }
    
    era_daily_zonal <- tidyrgee::ee_extract_tidy(x = era_filt,
                                                 y =poly ,
                                                 stat = "mean",
                                                 via="drive",
                                                 scale = scale_val)
    return(era_daily_zonal)
}




#' subset local era5 hourly raster collections to just daily cumualtive values
#' @description 
#' era5 data downloaded in cumulative hourly format where each grib file holds a month worth of 
#' hourly cumulative records. The records are cumulative daily where 00:00:00 represents the precip for the
#' entire previous 24 hours
#' @param r raster
#'
#' @return SpatRaster class 
#' @examples \dontrun{
#' era5_dir <-  file.path(Sys.getenv("AA_DATA_DIR"),"public", "raw","yem","ecmwf")
#' era5_fp <- file.path(era5_dir, "yem_era5_tp_2001-09.grib2")
#' # load month of data
#' era5_hourly <- terra::rast(era5_fp)
#' era5_daily <- get_local_era5_daily_precip(era5_hourly)
#' era5_hourly
#' }

subset_era5_hourly_to_daily <- function(r){
    r_time <- terra::time(r)
    r_00_idx <- which(str_detect(r_time,"00:00:00"))
    r_subset <- r[[r_00_idx]]
    r_subset_time <-  terra::time(r_subset)
    r_subset_time_shifted <- as_date(r_subset_time)-1
    terra::set.names(r_subset,r_subset_time_shifted)
    return(r_subset)
}


#' extract_local_era5_daily
#' @description
#' We need to extract 00:00:00 rasters from each day within each file (monthly) in the era5 directory before running 
#' rolling stats on pixels and then extracting. This returns the daily rasters ready for rolling stats
#' @param raster_dir \code{character} file path to directory containing rasters 
#'
#' @return terra SpatRaster containing daily cumulative rasters from all files in provided `raster_dir`
#' @examples \dontrun{
#' library(tidyverse)
#' library(terra)
#' era5_dir <-  file.path(Sys.getenv("AA_DATA_DIR"),"public", "raw","yem","ecmwf")
#' era5_r <- extract_local_era5_daily(raster_dir = era5_dir)
#' }

subset_all_era5_hourly_to_daily <-  function(raster_dir,pattern = "\\.grib2$"){
    era5_files <- list.files(raster_dir,pattern =pattern,full.names = T)
    era5_rast_list <- era5_files %>% 
        map(\(rpath){
            r <- terra::rast(rpath)
            # set as wgs84
            terra::crs(r) <- "epsg:4326"
            r_daily <- subset_era5_hourly_to_daily(r)
            return(r_daily)
        })
    # convert raster list to one raster
    era5_r <- terra::rast(era5_rast_list)
    return(era5_r)
}


#' ecmwf_era5_rolling_zonal_local
#' @description 
#' We have hourly ERA5 precip data. Each file contains all the hours in a a month. The hours are cumulative per day. Therefore, we:
#' 1. open all data and subset 00:00:00 data layers which represent daily cumulative values
#' 2. define windows to run rolling sum calculations and run the rolling sum on raster pixels for each window
#' 3. run zonal stats on both the daily and rolling rasters
#' 
#' @details 
#' previously using `load_era_daily()` we run the calculations in GEE directly, but instead of performing pixel level rolling calculations
#' we just extracted the zonal mean for each day and ran rolling calculations on that directly. 
#' 
#' @param raster_dir \code{character} file path to directory containing rasters 
#' @param pattern \code{character} regex pattern to identify rasters (default = "\\.grib2$")
#' @param zonal_boundary
#' @param roll_windows \code{integer} windows to using for rolling sum calculation
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' library(tidyverse)
#' library(terra)
#' era5_dir <-  file.path(Sys.getenv("AA_DATA_DIR"),"public", "raw","yem","ecmwf")
#' era5_r <- extract_local_era5_daily(raster_dir = era5_dir)
#' }
ecmwf_era5_rolling_zonal_local <-  function(raster_dir, 
                                            pattern="\\.grib2$",
                                            zonal_boundary,
                                            roll_windows=c(3,5,10,15,20,25,30)){
    cat("extacting rasters\n")
    r <- subset_all_era5_hourly_to_daily(raster_dir = raster_dir, pattern=pattern)
    cat("running rolling stats on rasters")
    roll_dfs<- roll_windows %>% 
        # fun rolling su for each window and then do zonal extractions
        map(\(day_window){
            roll_r <- terra::roll(r, n = day_window, fun = sum, type = "to")
            zonal_stat <- exact_extract(
                x = roll_r ,
                y = zonal_boundary,
                append_cols = "governorate_name",
                fun = c("mean"),
                force_df = T,
                full_colnames = T
            ) %>%
                pivot_longer(-matches("governorate_name")) %>%
                separate(name, into = c("stat", "date"), sep = "\\.") %>%
                pivot_wider(names_from = "stat", values_from = "value") %>% 
                rename(
                    !!sym(paste0("roll",day_window)):="mean"
                ) 
            return(zonal_stat)
        })
    # after zonal extractions on rolling values do one for daily
    roll_dfs$precip_daly <-  exact_extract(
        x = r ,
        y = zonal_boundary,
        append_cols = "governorate_name",
        fun = c("mean"),
        force_df = T,
        full_colnames = T
    ) %>%
        pivot_longer(-matches("governorate_name")) %>%
        separate(name, into = c("stat", "date"), sep = "\\.") %>%
        pivot_wider(names_from = "stat", values_from = "value") %>%
        rename(
            precip_daily = "mean"
        )
    # join extractions into 1 data.frame
    roll_stats_zonal_list <- purrr::reduce(roll_dfs ,
                                                 left_join, 
                                                 by = c("governorate_name","date")) 
    
    
    return(roll_stats_zonal_list) 


}
# 
# 
# 
# 
# 
# era_daily_list <- list()
# for(i in seq_along(bla)){
#     era_temp <- terra::rast(bla[i])
#     terra::crs(era_temp) <- "epsg:4326"
#     era_daily_list[[i]] <- get_local_era5_daily_precip(era_temp)
# }
# 
# 
# ?terra::crs(era_temp) <- 
# raster::merge()
# r_sds <- terra::sds(era_daily_list)
# 
# terra::rast(era_daily_list)
# exact_extract(x = r_sds_r, y= high_risk_hulls)
# 
# exact_extract(x = r_sds, y= high_risk_hulls)
# 
# high_risk_hulls
# terra::merge(era_daily_list)
