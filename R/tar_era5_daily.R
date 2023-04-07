
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
    if(ds=="raw"){
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

