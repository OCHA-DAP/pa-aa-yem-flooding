



#' ecmwf_mars_historical_zonal_stats
#'
#' @param raster_dir directory containing raster files
#' @param zonal_boundary sf class zonal polygon for zonal stat
#'
#' @return data.frame containing ECMWF MARS data extracted to zonal boundary by weighted mean and median


ecmwf_mars_historical_zonal_stats <-  function(raster_dir=dp2,
                                               zonal_boundary=high_risk_hulls){
    full_fps <- list.files(raster_dir, full.names = T,pattern = "\\.nc$",recursive = F)
    cat("reading rater files as sds\n")
    r_sds <- terra::sds(full_fps)
    cat("converter sds to rast\n")
    r<- terra::rast(r_sds)
    cat("running zonal extractions\n")
    zonal_stat <- exact_extract(
        x = r,
        y = zonal_boundary,
        append_cols = "governorate_name",
        fun = c("mean", "median"),
        force_df = T,
        full_colnames = T
    ) %>%
        pivot_longer(-matches("governorate_name")) %>%
        separate(name, into = c("stat", "date"), sep = "\\.") %>%
        pivot_wider(names_from = "stat", values_from = "value")
    return(zonal_stat)
    
}
