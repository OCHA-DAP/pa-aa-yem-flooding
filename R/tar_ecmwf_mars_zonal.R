

# caffeinate -i -s Rscript -e 'targets::tar_make()' 
# caffeinate -s targetstar_make()

#' ecmwf_mars_historical_zonal_stats
#'
#' @param raster_dir directory containing raster files
#' @param zonal_boundary sf class zonal polygon for zonal stat
#'
#' @return data.frame containing ECMWF MARS data extracted to zonal boundary by weighted mean and median


ecmwf_mars_historical_zonal_stats <-  function(raster_dir=ecmwf_mars_dir,
                                               zonal_boundary=high_risk_hulls){
    full_fps <- list.files(raster_dir, full.names = T,pattern = "\\.nc$|\\.grib2$",recursive = F)

    cat("reading rater files as sds\n")
    r_sds <- terra::sds(full_fps)
    cat("converter sds to rast\n")
    r<- terra::rast(r_sds)
    
    # we changed location of local raster and the format changed
    # with change of format, they read in differently
    # there is probably a smarter way to rename, but I just want it 
    # to match the exact format used previously to not interfere with all 
    # downstream targets
    
    if(all(str_detect(full_fps,"\\.grib2$"))){
        r_naming_table <- tibble(
            df=as_date(stringr::str_remove_all(names(r),"^[^0-9]+|_SFC.*$")),
            dp= as_date(time(r)),
            lt_hr = as.integer(dp-df)*24,
            name_new = paste0("yem_fc_tp_",df,"_tp_step=",lt_hr)
            )
        
        terra::set.names(r,r_naming_table$name_new)
        
        # technically i think we should be using Yemen NGN96 / UTM zone 38N - EPSG:2089 for
        # maximum accuracy.... but can play with that later.
        r_proj<- terra::project(x= r,y="epsg:4326")
        
        zonal_stat <- exact_extract(
            x = r_proj,
            y = zonal_boundary,
            append_cols = "governorate_name",
            fun = c("mean", "median"),
            force_df = T,
            full_colnames = T
        ) %>%
            pivot_longer(-matches("governorate_name")) %>%
            separate(name, into = c("stat", "date"), sep = "\\.") %>%
            pivot_wider(names_from = "stat", values_from = "value")
        
    }
    if(all(str_detect(full_fps,"\\.nc$"))){
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
    }
    return(zonal_stat)
}

#' split_hres_by_leadtimes
#'
#' @param r 
#' @param lead_times 
#'
#' @return
#' @export
#'
#' @examples
#' 
split_hres_by_leadtimes <-  function(r, lead_times= c(1:10)){
    r_split_leadtimes <- lead_times %>% 
        map(\(days){
            tstep_hours <- (days*24+24)-24
            t_step_rgx <- glue("={tstep_hours}$")
            t_step_idx <- which(str_detect(names(r),t_step_rgx))
            r[[t_step_idx]]
        }) %>% 
        set_names(
            glue("{lead_times} days")
        )
    return(r_split_leadtimes)
}


 

#' ecmwf_hres_rolling_zonal
#'
#' @param raster_dir 
#' @param zonal_boundary 
#' @param leadtimes 
#' @param roll_windows 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' ecmwf_hres_rolling_zonal(raster_dir = ecmwf_mars_dir,
#'                          zonal_boundary = high_risk_hulls,
#'                          roll_windows=c(3,5,10,20,25,30),
#'                          lead_times=c(1:10)
#'                          )
#'}

ecmwf_hres_rolling_zonal <-  function(raster_dir,
                                      zonal_boundary,
                                      lead_times= c(1:10),
                                      roll_windows=c(3,5,10,20,25,30)){
    full_fps <- list.files(raster_dir, full.names = T,pattern = "\\.nc$",recursive = F)
    cat("reading rater files as sds\n")
    r_sds <- terra::sds(full_fps)
    cat("converter sds to rast\n")
    r<- terra::rast(r_sds)
    cat("splitting raster by leadtimes \n")
    r_split_leadtime <- split_hres_by_leadtimes(r = r,lead_times = lead_times)
    cat("rolling pixel level calcs and zonal stats \n")
    r_split_leadtime %>% 
        # for each leadtime iterate through rollwindows
        map(\(r_temp){
            roll_dfs<- roll_windows %>% 
                map(\(day_window){
                    roll_r <- terra::roll(r_temp, n = day_window, fun = sum, type = "to")
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
            
            # after running through rolling stats also just do a zonal stat on daily mean
            roll_dfs$precip_daly <-  exact_extract(
                x = r_temp ,
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
            roll_stats_by_leadtime_list <- purrr::reduce(roll_dfs ,
                                                         left_join, 
                                                         by = c("governorate_name","date")) 
            
            
            return(roll_stats_by_leadtime_list) 
        })
    
    
}
