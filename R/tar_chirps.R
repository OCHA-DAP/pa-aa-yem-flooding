#' these are utility functions to facilitate targets pipeline

####################################
# Note: ############################
####################################
#' They should not be though of as generic functions, rather individual scripts that take
#' a very specific input and produce a specific outline  to facilitate the work flow
#' sometimes features/arguments are added to the functions to make them more generalizeable if that is seen
#' as beneficial to reaching the goals of this project, however this is secondary to creating useful outputs.
####################################

#' @title chirps_daily_to_sites
#'
#' @param raster_dir \code{character} path AA_DATA_DIR to folder containing daily chirps
#' @param pt spatial data frame in epsg:4326 containing site locations
#'
#' @return site locations with daily chirps values

chirps_daily_to_sites <- function(raster_dir, pt,batch_by_year) {
    full_fps <- list.files(raster_dir, full.names = T)
    rast_names <- list.files(raster_dir)
    rast_dates <- str_extract(
        string = rast_names,
        pattern = "(?<=yem_chirps_daily_).*?(?=_r0)"
    ) %>%
        str_replace_all("_", "-")
    
    if(batch_by_year){
        # I'm going to batch read in the rasters by year to get an understanding
        # of time
        file_tbl <- tibble(
            full_path=full_fps,
            file_name= rast_names,
            rast_dates= rast_dates,
            rast_year = year(ymd(rast_dates))
        )
        file_tbl_split <- split(file_tbl, file_tbl$rast_year)
        
        rstack_list <- file_tbl_split %>%
            map(\(file_group){
                yr_temp <- unique(file_group$rast_year)
                cat("loading  ",yr_temp, "\n")
                rstack <- terra:::rast(raster::stack(file_group$full_path))
                terra::set.names(rstack, file_group$rast_dates)
                
                return(rstack)
                
            })
        sites_extracted_long <- rstack_list %>%
            imap_dfr(\(rstack,nm)
                     {
                         cat("treating null and extracting for year: ",nm,"\n")
                         rstack[rstack == -9999] <- NA
                         pt_extracted<- terra::extract(x = rstack, y = pt)
                         cbind(site_id = pt$site_id, pt_extracted) %>%
                             select(-ID) %>%
                             pivot_longer(
                                 -site_id,
                                 names_to = "date",
                                 values_to = "precip_daily"
                             )
            }
            )
    }
    if(!batch_by_year){
        cat("loading chirps local data sets")
        chirps_daily_full <- terra:::rast(raster::stack(full_fps))
        terra::set.names(x = chirps_daily_full, rast_dates)
        
        cat("replacing -9999 with NA")
        chirps_daily_full[chirps_daily_full == -9999] <- NA
        chirps_pt <- terra::extract(x = chirps_daily_full, y = pt)
        
        sites_extracted_long <- cbind(site_id = pt$site_id, chirps_pt) %>%
            select(-ID) %>%
            pivot_longer(
                -site_id,
                names_to = "date",
                values_to = "precip_daily"
            )
    }
    return(sites_extracted_long)
}





load_chirps_stack <- function(raster_dir, batch_by_year=T){
    full_fps <- list.files(raster_dir, full.names = T)
    rast_names <- list.files(raster_dir)
    
    # rgx to pull out dates of raster to use for layer names
    rast_dates <- str_extract(
        string = rast_names,
        pattern = "(?<=yem_chirps_daily_).*?(?=_r0)"
    ) %>%
        str_replace_all("_", "-")
    
    if(batch_by_year){
        # I'm going to batch read in the rasters by year to get an understanding
        # of time
        file_tbl <- tibble(
            full_path=full_fps,
            file_name= rast_names,
            rast_dates= rast_dates,
            rast_year = year(ymd(rast_dates))
        )
        file_tbl_split <- split(file_tbl, file_tbl$rast_year)
        
        rstack_list <- file_tbl_split %>%
            map(\(file_group){
                yr_temp <- unique(file_group$rast_year)
                cat("loading  ",yr_temp, "\n")
                rstack <- terra:::rast(raster::stack(file_group$full_path))
                terra::set.names(rstack, file_group$rast_dates)
                return(rstack)
            })
    
    }
    if(!batch_by_year){
        cat("loading chirps local data sets")
        rstack <- terra:::rast(raster::stack(full_fps))
        terra::set.names(x = rstack, rast_dates)
        
        cat("replacing -9999 with NA")
        rstack[rstack == -9999] <- NA
    }
    return(rstack)
}
