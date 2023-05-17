
#' Title
#'
#' @param run_date 
#' @param n_days 
#'
#' @return
#' @export
#'
#' @examples
#' @import lubridate
chirps_gefs_urls <- function(run_date=Sys.Date(),n_days=10){
    
    # add stopifnot/asserthat
    
    
    base_url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/"
    forecast_dir <- format(run_date,"%Y/%m/%d")
    daily_url <- paste0(base_url,forecast_dir)
    
    
    start_date <- run_date
    end_date <- start_date+n_days
    forecasted_dates <- seq(start_date,end_date,by="day")
    
    file_names <- paste0("data.",format(forecasted_dates,"%Y.%m%d"),'.tif')
    paste0(daily_url,"/",file_names)
    
    
    
}

set_chirps_gefs_download_path <-  function(run_date=Sys.Date()){
    
}
#' Title
#'
#' @param run_date 
#' @param n_days 
#' @param mask 
#' @param ouput_dir 
#'
#' @return
#' @export
#' @import terra
#' @examples
#' library(tidyverse)
#' library(terra)
#' library(readr)
#' library(sf)
#' library(exactextractr)
#' library(lubridate)
#' roi_fp <- file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","live_monitoring","inputs", "high_risk_hulls.rds")
#' roi <- read_rds(roi_fp)
#' load_chirps_gefs_cropped(run_date = Sys.Date()-1,leadtime=1:2, mask=roi,write_outputs = T)



load_chirps_gefs_cropped <- function(run_date=Sys.Date(),
                             leadtime=1:10,
                             mask=roi,
                             write_outputs=T
                             ){
    
    
    monitoring_drive_id <- Sys.getenv("AA_YEM_LIVE_MONITORING_DIR_ID")
    gdrive_id_day <- cascade_ymd_directories(run_date = run_date,gdrive_id = monitoring_drive_id)
    # need to adjust all gdrive handling
    # gdrive_dir <- file.path(Sys.getenv("AA_DATA_DIR"),
    #           "public",
    #           "processed",
    #           "yem",
    #           "live_monitoring"
    #           )
    
    base_url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/"
    forecast_dir_url <- format(run_date,"%Y/%m/%d")
    
    # create folder for files if it does not exist
    # gdrive_raster_outdir <- file.path(gdrive_dir,"inputs","chirps_gefs",forecast_dir_url)
    # gdrive_processed_outdir <- file.path(gdrive_dir,"outputs","chirps_gefs")
    # 
    # if(!dir.exists(gdrive_raster_outdir)){
    #     dir.create(gdrive_raster_outdir,recursive = T)
    # }
    
    url_dir <- paste0(base_url,forecast_dir_url) 
    
    leadtime_adj <- leadtime-1
    forecasted_dates <- run_date+leadtime_adj
    
    
    lyr_names <- paste0(format(run_date,"%Y-%m-%d"),".",leadtime)
    gdrive_file_name <-   paste0(format(forecasted_dates,"%Y.%m%d"),'.tif')
    # gdrive_file_path <-  file.path(gdrive_raster_outdir, paste0(format(forecasted_dates,"%Y.%m%d"),'.tif'))
    
    url_file_names <- paste0("data.",gdrive_file_name)
    url_downloads <- paste0(url_dir,"/",url_file_names)
    
    r_cropped_list <- url_downloads %>% 
        map2(lyr_names,\(url,lyr_name){
            cat("downloading ",lyr_name," to memory\n")
            r <- rast(url)
            cat("cropping ", lyr_name," to mask\n")
            r_cropped <- crop(x=r,y=mask)
            terra::set.names(r_cropped,lyr_name)
            return(r_cropped)
        }
        )
    
    #### okay here is where i need to input fle
    temp_r_paths <- file.path(tempdir(),gdrive_file_name)
    if(write_outputs){
        r_cropped_list %>% 
            map2(temp_r_paths,\(r,fname){
                cat("TEMP writing ",fname,"\n")
                writeRaster(x =r,filename =fname, overwrite=T )
            })
        temp_r_paths %>% 
            map2(gdrive_file_name,\(temp_path,fname){
                cat("Uploading ",fname, " to GDRIVE")
                drive_upload(media=temp_path,
                             path=as_id(gdrive_id_day$id),
                             name= fname)
            }
                
            )
    }
        
    
    
    
    cat("stacking raster collection \n")
    r_stack <- rast(r_cropped_list)
    
    roi_means <- exact_extract(
        x = r_stack,
        y = mask,
        append_cols = "governorate_name",
        fun = c("mean"),
        force_df = T,
        full_colnames = T
    ) %>% #wrangle
        pivot_longer(-matches("governorate_name")) %>%
        separate(name, into = c("stat", "date","leadtime"), sep = "\\.") %>%
        pivot_wider(names_from = "stat", values_from = "value") %>% 
        mutate(
            date= as_date(date),
            leadtime= as.integer(leadtime),
            mean = as.numeric(mean)
        )
    
    temp_csv_name <- paste0(format(run_date,"%y%m%d"),
           "_chirps_gefs_zonal.csv")
    temp_csv_file <- file.path(tempdir(),temp_csv_name)
    
    if(write_outputs){
        write_csv(roi_means,
                  file = temp_csv_file)
        drive_upload(media= roi_means,path = as_id(get_gdrive_shared_dir(gdrive_id = monitoring_drive_id,which="outputs")$id),temp_csv_name)
                  
    }
    return(roi_means)
}
