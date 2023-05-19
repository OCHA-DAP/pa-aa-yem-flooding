load_chirps_gefs_cropped <- function(run_date=Sys.Date(),
                             leadtime=1:10,
                             mask=roi,
                             write_outputs=T
                             ){
    
    drive_contents <- drive_ls(
        corpus="user"
    ) 
    r_dir <- drive_contents %>% 
        filter(name=="chirps_gefs_rasters")
    zonal_stats_dir <- drive_contents %>% 
        filter(name=="chirps_gefs_zonal")
    
    base_url <- "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/"
    forecast_dir_url <- format(run_date,"%Y/%m/%d")
    
    url_dir <- paste0(base_url,forecast_dir_url) 
    
    leadtime_adj <- leadtime-1
    forecasted_dates <- run_date+leadtime_adj
    
    lyr_names <- paste0(format(run_date,"%Y-%m-%d"),".",leadtime)
    gdrive_file_name <- paste0("yem_aoi_chirps_gefs_",lyr_names,".tif")
    
    
    url_file_names <- paste0("data.",format(forecasted_dates,"%Y.%m%d"),'.tif')
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
                             path=as_id(r_dir$id),
                             name= fname)
            }
                
            )
    }
        
    
    
    
    # cat("stacking raster collection \n")
    # r_stack <- rast(r_cropped_list)
    # 
    # roi_means <- exact_extract(
    #     x = r_stack,
    #     y = mask,
    #     append_cols = "governorate_name",
    #     fun = c("mean"),
    #     force_df = T,
    #     full_colnames = T
    # ) %>% #wrangle
    #     pivot_longer(-matches("governorate_name")) %>%
    #     separate(name, into = c("stat", "date","leadtime"), sep = "\\.") %>%
    #     pivot_wider(names_from = "stat", values_from = "value") %>% 
    #     mutate(
    #         date= as_date(date),
    #         leadtime= as.integer(leadtime),
    #         mean = as.numeric(mean)
    #     )
    # 
    # temp_csv_name <- paste0(format(run_date,"%y%m%d"),
    #        "_chirps_gefs_zonal.csv")
    # temp_csv_file <- file.path(tempdir(),temp_csv_name)
    # 
    # if(write_outputs){
    #     write_csv(roi_means,
    #               file = temp_csv_file)
    #     drive_upload(media= temp_csv_file,
    #                  path = as_id(zonal_stats_dir$id),
    #                  temp_csv_name)
    #               
    # }
    # return(roi_means)
}
