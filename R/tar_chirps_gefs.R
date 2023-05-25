
zonal_stats_chirps_gefs <-  function(raster_dir,zonal_boundary){
    full_fps<- list.files(file.path(raster_dir,full.names = T,pattern = "\\.tif$"))
    fname<- basename(full_fps)
    fdates <- as_date(str_extract(string = fname,pattern =  "\\d{4}-\\d{2}-\\d{2}"))
    lt <-  str_extract(fname,pattern="(?<=\\D)(\\d{2})(?=\\D*$)")
    file_catalogue <- tibble(
        name_short = fname,
        name_long = full_fps,
        date = fdates,
        year= year(date),
        lt = lt
    )
    
    gefs_zone <- file_catalogue %>% 
        filter(year %in% c(2010,2011)) %>% 
        split(.$year) %>% 
        imap_dfr(\(yr_meta,nm){
            cat("ingesting ",nm," rasters\n")
            r_stack_temp <- rast(yr_meta$name_long)
            
            assertthat::assert_that(st_crs(r_stack_temp)==st_crs(zonal_boundary),
                                    msg = "different projections for raster and zonal boundary"
            )
            
            cat("extracting ",nm," rasters\n")
            exact_extract(
                x = r_stack_temp ,
                y = zonal_boundary,
                append_cols = "governorate_name",
                fun = c("mean"),
                force_df = T,
                full_colnames = T
            ) %>%
                pivot_longer(-matches("governorate_name")) %>%
                separate(name, into = c("stat", "date"), sep = "\\.") %>%
                pivot_wider(names_from = "stat", values_from = "value") 
            
        }
        )
    return(gefs_zone)
    
}



