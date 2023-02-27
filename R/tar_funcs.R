# these are utility functions to facilitate targets pipeline


load_local_chirps_stack <- function(fps, names){
    r_stack <- terra:::rast(raster::stack(fps)) 
    terra::set.names(x = r_stack, names)
    r_stack[r_stack==-9999] <- NA  
    return(wrap(r_stack))
}

roll_wrapped <- function(x){
    x_unwrapped <- unwrap(x)
    x_roll <- roll(x_unwrapped,
         n=10,
         fun=sum,
         type="to")
    return(wrap(x_roll))
    
}

chirps_daily_to_sites <- function(raster_dir,pt){
    full_fps <- list.files(raster_dir,full.names = T)
    rast_names <- list.files(raster_dir)
    
    rast_dates <- str_extract(string = rast_names,
                              pattern = "(?<=yem_chirps_daily_).*?(?=_r0)"
    ) %>% 
        str_replace_all("_","-")
    
    cat("loading chirps local data sets")
    chirps_daily_full <- terra:::rast(raster::stack(full_fps))
    terra::set.names(x = chirps_daily_full,rast_dates)
    
    cat("replacing -9999 with NA")
    chirps_daily_full[chirps_daily_full==-9999] <- NA   
    chirps_pt <- terra::extract(x = chirps_daily_full,y=pt)
    
    cbind(site_id= pt$site_id,chirps_pt) %>% 
        select(-ID) %>% 
        pivot_longer(
            -site_id,
            names_to="date",
            values_to = "precip_daily"
        )
    
}


chirps_rollsum_to_sites <- function(raster_dir, pt,roll_window){
    full_fps <- list.files(raster_dir,full.names = T)
    rast_names <- list.files(raster_dir)
    
    rast_dates <- str_extract(string = rast_names,
                              pattern = "(?<=yem_chirps_daily_).*?(?=_r0)"
                              ) %>% 
        str_replace_all("_","-")
    
    cat("loading chirps local data sets")
    chirps_daily_full <- terra:::rast(raster::stack(full_fps))
    terra::set.names(x = chirps_daily_full,rast_dates)
    
    cat("replacing -9999 with NA")
    chirps_daily_full[chirps_daily_full==-9999] <- NA   
    
    ret <- list()
    for(i in seq_along(roll_window)){
        window_temp <- roll_window[i]
        cat("running window ",window_temp," days")
        chirps_roll <- terra::roll(chirps_daily_full,
                                     n=window_temp,
                                     fun=sum,
                                     type="to")    
        roll_df <- terra::extract(x = chirps_roll,y=pt)
        gc(chirps_roll)
        
        ret[[paste0("precip_roll",window_temp)]] <- cbind(site_id= pt$site_id,roll_df) %>% 
            select(-ID) %>% 
            pivot_longer(
                -site_id,
                names_to="date",
                values_to = paste0("precip_roll",window_temp)
                )
    }
    return(
        reduce(ret,left_join,by="site_id")
    )
}



load_cccm_wb <-  function(path){

    sheet_names <- excel_sheets(path)
    ss_skip_vals <- c(1,rep(0,3))
    wb <- sheet_names %>% 
        map2(.y = ss_skip_vals,
             ~read_excel(path,sheet=.x,skip = .y) %>% 
                 clean_names()
        ) %>% 
        set_names(sheet_names)
}

get_site_locs <-  function(wb){
    master <- wb[["ML- Flooding Available data" ]]
    flood_reports <- wb[["CCCM FLOOD REPORT IN IDP SITES"]]
    
    master_w_coords <-  master %>%
        rename(
            longitude= "available_coordinates_longitude",
            latitude= "available_coordinates_latitude"
            ) %>% 
        filter(!is.na(longitude),!is.na(latitude))
    
    lat_lon_lookup <- master_w_coords %>% 
        select(site_id,
               longitude,
               latitude,
               site_population)
    
    sites_ids_w_loc_in_master <- flood_reports %>% 
        rename(site_id = "cccm_idp_sites_master_list_site_id") %>% 
        filter(site_id %in% master_w_coords$site_id) %>% 
        distinct(site_id)
        
    
    st_as_sf(
        sites_ids_w_loc_in_master %>%
        left_join(lat_lon_lookup,
                  by="site_id") ,
        coords= c("longitude","latitude"),
        crs= 4326)
         
        
    
}
    
clean_cccm_impact_data <- function(wb,floodsites){
    wb[["CCCM FLOOD REPORT IN IDP SITES"]] %>% 
        rename(
            site_id = "cccm_idp_sites_master_list_site_id",
            site_name= "cccm_idp_sites_master_list_site_name"
            ) %>%
        filter(site_id %in% floodsites$site_id) %>% 
        left_join(floodsites %>% 
                      select(site_id,site_population)) %>% 
            rename(
                num_init_hhs ="initial_figures_h_hs",
                num_verified_hhs = "verified_figures_h_hs",
                num_shelters_affected= "how_many_shelters_were_affected",
                num_hhs_lost_documentation="if_yes_how_many_h_hs_did_they_lose_their_personal_legal_documentations_due_to_the_flooding",
            ) %>% 
            mutate(
                #https://fscluster.org/sites/default/files/documents/operational_guidance_note_-_minimum_expenditure_basket_september_2022.pdf  
                pop_affected= num_shelters_affected * 7, 
                pct_pop_affected=  pop_affected/site_population,
                ratio_shelters_to_pct_pop = num_shelters_affected/(10-pct_pop_affected)
            )
}


find_priority_sites <- function(wb,floodlist,n){
    
    top_sites <- list()
    wb$`ML- Flooding Available data`$site_population
    site_level_stats <- wb$`CCCM FLOOD REPORT IN IDP SITES` %>%
        rename(
            site_id = cccm_idp_sites_master_list_site_id
            ) %>% 
        filter(site_id %in% floodlist$site_id) %>% 
        left_join(
            floodlist %>%
                st_drop_geometry()
            , by = "site_id"
        ) %>% 
        rename(
            num_init_hhs =initial_figures_h_hs,
            num_verified_hhs = verified_figures_h_hs,
            num_shelters_affected= how_many_shelters_were_affected,
            num_hhs_lost_documentation=if_yes_how_many_h_hs_did_they_lose_their_personal_legal_documentations_due_to_the_flooding,
            site_name= cccm_idp_sites_master_list_site_name
        ) %>% 
        mutate(
            #https://fscluster.org/sites/default/files/documents/operational_guidance_note_-_minimum_expenditure_basket_september_2022.pdf  
            pop_affected= num_shelters_affected * 7, 
            pct_pop_affected=  pop_affected/site_population,
            ratio_shelters_to_pct_pop = num_shelters_affected/(10-pct_pop_affected)
        ) %>% 
        group_by(governorate_name,district_name, sub_district_name,site_name ,site_id) %>% 
        summarise(
            num_flood_reports = n(),
            max_affected_storm = max(num_shelters_affected),
            across(starts_with("num_"),~sum(.x,na.rm=T)),
            site_population = unique(site_population),
            mean_pct_pop_affected = mean(pct_pop_affected,na.rm=T),
            avg_ratio_shelters_to_pct_pop = mean(ratio_shelters_to_pct_pop,na.rm=T),
            max_pct_pop_affected = max(pct_pop_affected),
            .groups = "drop"
        ) 
    top_sites[["by_num_reports"]] <- site_level_stats %>% 
        arrange(desc(num_flood_reports)) %>% 
        slice(1:n)
    top_sites[["by_affected_shelters"]] <- site_level_stats %>% 
        arrange(desc(num_shelters_affected)) %>% 
        slice(1:n)
    
    top_sites[["by_pct_pop_affected"]] <- site_level_stats %>% 
        arrange(desc(mean_pct_pop_affected)) %>% 
        slice(1:n)
    
    top_sites[["by_shelters_to_pct_ratio"]] <- site_level_stats %>% 
        arrange(desc(avg_ratio_shelters_to_pct_pop)) %>% 
        slice(1:n)
    return(top_sites)
    
}
