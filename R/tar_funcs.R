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


calc_rolling_precip_sites <-  function(df){
     df %>% 
        group_by(site_id) %>%
        arrange(site_id,date) %>% 
        mutate(
            precip_roll3 = rollsum(x= precip_daily,fill=NA, k= 3,align="right"),
            precip_roll5=  rollsum(x= precip_daily,fill=NA, k= 5,align="right"),
            precip_roll10=  rollsum(x= precip_daily,fill=NA, k= 10,align="right"),
            precip_roll30=  rollsum(x= precip_daily,fill=NA, k= 30,align="right"),
            
            precip_roll3_c = rollsum(x= precip_daily,fill=NA, k= 3,align="center"),
            precip_roll5_c =  rollsum(x= precip_daily,fill=NA, k= 5,align="center"),
            precip_roll10_c =  rollsum(x= precip_daily,fill=NA, k= 11,align="center"),
            precip_roll30_c =  rollsum(x= precip_daily,fill=NA, k= 31,align="center"),
        )    %>% 
        ungroup()
}


plot_rainfall_impact_timeseries <- function(site_rainfall,
                                                  flood_report,
                                                  prioritization_list =high_priority_sites,
                                                  prioritize_by="by_affeted_shelters"){
    plot_list <- list()

    high_impact_events <- flood_report %>% 
        filter(
            site_id %in% prioritization_list[[prioritize_by]]$site_id
        ) %>% 
        mutate(
            date= ymd(date_of_episode)
        ) 
    
    cccm_site_chirps_stats_long <- site_rainfall %>% 
        filter(
            # site_id %in% high_priority_sites$by_affected_shelters$site_id,
            # year(date) %in% c(2021,2022)
        ) %>% 
        pivot_longer(cols = contains("roll"), names_to ="period",values_to = "precip") %>% 
        mutate(
            fevent = paste0(site_id,date)%in% paste0(flood_report$site_id,
                                                     flood_report$date_of_episode),
            date= ymd(date)
            
            
        )
    monthly_site_stats <- cccm_site_chirps_stats_long %>% 
        group_by(site_id,mo=month(date),period) %>%
        summarise(
            precip= mean(precip,na.rm=T),
            .groups = "drop"
        )
    
    cccm_site_historical_doy <- cccm_site_chirps_stats_long %>% 
        group_by(site_id,doy=day(date),period) %>%
        summarise(
            precip_mean= mean(precip,na.rm=T),
            precip_med= median(precip,na.rm=T),
            .groups = "drop"
            # median_precip=median(precip,na.rm=T)
        )
    
    chirps_doy_anom <- cccm_site_chirps_stats_long %>% 
        filter(year(date) %in% c(2021,2022)) %>% 
        mutate(
            doy= day(date)
        ) %>% 
        left_join(
            cccm_site_historical_doy ,
            by =c("site_id","period","doy")
        ) %>% 
        mutate(
            precip_anom_mean=precip-precip_mean,
            precip_anom_med=precip-precip_med
        )
    chirps_top10_anom_dat <- chirps_doy_anom %>% 
        filter(
            site_id %in% prioritization_list[[prioritize_by]]$site_id
        ) %>% 
        left_join(high_impact_events %>% distinct(site_id, site_name), by="site_id")
    
    p_top10_shelt_affected_anom_doy_ts <- chirps_top10_anom_dat %>% 
        ggplot(aes(x= date,y=precip_anom_mean,color=period))+
        geom_line()+
        scale_x_date(date_breaks = "month",date_labels = "%b-%y")+
        # scale_y_continuous(breaks = seq(0,400,by=25))+
        geom_vline(data= high_impact_events,aes(xintercept=date, group=site_id),color="black",linetype="dashed")+
        geom_text_repel(data=high_impact_events,aes(x=date,label=num_shelters_affected,y=100),color="black",angle =90)+
        facet_wrap(~site_name)+
        theme_hdx()+
        labs(y="Precipitation anomaly (mm)", 
             title="Precipitation anomaly vs Flood Events",
             subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)",
             caption = "1. Black dashed line mark date of reported flood event. Vertical label indicates # of shelters reported damaged\n2. Colored lines in timeseries represent daily precipitation anomaly for each accumulation window. Anomaly generated by calculating: comparing 2021/2022 rolling windows\nagainst historical averages for each window."
        )+
        theme(
            axis.text.y=element_text(size=7),
            axis.text.x = element_text(angle=90),
            # plot.caption.position = "plot"
            plot.caption = element_text(hjust = 0)
            # legend.position="none"
        )
    
    ### zoom in
    p_zoom_in_historical <- chirps_top10_anom_dat %>% 
        filter(snakecase::to_snake_case(site_name)=="al_sowayda",
               !str_detect(period,"_c$")
        ) %>% 
        select(-precip_anom_mean,-precip_anom_med,-doy) %>% 
        pivot_longer(cols = c("precip_mean","precip_med","precip"),names_to = "precip_type") %>% 
        ggplot(aes(x= date,y=value, color=precip_type,group=precip_type))+
        geom_line()+
        scale_x_date(date_breaks = "month",date_labels = "%b-%y")+
        facet_wrap(~period)+
        theme_hdx()+
        labs(y="Precipitation (mm)", 
             title="Precipitation at different accumulation windows vs historical",
             subtitle = "Yemen: Al Sowayda site",
             caption = ""
        )+
        theme(
            axis.text.y=element_text(size=7),
            axis.text.x = element_text(angle=90),
            # plot.caption.position = "plot"
            plot.caption = element_text(hjust = 0)
            # legend.position="none"
        )
    p_zoom_in_historical_pseudolog <- p_zoom_in_historical+
        scale_y_continuous(trans = scales::pseudo_log_trans())
        
    
    
    
    
    
   
    
    cccm_chirps_monthly_anomaly <- cccm_site_chirps_stats_long %>% 
        filter(year(date) %in% c(2021, 2022)) %>% 
        mutate(mo=month(date)) %>% 
        left_join(monthly_site_stats %>% rename(precip_historical="precip"), by = c("site_id","mo","period")) %>% 
        mutate(precip_anom_mean = precip-precip_historical)
    
    
    p_top10_shelt_affected <- cccm_chirps_monthly_anomaly %>% 
        filter(
            site_id %in% prioritization_list[[prioritize_by]]$site_id
        ) %>% 
        left_join(
            high_impact_events %>% 
                distinct(site_id, site_name),
            by="site_id"
        ) %>% 
        mutate(
            date= ymd(date)
        ) %>% 
        ggplot(aes(x= date,y=precip,color=period))+
        geom_line()+
        scale_x_date(date_breaks = "month",date_labels = "%b-%y")+
        scale_y_continuous(breaks = seq(0,400,by=25))+
        geom_vline(data= high_impact_events,aes(xintercept=date, group=site_id),color="black",linetype="dashed")+
        geom_text_repel(data=high_impact_events,aes(x=date,label=num_shelters_affected,y=100),color="black",angle =90)+
        facet_wrap(~site_name)+
        theme_hdx()+
        labs(y="Precipitation: 10 day rolling sum (mm)", 
             title="Precipitation vs Flood Events",
             subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)")+
        theme(
            axis.text.y=element_text(size=7),
            axis.text.x = element_text(angle=90)
            # legend.position="none"
        )
    
    p_top10_shelt_affected_anom_mo_ts <- cccm_chirps_monthly_anomaly %>% 
        filter( site_id %in%  prioritization_list[[prioritize_by]]$site_id) %>% 
        left_join(high_impact_events %>% distinct(site_id, site_name)) %>% 
        ggplot(aes(x= date,y=precip_anom_mean,color=period))+
        geom_line()+
        scale_x_date(date_breaks = "month",date_labels = "%b-%y")+
        # scale_y_continuous(breaks = seq(0,400,by=25))+
        geom_vline(data= high_impact_events,aes(xintercept=date, group=site_id),color="black",linetype="dashed")+
        geom_text_repel(data=high_impact_events,aes(x=date,label=num_shelters_affected,y=100),color="black",angle =90)+
        facet_wrap(~site_name)+
        theme_hdx()+
        labs(y="Precipitation anomaly (mm)", 
             title="Precipitation anomaly vs Flood Events",
             subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)",
             caption = "1. Black dashed line mark date of reported flood event. Vertical label indicates # of shelters reported damaged\n2. Colored lines in timeseries represent daily precipitation anomaly for each accumulation window. Anomaly generated by calculating: a.) rolling statistics daily, b.) averaging each rolling statistic per month\nthroughout the historical record (1998-2022), c.) subtracting daily rolling statistic for monthly average"
        )+
        theme(
            axis.text.y=element_text(size=7),
            axis.text.x = element_text(angle=90),
            # plot.caption.position = "plot"
            plot.caption = element_text(hjust = 0)
            # legend.position="none"
        )
    
    

    
    p_ts_chirs_vs_rainfall <- cccm_chirps_monthly_anomaly %>% 
        filter(!str_detect(period,"_c$")) %>% 
        mutate(
            site_period = paste0(site_id,period)
        ) %>% 
        ggplot(aes(x= date,y=precip,group=site_id))+
        geom_line(alpha=0.1)+
        geom_vline(data=cccm_site_chirps_stats_long %>% 
                       filter(fevent,
                              !str_detect(period,"_c$")
                       ),
                   aes(xintercept=date),alpha=0.2
                   
        )+
        facet_wrap (~period) +
        scale_x_date(date_breaks = "month",date_labels = "%b-%y")+
        scale_y_continuous(breaks = seq(0,400,by=25))+
        theme_hdx()+
        labs(y="Rolling precip (mm)", 
             title="Precipitation vs Flood Events",
             subtitle = "Yemen: 260 IDP sites with 390 reported flood events")+
        theme(
            axis.text.y=element_text(size=7),
            axis.text.x = element_text(angle=90),
            legend.position="none"
        )
    
    plot_list[["top10_sites_shelter_affected"]] <- p_top10_shelt_affected
    plot_list[["top10_sites_shelter_affected_anom_mo"]] <- p_top10_shelt_affected_anom_mo_ts
    plot_list[["top10_sites_shelter_affected_anom_doy"]] <- p_top10_shelt_affected_anom_doy_ts
    plot_list[["p_zoom_in_historical"]] <- p_zoom_in_historical
    plot_list[["p_zoom_in_historical_pseudolog"]] <- p_zoom_in_historical_pseudolog
    plot_list[["all_sites_chirps"]] <- p_ts_chirs_vs_rainfall
    return(plot_list)
    
}


extract_chirps_gefs_to_pts <- function(raster_dir,forecast,sites=cccm_flood_report_sites){
    forecast_str = as.character(forecast)
    sub_dir <- switch(forecast_str,
           "10"="10days",
           "5" = "05days",
           "15" = "15days")
    raster_dir_full <-  file.path(raster_dir,sub_dir)
    file_names_short <- list.files(raster_dir_full)
    file_names_full <-  list.files(raster_dir_full,full.names = T)
    
    rast_dates <- str_extract_all(string = file_names_short,"\\d+") %>% 
        map(2) %>% 
        unlist() %>% 
        ymd()
    
    # I'm going to batch read in the rasters by year to get an understanding
    # of time
    file_tbl <- tibble(
        file_names_short,
        file_names_full,
        rast_dates,
        rast_year = year(rast_dates)
    )
    file_tbl_split <- split(file_tbl,file_tbl$rast_year)
    
    rstack_list <- file_tbl_split %>% 
        map(~ {
            yr_temp<- unique(.x$rast_year)
            cat(yr_temp,"\n")
            rstack <- terra:::rast(raster::stack(.x$file_names_full))
            terra::set.names(rstack,.x$rast_dates)
            return(rstack)
        }
        )
    
    gef_chirps_sites <- rstack_list %>% 
        map_dfr(
           ~{
               gefs_sites <- terra::extract(x = .x,y=sites)
               cbind(site_id= sites$site_id,gefs_sites) %>% 
                   select(-ID) %>% 
                   pivot_longer(
                       -site_id,
                       names_to="date",
                       values_to = paste0("gefs_chirps_",forecast_str)
                   )
           }
        )
    chull<- st_convex_hull(sites %>% summarise())
    set.seed(700)
    rnd_sample<- st_sample(chull, size= 1000) %>% 
        st_sf() %>% 
        mutate(samp_id = row_number())
    
    gef_chirps_sample <- rstack_list %>% 
        map_dfr(
            ~{
                gefs_sample <- terra::extract(x = .x,y=rnd_sample)
                cbind(site_id= rnd_sample$samp_id,st_coordinates(rnd_sample),gefs_sample) %>% 
                    select(-ID) %>% 
                    pivot_longer(
                        -c(site_id,X,Y),
                        names_to="date",
                        values_to = paste0("gefs_chirps_",forecast_str)
                    )
            }
        )
    ret <- list()
    ret$chirps_gefs_sites <-gef_chirps_sites
    ret$chirps_gefs_sample <-gef_chirps_sample
    
    return(ret)
    
}




plot_chirps_gefs_comparison <- function(gef_values, chirps_values,gef_forecast_window=10){
    ret <-  list()
    
    day_adj_gefs <- gef_forecast_window-1
    rainfall_and_forecast_site <- gef_values$chirps_gefs_sites %>% 
        mutate(
            date =ymd(date),
            date = date+ day_adj_gefs
        ) %>% 
        left_join(
            chirps_values %>% 
                mutate(date= ymd(date)) ,
            by = c("site_id","date")
        )
    
    p_gefs_chirps_site_avg <- rainfall_and_forecast_site %>% 
        group_by(date) %>% 
        summarise(
            mean_gefs10 = mean(gefs_chirps_10,na.rm=T),
            mean_chirps10= mean(precip_roll10,na.rm=T)
        ) %>% 
        ggplot(aes(x= mean_gefs10,y=mean_chirps10))+
        geom_point(alpha= 0.1,color="#1EBFB3")+
        labs(
            x= "CHIRPS-GEFS 10-day accumulation Forecast (mm)",
            y= "CHIRPS 10 day accumulation (mm)",
            title= "CHIRPS-GEFS Forecast compared to CHIRPS Historical",
            subtitle = "Yemen - daily averages of CHIRPS & CHIRPS GEFS calculated across 260 CCCM sites",
            caption= "Dates in data sets aligned by the following manipulation:\n a.) CHIRPS 10-day rolling sum ('right aligned'), b.) CHIRPS-GEFS - added 9 days to raster date"
        )+
        geom_smooth(method='lm', formula= y~x)+
        theme_hdx()+
        theme(
            plot.caption=element_text(hjust=0)
        )
    
    p_gef_chirps_rnd_site <-  rainfall_and_forecast_site %>% 
        filter(site_id=="YE2002_1161") %>% 
        ggplot(aes(x= gefs_chirps_10,y=precip_roll10))+
        geom_point(alpha= 0.2,color="#1EBFB3")+
        labs(
            x= "CHIRPS-GEFS 10-day accumulation Forecast (mm)",
            y= "CHIRPS 10 day accumulation (mm)",
            title= "CHIRPS-GEFS Forecast compared to CHIRPS Historical",
            subtitle = "Yemen CCCM Site YE2002_1161: 2000-2022",
            caption= "Dates in data sets aligned by the following manipulation:\n
        a.) CHIRPS 10-day rolling sum ('right aligned')\nb.) CHIRPS-GEFS - added 9 days to raster date\n"
        )+
        geom_smooth(method='lm', formula= y~x)+
        theme_hdx()+
        theme(
            plot.caption=element_text(hjust=0)
        )
    
    ret$averaged_across_sites <- p_gefs_chirps_site_avg
    ret$rnd_site <- p_gef_chirps_rnd_site
    return(ret)
}



