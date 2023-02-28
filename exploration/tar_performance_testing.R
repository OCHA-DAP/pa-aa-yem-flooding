# performance testing 

include_lgl <- function(x, thresh=20){
    x_gte <- x >= thresh
    x_gte_roll=rollsum(x = x_gte,k = 2,align = 'right',fill = NA)
    x_incl = ifelse(x_gte & x_gte_roll>1,NA,x)
    return(x_incl)
}
rainfall_event_long_split$precip_roll10 %>% 
    group_by(site_id) %>% 
    arrange(site_id, date) %>% 
    mutate(
        value =include_lgl(x = value,thresh = 20)
    ) %>% 
    filter(!is.na(value))


threshold_test <- function(site_rainfall=cccm_site_chirp_stats , site_flooding=cccm_flood_impact_data){
    site_report_date_range <-  site_flooding$date_of_episode %>% range(na.rm=T)
    rainfall_time_constrained <- site_rainfall %>%
        mutate(date= ymd(date)) %>% 
        filter(date>=site_report_date_range[1],
               date<=site_report_date_range[2]) %>% 
        mutate(
            fevent= paste0(site_id,date)%in% paste0(cccm_flood_impact_data$site_id,
                                                    cccm_flood_impact_data$date_of_episode)
        )
    
    rainfall_event_long <- rainfall_time_constrained %>% 
        pivot_longer(starts_with("precip_"))
    rainfall_event_long_split <-  split(rainfall_event_long,rainfall_event_long$name)

    
    classification_tbl <- rainfall_event_long_split %>% 
        map2(names(rainfall_event_long_split),
            \(window_df,nm){
                cat(crayon::green(nm),"\n")
             c(1:300) %>% 
                    map_dfr(
                        \(thresh){
                            df_filt <- window_df %>% 
                                mutate(value=include_lgl(value,thresh = thresh)) %>% 
                                filter(!is.na(value))
                            
                            above_thresh <- df_filt %>% 
                                filter(value>=thresh)
                            
                            below_thresh <- df_filt %>% 
                                filter(value<thresh) 
                            
                            TP_num <-  above_thresh %>% 
                                filter(fevent) %>% 
                                nrow()
                            FP_num <- above_thresh %>% 
                                filter(!fevent) %>% 
                                nrow()
                            TN_num <- below_thresh %>% 
                                filter(!fevent) %>% 
                                nrow()
                            FN_num <- below_thresh %>% 
                                filter(fevent) %>% 
                                nrow()
                            
                                tibble(
                                thresh,
                                precip_type=nm,
                                TP =TP_num,
                                FP= FP_num,
                                TN= TN_num,
                                FN= FN_num,
                                accuracy = (TP+TN)/(TP+FP+TN+FN),
                                precision = TP/(TP+FP),
                                recall = TP/(TP+FN)
                                
                            )
                        }
                    )
                
            }
        )
    classification_tbl$precip_roll10 %>% 
        ggplot(aes(x= thresh, y = precision))+
        geom_line()+
        scale_y_continuous(labels = scales::percent)
    
    classification_tbl$precip_roll10 %>% 
        ggplot(aes(x= thresh, y = accuracy))+
        geom_line()+
        scale_y_continuous(labels = scales::percent)
    
    classification_tbl$precip_roll10 %>% 
        ggplot(aes(x= thresh, y = recall))+
        geom_line()+
        scale_y_continuous(labels = scales::percent)
    
    
    
}
tar_load_everything()
cccm_flood_impact_data$date_of_episode
cccm_site_chirp_stats %>% 
    filter(date>=)

cccm_site_chirp_stats %>% 
    filter(
        
    ) %>% 
    pivot_longer(cols = contains("roll"), names_to ="period",values_to = "precip") %>% 
    mutate(
        fevent = paste0(site_id,date)%in% paste0(cccm_flood_impact_data$site_id,
                                                 cccm_flood_impact_data$date_of_episode),
        date= ymd(date)
        
        
    )#%>% 
