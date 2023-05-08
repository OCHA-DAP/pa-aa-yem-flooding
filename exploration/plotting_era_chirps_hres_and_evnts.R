library(targets)
library(tidyverse)
library(lubridate)
library(extRemes)
library(gghdx)
library(sysfonts)
font_add_google("Source Sans Pro")
showtext::showtext_auto()

tar_source()
tar_load(rainfall_impact_tbl)
tar_load(ecmwf_mars_clean)
tar_load(gov_area_rainfall_impact_tbl)
tar_load(hres_rolled_per_date_gen)
tar_load(zonal_stats_high_risk_hull)
tar_load(era5_w_rolling)

events_by_date <- gov_area_rainfall_impact_tbl %>% 
    filter(governorate_name %in% c("Hajjah","Marib")) %>% 
    select(date, governorate_name,fevent)


hres_roll_lead5<- hres_rolled_per_date_gen %>% 
    filter(name %in% c("roll3")) %>% 
    mutate(value = if_else(value<1e-3,0,value)) %>% 
    arrange(governorate_name) %>% 
    filter(leadtime%in%c(3,4,5)) 
    


tar_load(ecmwf_mars_high_risk_hulls)
ecmwf_mars_clean

ecmwf_mars_clean %>%
    select(-median,
           governorate_name, 
           date_forecasted,
           leadtime,mean) %>%
    group_by(governorate_name,date_forecast_made) %>% 
    arrange(date_forecasted) %>%
    mutate(
        roll3=  rollsum(mean, fill= NA, k=3, align ="right"),
        roll5=  rollsum(mean, fill= NA, k=5, align ="right"),
        roll10= rollsum(mean, fill= NA, k=10, align ="right")
    ) %>% 
    ungroup() %>% 
    pivot_longer(cols = c("roll3","roll5","roll10",mean)) %>% 
    mutate(
        name = str_replace(name, "mean","precip_daily"),
    ) %>% 
    rename(date_forecast_predict = "date_forecasted")
)

hres_roll_lead5 %>% 
    arrange(governorate_name,date_forecast_made,date_forecast_predict) %>% 
    group_by(governorate_name,date_forecast_made, date_forecast_predict) %>% 
    summarise(value=max(value))

events_w_forecast <- events_by_date %>% 
    left_join(hres_roll_lead5,
              by=c("governorate_name","date"="date_forecast_predict"))

rainfall_impact_tbl$date %>% range()

rainfall_impact_tbl %>% 
    filter(governorate_name=="Hajjah") %>% 
    pull(date) %>% range()

cccm_flood_impact_data_w_coords %>% 
    filter(governorate_name=="Hajjah") %>% 
    pull(date_of_episode) %>% range()

library(sf)
tar_load(cccm_flood_impact_data_w_coords)

cccm_flood_impact_data_w_coords$date_of_episode %>% range()

events_w_forecast %>% 
    ggplot(aes(x=date, y= value))+
    geom_line()+
    geom_point(data=events_w_forecast %>% filter(fevent))+
    facet_wrap(~governorate_name)


tar_load(zonal_stats_high_risk_hull)
era5_w_rolling

# this has alot of what i want .... but i want to make it easier to read -- also remove wind
plot_site_events_classified(
    df = events_w_forecast %>%
        filter(governorate_name == "Hajjah") %>%
        arrange(date),
    plot_title = "Hajjah",
    x = "value",
    event = "fevent",
    thresh = 19.8, 
    day_window = 60
)+
    geom_line(data=zonal_stats_high_risk_hull$roll3 %>% 
                  rename(value =mean) %>% 
                  mutate(date=as_date(date)) %>% 
                  filter(governorate_name =="Hajjah",
                         date %in% seq(as_date("2022-03-01"),as_date("2022-09-30"),1)
                         ), color="blue")+
    
    geom_line(data= era5_w_rolling %>% 
                  rename(value = era_roll3) %>% 
                  filter(governorate_name =="Hajjah",
                         date %in% seq(as_date("2022-03-01"),as_date("2022-09-30"),1)
                  ), color="orange")+
              
    labs(y= "Forecasted 3 day accumulation (5 day lead)")




# let's set up a data.frame for easy plotting

events_w_forecast <- events_by_date %>% 
    left_join(hres_roll_lead5,
              by=c("governorate_name","date"="date_forecast_predict"))


events_by_date

hres_chirps_era <- bind_rows(
    hres_roll_lead5 %>% 
        rename(date= date_forecast_predict) %>% 
        select(date, governorate_name, value) %>% 
        mutate(
            source ="HRES"
        ),
    zonal_stats_high_risk_hull$roll3 %>% 
        rename(value = mean) %>% 
        mutate(date= as_date(date),
               source="CHIRPS") %>% 
        select(date,governorate_name,value, source),
    
    era5_w_rolling %>% 
        mutate(date= as_date(date),
               source="ERA5") %>% 
        select(date,governorate_name,value=era_roll3, source)
) 
hres_chirps_era_events<- events_by_date %>% 
    left_join(hres_chirps_era)

merge_rainfall_event_datasets(hres=hres_rolled_per_date_gen,
                              era=era5_w_rolling,
                              chirps=zonal_stats_high_risk_hull,
                              events=gov_area_rainfall_impact_tbl, 
                              precip_regime="roll3",
                              lead_time=5)

hres_rolled_per_date_gen %>% 
    filter(name %in% c("roll3")) %>% 
    mutate(value = if_else(value<1e-3,0,value)) %>% 
    arrange(governorate_name) %>% 
    filter(leadtime %in% c(3,4,5)) %>% 
    filter(governorate_name=="Hajjah") %>% 
    arrange(date_forecast_made)  %>% 
    mutate(uid=row_number()) %>% 
    filter(year(date_forecast_predict)==2007,month(date_forecast_predict)==7) %>% View()
    filter(value>30)

merge_rainfall_event_datasets <- function(hres=hres_rolled_per_date_gen,
                                          era=era5_rolling_zonal_local,
                                          chirps=zonal_stats_high_risk_hulls,
                                          events=gov_area_rainfall_impact_tbl, precip_regime="roll3",lead_time){
    
    precip_regime_label = switch(precip_regime,
                                 "precip_daily" = "1 ",
                                 "roll3"= "3 ",
                                 "roll5" = "5 ",
                                 "roll10"="10") %>% 
        paste0("Rainfall Accumulation (mm)")
    plot_label = glue::glue("HRES Forecast ({lead_time} day lead time) & CHIRPS + ERA Historical")
    precip_regime_num <- parse_number(precip_regime_label)
    leadtimes <- seq(precip_regime_num,lead_time, by =1)
    hres_filt <- hres %>% 
        filter(name %in% c(precip_regime)) %>% 
        mutate(value = if_else(value<1e-3,0,value)) %>% 
        arrange(governorate_name) %>% 
        # filter(leadtime==lead_time) 
        filter(leadtime%in%leadtimes) 
    
    hres_chirps_era <- bind_rows(
        hres_filt %>% 
            rename(date= date_forecast_predict) %>% 
            select(date, governorate_name, value) %>% 
            mutate(
                source ="HRES"
            ),
        chirps[[precip_regime]] %>% 
            rename(value = mean) %>% 
            mutate(date= as_date(date),
                   source="CHIRPS") %>% 
            select(date,governorate_name,value, source),
        
        era %>%
            mutate(date= as_date(date),
                   source="ERA5") %>% 
            select(date,governorate_name,value=paste0("era_",precip_regime), source)
    ) 
    
    events_by_date <- events %>% 
        filter(governorate_name %in% c("Hajjah","Marib")) %>% 
        select(date, governorate_name,fevent)
    
    hres_chirps_era_events<- events_by_date %>% 
        left_join(hres_chirps_era) %>% 
        mutate(precip_regime = precip_regime_label,
               title = plot_label)
    
    return(hres_chirps_era_events)
    
}

# Lead Time 5
merged_df_lead5 <- merge_rainfall_event_datasets(hres=hres_rolled_per_date_gen,
                              era=era5_w_rolling,
                              chirps=zonal_stats_high_risk_hull,
                              events=gov_area_rainfall_impact_tbl, 
                              precip_regime="roll3",
                              lead_time=5)

# debugonce(plot_rainfalls_forecast_events)
plot_rainfalls_forecast_events(df=merged_df_lead5,
                               gov="Hajjah", 
                               thresh=19.8, 
                               look_back=7,
                               look_ahead=3)

# debugonce(plot_rainfalls_forecast_events)
plot_rainfalls_forecast_events(df=merged_df_lead5,
                               gov="Marib", 
                               thresh=30.6, 
                               look_back=7,
                               look_ahead=3)



merged_df_lead10 <- merge_rainfall_event_datasets(hres=hres_rolled_per_date_gen,
                              era=era5_w_rolling,
                              chirps=zonal_stats_high_risk_hull,
                              events=gov_area_rainfall_impact_tbl, 
                              precip_regime="roll3",
                              lead_time=10)

plot_rainfalls_forecast_events(df=merged_df_lead10,
                               gov="Hajjah", 
                               thresh=19.8, 
                               look_back=7,
                               look_ahead=3)


plot_rainfalls_forecast_events(df=merged_df_lead10,
                               gov="Marib", 
                               thresh=30.6, 
                               look_back=7,
                               look_ahead=3)




plot_rainfalls_forecast_events(df=hres_chirps_era_events,
                                   gov="Marib", 
                                   thresh=30.6, look_back=7,look_ahead=3)


    
plot_rainfalls_forecast_events <- function(df=hres_chirps_era_events,
                                           gov="Hajjah", 
                                           thresh=19.8, 
                                           look_back=7,look_ahead=3){
    # set up
    df_gov = df %>%
        filter(governorate_name == gov) %>%
        arrange(date)
    reporting_date_range <- df_gov %>% 
        filter(fevent) %>% 
        pull(date) %>% 
        range()

    median_date <- median(reporting_date_range)
    x_axis_start <- reporting_date_range[1]-100
    x_axis_end <- reporting_date_range[2]+100
    
    df_gov_forecast <- df_gov %>% 
        filter(source=="HRES") %>% 
        group_by(date) %>% 
        filter(value==max(value)) %>% 
        ungroup()
    
       
    df_gov <- bind_rows(df_gov %>% 
                  filter(source!="HRES"),
              df_gov_forecast
              )
    df_gov_forecast <- df_gov_forecast %>% 
        filter(date %in% seq(x_axis_start,x_axis_end,1))
    y_val_vec <- df_gov %>% 
        filter(date %in% seq(x_axis_start,x_axis_end,1)) %>% 
        pull(value)
    
    rep_range_hline_position <- max(y_val_vec,na.rm=T)*1.1
    y_label <-  unique(df_gov_forecast$precip_regime)
    plot_title <-  unique(df_gov_forecast$title)
    
 
    class_res <- calc_TPFPFN(df = df_gov_forecast,
                             x = "value",
                event = "fevent",
                thresh = thresh,
                look_back = look_back, 
                look_ahead = look_ahead)
    
    
    class_res_simp <- class_res$event %>%
        select(idx, TPFN) %>% 
        mutate(
            date= df_gov_forecast[["date"]][idx],
            values = thresh+5,
            source="TP/FN"
        ) 
    
    
    df_forecast_fp <- df_gov_forecast %>%
        mutate(FP=class_res$FPs) %>% 
        filter(FP)
    event_date_df <- df_gov_forecast %>% 
        filter(fevent)

    df_gov %>% 
        ggplot()+
        geom_line(aes(x= date,
                      y= value,
                      color=source, 
                      group=source,
                      alpha= source, 
                      size = source
        ))+
        scale_size_manual(values=c(0.3,0.3,0.8))+
        scale_alpha_manual(values = c(0.6,0.8,0.8))+
        scale_x_date(limits=c(x_axis_start,x_axis_end), breaks="10 days",date_labels = "%y-%m-%d",sec.axis = dup_axis())+
        scale_y_continuous(limits=c(0,rep_range_hline_position*1.15))+
        
        # vertical segment
        geom_segment(data= event_date_df,aes(x = date , y = rep_range_hline_position*0.95, xend = date, yend = rep_range_hline_position),alpha=0.2)+
        # horizontal segment
        geom_segment(data= event_date_df,aes(x = reporting_date_range[1] , y = rep_range_hline_position, xend = reporting_date_range[2], yend = rep_range_hline_position),alpha=0.2)+
        # reported time points
        geom_point(data= event_date_df,aes(x = date , y = rep_range_hline_position),size=4,alpha=0.2)+
        # classifcation labels
        geom_label(
            data =class_res_simp,
            aes(label = TPFN, x=date,y=rep_range_hline_position*0.9)
        )  +
        # annotate seems way faster than geom_text and the fonts don't get messed up
        annotate("text",
            x=median_date,
            y= rep_range_hline_position*1.05,
            label="Flood Reporting Range"
        )  +
        annotate("text",
            x=median_date,
            y= thresh*1.1,
            label=paste0(thresh," mm"),
            color="red"
        )  +
        geom_hline(yintercept = thresh, color = "red",linetype='dashed')+
        
        geom_point(data= df_forecast_fp,
                   aes(x= date,y=value))+
        labs(y=y_label,
             title = plot_title,subtitle = gov)+
        
        theme_hdx()+
        theme(
            axis.text.x = element_text(angle=90),
            axis.title.x = element_blank()
        )
    
}




# trying to understand someting about leadtimes
hres_rolled_per_date_gen %>% 
    filter(governorate_name=="Marib", name %in% c("roll3","precip_daily")) %>% 
    filter(year(date_forecast_predict)==2022, month(date_forecast_predict)==7) %>% 
    arrange(date_forecast_made,date_forecast_predict) %>% 
    pivot_wider(names_from = name, values_from=value) %>% 
    # filter(roll3>30.6)
    View()


plot_performance_all_sites(df=rainfall_impact_tbl, 
                           x="precip_roll10",
                           event = "fevent",
                           thresh=25,
                           day_window=60
) %>% 


return_period_level_tibble(df = hres_roll3_l5 %>% 
                               filter(governorate_name=="Hajjah"),
                           value = "value", 
                           date = "date_forecast_predict", 
                           rp_year = c(2,3))
return_period_level_tibble(df = hres_roll3_l5 %>% 
                               filter(governorate_name=="Marib"),
                           value = "value", 
                           date = "date_forecast_predict", 
                           rp_year = c(2,3))


# okay try one left aligned.... no diff
library(zoo)
return_period_level_tibble(df = era5_w_rolling %>% 
                               group_by(governorate_name) %>% 
                               mutate(
                                   roll3_l = rollsum(era_precip_daily,fill = NA,align = "left",k=3)
                               ) %>% 
                               ungroup() %>% 
                               filter(governorate_name=="Hajjah") %>% 
                               filter(year(date)>=2007),
                           value = "roll3_l", 
                           date = "date", 
                           rp_year = c(2,3,4,5))


# what about if we compare to non-gee local 
tar_load(era5_rolling_zonal_local)

return_period_level_tibble(df = era5_rolling_zonal_local %>% 
                               filter(governorate_name=="Marib"),
                           value = "roll3_l", 
                           date = "date", 
                           rp_year = c(2,3,4,5))

return_period_level_tibble(df = era5_w_rolling %>% 
                               filter(governorate_name=="Hajjah"),
                           value = "era_roll3", 
                           date = "date", 
                           rp_year = c(2,3,4,5))

tar_load(era5_w_rolling)


calc_TPFPFN(df = events_w_forecast,x = "value",event = "fevent",thresh = 30)
