library(rgee)
library(tidyrgee)
library(targets)
library(lubridate)
library(zoo)
ee_Initialize()
tar_load(zonal_stats_high_risk_hull)
tar_load(era5_w_rolling)
smap <- read_rds(file.path(Sys.getenv("AA_DATA_DIR"),"public","raw","yem","smap","smap_l4_soil_moisture_anomaly.rds")) %>% 
    rename(mean = value)

smap_era_chirps_anoms <- bind_rows(
    zonal_stats_high_risk_hull$precip_daily %>% 
    mutate(
        source = "chirps",
        date= as_date(date)
    ),
era5_w_rolling %>% 
    rename(mean = era_precip_daily) %>% 
    mutate(
        source= "ERA5"
    ),
smap %>% 
    mutate(
        source= "SMAP"
    )
) %>% 
    select(1:5) %>% 
    group_by(source, governorate_name) %>% 
    arrange(date) %>% 
    mutate(
        roll30 = rollmean(mean,k=30,align = "center",fill= NA)
    ) %>% 
    mutate(
        doy=lubridate::yday(date)
    ) %>% 
    group_by(doy,.add=T) %>% 
    mutate(
        mean30 = mean(roll30,na.rm = T),
        diff_anom = roll30-mean30
    ) %>% 
    ungroup() 
    
    
smap_era_chirps_anoms %>% 
    pivot_wider(id_cols = c("governorate_name","date"),names_from =source, values_from= diff_anom) %>% 
    ggplot(aes(x= chirps, y= lag(SMAP,0)))+
    geom_point()

smap_era_chirps_anoms %>% 
    pivot_wider(id_cols = c("governorate_name","date"),names_from =source, values_from= diff_anom) %>% 
    ggplot(aes(x= ERA5, y= lag(SMAP,0)))+
    geom_point()


smap_era_chirps_anoms %>% 
    group_by(governorate_name, source,date=floor_date(date,"month")) %>% 
    summarise(
        diff_anom = mean(diff_anom,na.rm = T)
    ) %>% 
    ggplot(aes(x=date,y=diff_anom,color=source,group=source))+
    geom_line(alpha=0.4)

smap_era_chirps_anoms %>% 
    mutate(
        diff_anom = ifelse(source=="SMAP",diff_anom*10,diff_anom)
    ) %>% 
    filter(date>"2017-01-01") %>% 
    ggplot(aes(x=date,y=diff_anom,color=source,group=source))+
    geom_line(alpha=0.4)+
    facet_wrap(~governorate_name)+
    labs(y="Anomaly (difference)")+
    theme_hdx()
