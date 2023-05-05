# Small script to aggregate 3 hourly surface moisture anomaly data to daily
# and then extract to zonal means 
# will convert to _targets pipeline later 

library(rgee)
library(tidyrgee)
library(targets)
library(lubridate)
ee_Initialize()
tar_load(high_risk_hulls)

ssma <- ee$ImageCollection("NASA/SMAP/SPL4SMGP/007")$select("sm_surface_anomaly")

# creates data.frame catalogue with all images and time stamp
ic_date_df <- ee_get_date_ic(ssma)

# need to loop through unique dates to aggregate them on GEE server
unique_dates <- as_date(ic_date_df$time_start) %>% 
    sort() %>%
    unique() %>%
    as.character()

# putting into a table so i can split and batch by year
# was runnning into GEE memory limits when i just looped on list
unique_dates_df <- tibble(date = as_date(ic_date_df$time_start) %>% 
    sort() %>%
    unique() %>%
    as.character(),
    year = year(as_date(date))
)

zonal_sma_extractions <- unique_dates_df %>% 
    split(.$year) %>% 
    map(
        \(temp_df){
        cat("processing year: ", unique(temp_df$year),"\n")
        
        
        ee_date_list <- ee$List(temp_df$date)
        
        ic_temp <- rgee::ee$ImageCollection$fromImages(
            ee_date_list$map(rgee::ee_utils_pyfunc(function (dt) {
                start_date <- ee$Date(dt)
                end_date <- start_date$advance(1,"day")
                ic_temp_filtered <- ssma$filterDate(start_date,end_date)            
                ic_composite <- ic_temp_filtered$
                    mean()$
                    set('system:time_start',start_date)
                return(ic_composite)
            }
            )))
        zonal_temp <-  ee_extract_tidy(x = ic_temp,
                                       y = high_risk_hulls,
                                       stat = "mean",
                                       scale = 11000, via="drive"
        )
        return(zonal_temp)
        }
    )




zonal_sma_extractions %>% 
    bind_rows() %>% 
    write_rds(
        file.path(Sys.getenv("AA_DATA_DIR"),"public","raw","yem","smap","smap_l4_soil_moisture_anomaly.rds")
    )
    


# convert to GEE list
ee_date_list <- ee$List(unique_dates)



ssma_daily <- rgee::ee$ImageCollection$fromImages(
    ee_date_list$map(rgee::ee_utils_pyfunc(function (dt) {
        start_date <- ee$Date(dt)
        end_date <- start_date$advance(1,"day")
        ic_temp_filtered <- ssma$filterDate(start_date,end_date)            
        ic_composite <- ic_temp_filtered$
            mean()$
            set('system:time_start',start_date)
        return(ic_composite)
    }
    )))



doy_precip_roll30_mean <- zonal_stats_high_risk_hull %>%
    filter(date>"2015-04-01",date<="2023-0-01") %>% 
    group_by(governorate_name) %>% 
    arrange(date) %>% 
    mutate(
        roll30 = rollmean(k=30,align="center")
    ) %>% 
    group_by(doy(date)) %>% 
    summarie(
        mean_30 = mean(roll30,na.rm=T)
    )

precip_30_diff_anomaly <- zonal_stats_high_risk_hull %>%
    filter(date>"2015-04-01",date<="2023-0-01") %>% 
    mutate(
        doy = doy(date)
    ) %>% 
    left_join(doy_precip_roll30_mean, by = "doy") %>% 
    mutate(
        precip_mean_diff= roll30 - mean30
    )

ssm_precip_30_anomaly <- ssma_zonal %>% 
    left_join(
        precip_30_diff_anomaly, by= c("governorate_name","date")   
    )

df %>% 
    pivot_longer(cols= c("precip_mean_diff","sm_surface_anomaly"))

ggplot(aes(x=date, y= v))

plot_ssm_precip_anom <-  function(df, ssm_lag=0){
    df %>% 
        pivot_onger
        ggplot(aes(x=date, y= v))
}


ssm_zonal %>% 
    group_by(governorate_name) %>% 
    arrange(date) %>% 
    summarise(
        diff= value-lag(value)
    )