# using this space to document output data sets used for other purposes


# 1. admin level % hh/pop in high risk flood sites for mapping in qgis
library(targets)
library(sf)
tar_load(high_risk_flood_stats_by_cod)
<<<<<<< Updated upstream
st_write(high_risk_flood_stats_by_cod$adm1,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm2",append = T)
st_write(high_risk_flood_stats_by_cod$adm2,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm0",append = T)
=======
st_write(high_risk_flood_stats_by_cod$adm1,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm1_rev",append = T)
st_write(high_risk_flood_stats_by_cod$adm2,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm2_rev",append = T)
>>>>>>> Stashed changes

# moving a few targets to specific folder for shiny -- it doesn't seem to want to access targets via tar_load and probably
# good to package theme separatley if going to deploy the app

tar_load(hres_rolled_per_date_gen)
tar_load(era_chirps_long)

write_rds(hres_rolled_per_date_gen,"shinydata/hres_rolled_per_date_gen.rds")
write_rds(era_chirps_long,"shinydata/era_chirps_long.rds")


# chirps-gefs data extracted to hulls by Monica & Pauline.

# will turn this is to a target- but temporarily want to see how it looks in dashboard so putting here.
chirps_gefs <- read_csv(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps_gefs","chirps-gefs_daily_high_risk_hulls.csv"))

chirps_gefs <- chirps_gefs %>% 
    rename(date_forecast_made= "time",
           governorate_name = "gov") 

date_seq_df <- expand_grid(date_forecast_made = seq(as_date("2000-01-01"),
                                                 as_date("2021-05-01"),1),
                        leadtime=1:10, governorate_name=c("Hajjah","Marib"))

missing_from_chirps_gefs <- date_seq_df %>% 
    filter(!date_forecast_made %in% chirps_gefs$date_forecast_made)



chirps_gefs_complete<- bind_rows(chirps_gefs, missing_from_chirps_gefs) %>% 
    arrange(date_forecast_made)

chirps_gefs_long <- chirps_gefs_complete %>% 
    mutate(
        date_forecast_predict =date_forecast_made+leadtime
    ) %>% 
    group_by(governorate_name,date_forecast_made) %>% 
    arrange(governorate_name, date_forecast_predict) %>% 
    mutate(
        roll3 = rollsum(value,k=3,align = "right",fill=NA ),
        roll5 = rollsum(value,k=5,align = "right",fill=NA ),
        roll10 = rollsum(value,k=10,align = "right",fill=NA )
    ) %>% 
    ungroup() %>% 
    rename(precip_daily= "value") %>% 
    pivot_longer(cols = matches("precip_daily|roll*")) 

write_rds(chirps_gefs_long,"shinydata/chirps_gefs.rds")




date_seq %>% 
    filter(!date%in%chirps_gefs_long$date_forecast_made)

chirps_gefs %>% 
    rename(date_forecast_made= "time",
           governorate_name = "gov") %>% 
    mutate(
        date_forecast_predict =date_forecast_made+leadtime
    ) %>% 
    group_by(governorate_name,date_forecast_made) %>% 
    arrange(governorate_name, date_forecast_predict) %>% 
    mutate(
        roll3 = rollsum(value,k=3,align = "right",fill=NA ),
        roll5 = rollsum(value,k=5,align = "right",fill=NA ),
        roll10 = rollsum(value,k=10,align = "right",fill=NA )
    ) %>% 
    ungroup() %>% 
    rename(precip_daily= "value") %>% 
    pivot_longer(cols = matches("precip_daily|roll*")) 
    write_rds("shinydata/chirps_gefs.rds")
