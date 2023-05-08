

# using this space to document output data sets used for other purposes



# Files used for Choropleth Map made in Q ---------------------------------


# 1. admin level % hh/pop in high risk flood sites for mapping in qgis
library(targets)
library(sf)
tar_load(high_risk_flood_stats_by_cod)

st_write(high_risk_flood_stats_by_cod$adm1,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm1_rev",append = T)
st_write(high_risk_flood_stats_by_cod$adm2,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm2_rev",append = T)




# Exploratory Shiny App Files ---------------------------------------------

# Data needs to be bundled to deploy app so moving targets.

tar_load(hres_rolled_per_date_gen)
tar_load(era_chirps_long)

write_rds(hres_rolled_per_date_gen,"app/shinydata/hres_rolled_per_date_gen.rds")
write_rds(era_chirps_long,"app/shinydata/era_chirps_long.rds")


# chirps-gefs data extracted to hulls by Monica & Pauline -- will probably eventually make this 
# a target output to simplify project structure, but for now using the csv... when I make into _target I can delete
# from here.

# will turn this is to a target- but temporarily want to see how it looks in dashboard so putting here.
chirps_gefs <- read_csv(file.path(Sys.getenv("AA_DATA_DIR"),
                                  "public",
                                  "processed",
                                  "yem",
                                  "chirps_gefs",
                                  "chirps-gefs_daily_high_risk_hulls.csv"))


# renaming for consistency with other shiny files
chirps_gefs <- chirps_gefs %>% 
    rename(date_forecast_made= "time",
           governorate_name = "gov") 

# dates missing from chirps-gefs so merge the dates back in with "NA" values to take care
# of any unexpected issues when plotting

# make date sequence that I know includes missing dates in chirps-gefs
date_seq_df <- expand_grid(date_forecast_made = seq(as_date("2000-01-01"),
                                                 as_date("2021-05-01"),1),
                        leadtime=1:10, governorate_name=c("Hajjah","Marib"))


# get missing dates
missing_from_chirps_gefs <- date_seq_df %>% 
    filter(!date_forecast_made %in% chirps_gefs$date_forecast_made)

# merge missing dates
chirps_gefs_complete<- bind_rows(chirps_gefs, missing_from_chirps_gefs) %>% 
    arrange(date_forecast_made)

# calculate rolling sums
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

# write out
write_rds(chirps_gefs_long,"shinydata/chirps_gefs.rds")


