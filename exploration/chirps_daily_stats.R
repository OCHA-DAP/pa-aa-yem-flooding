library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(lubridate)
library(readxl)
library(janitor)
write_zonal_stats <- c(T,F)[2]

# LOAD COD - Replace w GDRIVE ---------------------------------------------
db_info <- as.list(Sys.getenv()) %>% 
    keep_at(at = ~str_detect(.x,"^ldb"))

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host= db_info$ldb_host,
                      dbname = db_info$ldb,
                      user      = db_info$ldb_user,
                      port     = 5432)

adm0 <- st_read(con, "yem_adm0")
adm1 <- st_read(con, "yem_adm1")
adm2 <- st_read(con, "yem_adm2")

cccm_new_fp<- file.path(Sys.getenv("AA_DATA_DIR"),"private","exploration","yem","Yemen -  CCCM Cluster -December ML - Flooding available data.xlsx") 
cccm_new_sheet_names <- excel_sheets(cccm_new_fp)


ss_skip_vals <- c(1,rep(0,3))
cccm_new <- cccm_new_sheet_names %>% 
    map2(.y = ss_skip_vals,
         ~read_excel(cccm_new_fp,sheet=.x,skip = .y) %>% 
             clean_names()
    ) %>% 
    set_names(cccm_new_sheet_names) 

cccm_master_list <- cccm_new$`ML- Flooding Available data` 
cccm_floodlist <- cccm_new$`CCCM FLOOD REPORT IN IDP SITES`

lat_lon_lookup <- cccm_master_list %>% 
    select(site_id,
           longitude = available_coordinates_longitude,
           latitude = available_coordinates_latitude,
           site_population)

cccm_floodlist_w_coords <- cccm_floodlist%>% 
    left_join(lat_lon_lookup,
              by=c("cccm_idp_sites_master_list_site_id"="site_id")) %>% 
    filter(!is.na(latitude)) %>% 
    rename(site_id="cccm_idp_sites_master_list_site_id")

cccm_floodlist_sp <- st_as_sf(cccm_floodlist_w_coords, coords=c("longitude","latitude"),crs=4326)



full_fps <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"),full.names = T)
rast_names <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"))

rast_dates <- str_extract(string = rast_names,pattern = "(?<=yem_chirps_daily_).*?(?=_r0)") %>% 
    str_replace_all("_","-")


# Re Start Extraction -----------------------------------------------------


# Now that GDRIVE is fully synced turns out it might be possible to run on whole stack

# loading in works -- performance here is fine
chirps_daily_full <- terra:::rast(raster::stack(full_fps))
terra::set.names(x = chirps_daily_full,rast_dates)

# masking values and next step of rolling stat are slow on this big raster...
# would be interesting to see about `sds` methods, if no bueno batching is an option

# this is slooow 5-10 minutes
system.time(
    chirps_daily_full[chirps_daily_full==-9999] <- NA    
)

# slow also (8 min on my computer)... guess im asking alot of the local
system.time(
    chirps_roll10 <- terra::roll(chirps_daily_full,n=10,fun=sum,type="to")
)

cccm_sites_roll10 <- terra::extract(x = chirps_roll10,y=cccm_floodlist_sp)

roll10_df<- cbind(site_id= cccm_floodlist_sp$site_id,cccm_sites_roll10) %>% 
    select(-ID) %>% 
    pivot_longer(-site_id,names_to="date",values_to = "precip_roll10")

system.time(
    chirps_roll5 <- terra::roll(chirps_daily_full,n=5,fun=sum,type="to")
)
roll5_df<- cbind(site_id= cccm_floodlist_sp$site_id,cccm_sites_roll5) %>% 
    select(-ID) %>% 
    pivot_longer(-site_id,names_to="date",values_to = "precip_roll15")

system.time(
    chirps_roll3 <- terra::roll(chirps_daily_full,n=3,fun=sum,type="to")
)
system.time(
    chirps_roll30 <- terra::roll(chirps_daily_full,n=30,fun=sum,type="to")
)


# descriptive zonal stats
#########################

adm0_desc_stats <- exact_extract(chirps_daily_full,
                           adm0,
                           append_cols="admin0Pcode",
                           fun=c("mean","stdev","median","min","max","sum"), 
                           force_df=T,
                           full_colnames=T
)

adm1_desc_stats <- exact_extract(chirps_daily_full,
                           adm1,
                           append_cols=c("admin0Pcode","admin1Pcode"),
                           fun=c("mean","stdev","median","min","max","sum"), 
                           force_df=T,
                           full_colnames=T
)
adm2_desc_stats <- exact_extract(chirps_daily_full,
                           adm2,
                           append_cols=c("admin0Pcode","admin1Pcode","admin2Pcode"),
                           fun=c("mean","stdev","median","min","max","sum"), 
                           force_df=T,
                           full_colnames=T
)

adm0_roll_stats <- exact_extract(chirps_roll_full,
                           adm0,
                           append_cols=c("admin0Pcode"),
                           fun=c("mean","median","max"),
                           force_df=T,
                           full_colnames=T
)

adm1_roll_stats <- exact_extract(chirps_roll_full,
                                 adm1,
                                 append_cols=c("admin0Pcode","admin1Pcode"),
                                 fun=c("mean","median","max"),
                                 force_df=T,
                                 full_colnames=T
)
adm2_roll_stats <- exact_extract(chirps_roll_full,
                                 adm2,
                                 append_cols=c("admin0Pcode","admin1Pcode","admin2Pcode"),
                                 fun=c("mean","median","max"),
                                 force_df=T,
                                 full_colnames=T
)

adm2_roll_stats %>% 
    pivot_longer(-matches("admin\\d"))
adm_level_roll_stats <- list(adm0_roll_stats,adm1_roll_stats,adm2_roll_stats) %>% 
    map(
       ~.x  %>% 
           pivot_longer(-matches("^admin\\d")) %>% 
            separate(name,into = c('stat',"date"),sep = "\\.") %>% 
            pivot_wider(names_from = "stat",values_from = "value") %>%
            rename_with(.cols=c("mean","median","max"),~paste0("rollsum10_",.x))
    ) %>% 
    set_names(c("adm0","adm1","adm2"))

adm_level_desc_stats <- list(adm0_desc_stats,adm1_desc_stats,adm2_desc_stats) %>% 
    map(
        ~.x %>% 
            pivot_longer(-matches("^admin\\d")) %>% 
            separate(name,into = c('stat',"date"),sep = "\\.") %>% 
            pivot_wider(names_from = "stat",values_from = "value")
    ) %>% 
    set_names(c("adm0","adm1","adm2"))

adm0_zonal <- adm_level_roll_stats$adm0 %>% 
    left_join(adm_level_desc_stats$adm0)
    
adm1_zonal <- adm_level_roll_stats$adm1 %>% 
    left_join(adm_level_desc_stats$adm1)

adm_level_roll_stats$adm2 %>% head()
adm_level_desc_stats$adm2 %>% head()
adm2_zonal <- adm_level_desc_stats$adm2%>% 
    left_join(adm_level_roll_stats$adm2 )


if(write_zonal_stats){
    adm0_zonal %>% 
        write_csv(
            file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps_zonal","chirps_daily_stats_ADM0_PCODE.csv")        
        )
    adm1_zonal %>% 
        write_csv(
            file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps_zonal","chirps_daily_stats_ADM1_PCODE.csv")        
        )
    
    adm2_zonal %>% 
        write_csv(
            file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps_zonal","chirps_daily_stats_ADM2_PCODE.csv")        
        )
    
}

chirps_daily_full %>%
    names() %>% ymd() %>% range()

object.size(chirps_daily_full)
ckit<- terra::sds(full_fps)
 
system.time(
    ckit[ckit==-9999] <- NA    
)
object.size(ckit)
