library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(lubridate)

write_zonal_stats <- c(T,F)[2]

# LOAD COD - Replace w GDRIVE ---------------------------------------------

con <- DBI::dbConnect(RPostgres::Postgres(),
                      host= "localhost",
                      dbname = "global_gdb",
                      user      = "postgres",
                      # password      = db_info$drc_chol_db_pw,
                      port     = 5432)

adm0 <- st_read(con, "yem_adm0")
adm1 <- st_read(con, "yem_adm1")
adm2 <- st_read(con, "yem_adm2")



full_fps <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"),full.names = T)
rast_names <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"))

rast_dates <- str_extract(string = rast_names,pattern = "(?<=yem_chirps_daily_).*?(?=_r0)") %>% 
    str_replace_all("_","-")


# was using this batching method when I thought there was a performance issue, but I
# do believe it was simply because the GDRIVE was not synced

rast_cat_split<- tibble(date=rast_dates) %>% 
    mutate(
        id = row_number(),
        date= ymd(date),
        yr = year(date)
        ) %>% 
    group_split(yr)


adm0_stats <- rast_cat_split %>% 
    map_dfr(
        ~{
            print(.x %>% 
                pull(date) %>% range())
            indices <- .x %>% 
                pull(id)
            
            r <- terra:::rast(raster::stack(full_fps[indices]))
            r[r==-9999]<-NA
            terra::set.names(x = r,rast_dates[indices])
            adm0_extr <- exact_extract(r,
                                       adm0,
                                       fun=c("mean","stdev","median","min","max","sum"), 
                                       force_df=T,
                                       full_colnames=T
            )
            gc(r)
            return(adm0_extr %>%
                pivot_longer(everything()))
            
            
            }
        
    )


adm0_stats_wide <- bind_rows(adm0_stats) %>% 
    separate(name,into = c('stat',"date"),sep = "\\.") %>% 
    pivot_wider(names_from = "stat",values_from = "value") %>% 
    mutate(ADM0_PCODE = "YE")

if(write_zonal_stats){
    adm0_stats_wide %>% 
        write_csv(
            file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps_zonal","chirps_daily_stats_ADM0_PCODE.csv")        
        )
    
}


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
    chirps_roll_full <- terra::roll(chirps_daily_full,n=10,fun=sum,type="to")
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
