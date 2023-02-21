library(tidyverse)
library(sf)
library(terra)
library(exactextractr)


con <- DBI::dbConnect(RPostgres::Postgres(),
                      host= "localhost",
                      dbname = "global_gdb",
                      user      = "postgres",
                      # password      = db_info$drc_chol_db_pw,
                      port     = 5432)

adm0 <- st_read(con, "yem_adm0")

full_fps <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"),full.names = T)
rast_names <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"))

rast_dates <- str_extract(string = rast_names,pattern = "(?<=yem_chirps_daily_).*?(?=_r0)") %>% 
    str_replace_all("_","-")

library(lubridate)

rast_cat_split<- tibble(date=rast_dates) %>% 
    mutate(
        id = row_number(),
        date= ymd(date),
        yr = year(date)
        ) %>% 
    group_split(yr)


adm0_stats <- rast_cat_split %>% 
    map(
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
            adm0_extr %>%
                pivot_longer(everything())
            gc(r)
            
            }
        
    )



adm0_stats <-   
    rast_cat_split[[4]] %>% 
                pull(date) %>% range()
            indices <- rast_cat_split[[4]] %>% 
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
            adm0_extr %>%
                pivot_longer(everything())
            gc(r)
            
            }
        
    )


# this will take some time to load them all..... but will it ever?


ts <- terra:::rast(raster::stack(full_fps[1:1000]))
ts[ts==-9999]<-NA
terra::set.names(x = ts,rast_dates[1:1000])


adm0_extr <- exact_extract(ts,
              adm0,
              fun=c("mean","stdev","median","min","max","sum"), 
              force_df=T,
              full_colnames=T
) 
adm0_extr %>%
    pivot_longer(everything())






adm0_stats<- full_fps %>% 
    map2_dfr(.y= rast_dates,.f = \(x,y){
        print(y)
        r <- terra::rast(x)
        r[r==-9999] <- NA
        
        exact_extract(r,
                      adm0,
                      fun=c("mean","stdev","median","min","max","sum","count")
        ) %>% 
            mutate(date = y)
        
        
        
    })


str_extract(string = rast_names,pattern = "(?<=yem_chirps_daily_).*?(?=_r0)")




r[r==-9999] <- NA



adm3_median_spi_area <- exact_extract(r,
                                      adm0,
                                      fun=c("mean","median","min","max","count")
)

?exact_extract(x = )