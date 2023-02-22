full_fps <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"),full.names = T)
rast_names <- list.files(file.path(Sys.getenv("AA_DATA_DIR"),"public","processed","yem","chirps"))

rast_dates <- str_extract(string = rast_names,pattern = "(?<=yem_chirps_daily_).*?(?=_r0)") %>% 
    str_replace_all("_","-")


# loading in works -- performance here is fine
chirps_daily <- terra:::rast(raster::stack(full_fps[1:50]))
terra::set.names(x = chirps_daily,rast_dates[1:50])


# masking values and next step of rolling stat are slow on this big raster...
# would be interesting to see about `sds` methods, if no bueno batching is an option

system.time(
    chirps_daily[chirps_daily==-9999] <- NA    
)

chirps_daily_roll <- terra::roll(chirps_daily,n=10,fun=sum,type="to")
samp_pt <- mapedit::drawFeatures()
data.frame(st_coordinates(samp_pt)) %>% 
    datapasta::df_paste()

sp_info <- data.frame(
           X = c(45.440271, 46.209712, 45.330351),
           Y = c(14.208949, 14.932393, 16.002407),
           id = c("a","b","c")
             )

samp_pt_m<- st_as_sf(sp_info, coords= c("X","Y"),crs=4326)

samp_pt_chirps_local <- terra::extract(x = chirps_daily_roll,y=samp_pt)

samp_pt_chirps_local_long <- cbind(leaflet_id=samp_pt$`_leaflet_id`,samp_pt_chirps_local) %>% 
    select(-ID) %>% 
    pivot_longer(-leaflet_id,names_to="date") 

library(rgee)
ee_Initialize()
ee_Initialize()
chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
chirps_ic <- ee$ImageCollection(chirps_link)

chirps_ic_sub <- chirps_ic$filterDate("1998-01-12" ,"1998-03-02")
rolling_10_max <- ee_roll_stat(x = chirps_ic_sub,window = 10, stat="sum")
samp_pt_ee <- sf_as_ee(samp_pt)



samp_pts_chirpsroll_ee <- tidyrgee::ee_extract_tidy(x = rolling_10_max,
                                               y = samp_pt_ee,
                                               stat="median", # reducer stat does not matter with points
                                               scale = 5500,
                                               via = "getInfo")

samp_pt_chirps_ee <- tidyrgee::ee_extract_tidy(x = chirps_ic_sub,
                                               y = samp_pt_ee,
                                               stat="median", # reducer stat does not matter with points
                                               scale = 5500,
                                               via = "getInfo")

chirps_d_roll_ee <-  samp_pt_chirps_ee %>% 
    left_join(
        samp_pts_chirpsroll_ee %>% 
    rename(roll_value = "value") ,
    by = c('X_leaflet_id',"date")) %>% 
    select(X_leaflet_id, date, value, roll_value)

chirps_d_roll_ee %>% print(n=nrow(.))


samp_pt_chirps_local_long %>% 
    rename(local_roll_value= "value") %>% 
    mutate(
        date= ymd(date)
    ) %>% 
    left_join(chirps_d_roll_ee,by=c("leaflet_id"="X_leaflet_id","date"="date")) %>% 
    select(leaflet_id, date, daily_ee = value, roll_ee=roll_value, roll_local = local_roll_value) %>% print(n=100)
    filter(!is.na(value)) %>% 
    filter(local_val!=value)

samp_pt_chirps_local_long %>% 
    rename(local_val= "value") %>% View()
