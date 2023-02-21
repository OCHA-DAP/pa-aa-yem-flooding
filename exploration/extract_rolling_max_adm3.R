library(rgee)
library(tidyverse)
ee_Initialize()
chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_link)
chirps_daily_toi <- chirps$filterDate("2021-01-20","2023-01-01")
# install.packages("mapview")
# adm3<- st_simplify(adm3,1)
# mapview::npts(st_simplify(adm3,dTolerance = 10))

adm3_ee <- sf_as_ee(adm3 %>% 
                        filter(admin1Name_en=="Hajjah") %>% 
                        select(admin3Pcode)
)




rolling_10_max <- ee_roll_stat(x = chirps_daily_toi,window = 10, stat="max")

adm3_roll_max_stats <- tidyrgee::ee_extract_tidy(x = chirps_daily_toi,y = adm3_ee,stat = "max",scale = 5500,via = "drive")

write_rds(adm3_roll_max_stats,"hajjah_adm3_10d_rollstats_chirps.rds")

adm3_roll_max_stats %>% 
    ggplot(aes(x=date,y= value))+
    geom_point()+
    geom_line(
        data= cm_flood %>% 
            # filter(governorate_name=="Al Hodeidah") %>% 
            group_by(governorate_name,date_of_episode) %>% 
            summarise(
                across(starts_with("num_"),~sum(.x,na.rm=T))
            ) ,
        aes(x=ymd(date_of_episode),
            y= num_verified_hhs/100, color= governorate_name, group=governorate_name),
            
    )+
    scale_y_continuous(sec.axis= sec_axis(trans = ~.x*100,name = "cases"))


test_img <- chirps_daily_toi$first()
test_img$sample()

adm3 %>% st_drop_geometry() %>% 
    count(admin1Name_en) 
yem_convex_hull <- st_convex_hull(adm3 %>% 
                                      filter(admin1Name_en !="Socotra") %>% 
                                      summarise()
                                  )

library(leaflet )

library(rnaturalearth)
# yem50 <- rnaturalearth::ne_countries(scale = 50,country = "Yemen")
library(rgeoboundaries)
yemen_boundary <- geoboundaries("Yemen")

yem_mainlaind <- st_cast(yemen_boundary,"POLYGON") %>% 
    mutate(
        area= st_area(.)
    ) %>% 
    filter(as.numeric(area)>2e10)  

leaflet(yem_mainlaind) %>% 
    addTiles() %>% 
    addPolygons()


mainland_convex <- st_convex_hull(yem_mainlaind)
    
leaflet(mainland_convex) %>% 
    addTiles() %>% 
    addPolygons()


yem_convex_hull_ee <-  sf_as_ee(mainland_convex)

samp_test <- test_img$sample(
    region = yem_convex_hull_ee,
    scale= 5500,
    numPixels= 300,
    dropNulls=FALSE,
    geometries= FALSE
)
samp_test$colum
ee_as_sf(samp_test$randomColumn("asdf"))
tidyrgee:::set_idx.ee.imagecollection.ImageCollection


samp_test$set("test_prop","aasdfa")$getInfo()
samp_test_sf<- rgee::ee_as_sf(samp_test$set("test_prop","aasdfa"))



Map$centerObject(yem_convex_hull_ee,12)
Map$addLayer(test_img,visParams = list(min= 0, max =10,palette=c("orange","blue")),name = "rollmax")
Map$addLayer(yem_convex_hull_ee)
