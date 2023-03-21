library(rgee)
library(tidyverse)
library(janitor)
ee_Initialize()
chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_link)
tar_load(cccm_floodscore_df)
cccm_floodscore_df$site_flood_hazard_score %>% tabyl()

all_site_coords <- cccm_wb$`ML- Flooding Available data` %>% 
    select(longitude=available_coordinates_longitude,
           latitude= available_coordinates_latitude, site_id) %>% 
    filter(!is.na(longitude),!is.na(latitude))


all_high_risk_site_coords <- cccm_floodscore_df %>% 
    filter(site_flood_hazard_score=="High Hazard") %>% 
    left_join(all_site_coords) %>% 
    filter(!is.na(longitude),!is.na(latitude))

high_risk_hulls <- c("Hajjah","Marib") %>% 
    map(
        \(gov){
           pts_sf <-  all_high_risk_site_coords %>% 
            filter(governorate_name ==gov,
           site_id !="YE1712_0643"
           ) %>% 
            st_as_sf(coords=c("longitude","latitude"),crs=4326)
           st_convex_hull(st_union(pts_sf)) %>% 
               st_as_sf() %>% 
               mutate(governorate_name = gov) %>% 
               rename(geometry=x)
        }
    ) %>% set_names(c("Hajjah","Marib"))
     
hajjah_poly_ee <- sf_as_ee(high_risk_hulls$Hajjah)
# Map$addLayer(hajjah_poly_ee)

img_const_clip<- ee$Image$constant(1)$clip(hajjah_poly_ee)$selfMask()
Map$centerObject(hajjah_poly_ee,10)

Map$addLayer(img_const_clip,
             visParams = list(palette="green")
             # visParams = list(min=1,max=1,color="green")
             # name = "const_img_clipped"
             )

chirps_first <- chirps$first()
chirps_mask_first <- chirps_first$updateMask(img_const_clip)

chirps_mask_first_w_const <- chirps_mask_first$addBands(img_const_clip) 
chirps_mask_first_const <- chirps_mask_first$select("constant")
ck<- chirps_mask_first_w_const$reduceConnectedComponents( reducer=ee$Reducer$mean(), labelBands=chirps_mask_first_const)

clusterMajority =chirps_mask_first_w_const$reduceConnectedComponents(
    reducer= ee$Reducer$mean(),
    labelBand= 'constant'
)

ee$Projection(chirps)$getInfo()

img_const_clip_reproj <- img_const_clip$reproject(chirps$first()$projection()) 
chirps_zonal_ic <- chirps$map(function(img){
    # img_masked <- img$updateMask(img_const_clip)
    # img_masked_w_constant <- img_masked$addBands(img_const_clip) 
    img_masked <- img$updateMask(img_const_clip_reproj)
    img_masked_w_constant <- img_masked$addBands(img_const_clip) 
    img_masked_constant <- img_masked_w_constant$select("constant")
    img_mean =img_masked_w_constant$reduceConnectedComponents(
        reducer= ee$Reducer$mean(),
        labelBand= 'constant',
        maxSize=1024
    )$set("system:time_start",img$get("system:time_start"))
    return(img_mean)
}) 

tester <- chirps_zonal_ic$filterDate("2022-08-06","2022-08-07")$first()
chirps_raw_tester <- chirps$filterDate("2022-08-06","2022-08-07")$first()
tester_zonal <- tester %>% 
    ee_extract_tidy(y = hajjah_poly_ee,stat = "mean",scale = 5500)
chirps_raw_tester_zonal <- chirps_raw_tester %>% 
    ee_extract_tidy(y = hajjah_poly_ee,stat = "mean",scale = 5500)

chirps_zonal_ic_tidy <- as_tidyee(chirps_zonal_ic)

yearly_max_chirps <- chirps_zonal_ic_tidy %>% 
    group_by(year) %>% 
    summarise(
        stat="max"
    )


yearly_max_values_chirps_df <- yearly_max_chirps %>% 
    ee_extract_tidy(y = hajjah_poly_ee,stat = "mean",scale = 5500)

library(extRemes)
gev_daily <- fevd(yearly_max_values_chirps_df$value, type = "GEV")
ari_rps <- c(2,3,4,5,10)
rps_daily <- return.level(x = gev_daily, 
                           return.period =ari_rps,
                           do.ci = TRUE, alpha = 0.05)


# let's do some more tests
chirps_aug_2022 <- chirps$filterDate("2022-08-06","2022-08-15")
chirps_zonal_aug_2022 <- chirps_zonal_ic$filterDate("2022-08-06","2022-08-15")

chirps_aug_2022_extract <- chirps_aug_2022 %>% 
    ee_extract_tidy(y = hajjah_poly_ee,stat = "mean",scale = 5500)

chirps_zonal_aug_2022_extract <- chirps_zonal_aug_2022 %>% 
    ee_extract_tidy(y = hajjah_poly_ee,stat = "mean",scale = 5500)

# interestingly they are slightly different -- but im not sure why

chirps_aug_2022_extract;chirps_zonal_aug_2022_extract

# i thought it might b b/c we are taking the "weighted" mean in ee_extract
# therefore I thought if I just took the center point it would avoid that, but seems
# the same therefore the probcould be happening with reduce connectedcomponents
chirps_zonal_aug_2022_extract_cent <- chirps_zonal_aug_2022 %>% 
    ee_extract_tidy(y = st_centroid(high_risk_hulls$Hajjah),stat = "median",scale = 5500)

ee_help(ee$Image$reduceConnectedComponents)
chirps_aug_2022_extract;chirps_zonal_aug_2022_extract_cent

img_const_to_vec <- img_const_clip$reduceToVectors( 
    scale=5500
    )
img_const_to_vec <- ee$Feature(img_const_clip$reduceToVectors( 
    scale=5500,
    geometry= hajjah_poly_ee
    ))

img_const_to_vec$getInfo()


chirps_zonal_aug_2022_extract_from_rV <- chirps_zonal_aug_2022 %>% 
    ee_extract_tidy(y = img_const_to_vec,stat = "median",scale = 5500)

Map$addLayer(tester,visParams = list(bands="precipitation",min=0,max=5,palette=c("red","blue")))


clusterMajority$getInfo()
Map$addLayer(chirps_mask_first,
             visParams = list(palette="blue")
             # visParams = list(min=1,max=1,color="green")
             # name = "const_img_clipped"
)


Map$addLayer(chirps_masked,visParams = list(min=0,max=3))
chirps_masked <- chirps$first()$updateMask(img_const_clip)
chirps_masked2 <- chirps_masked$selfMask()
chirps_masked2$geometry()
chirps_masked$geometry()
chirps_masked$reduceRegion(reducer=ee$Reducer$mean(),geometry= hajjah_poly_ee)

chirps_masked$getInfo()
    ck<- chirps_masked$select("precipitation")$reduceConnectedComponents(reducer=ee$Reducer$mean())
ck %>% ee_print()

ckit <- chirps$map(function(img){
    img_masked <- img$updateMask(img_const_clip)
    stat<- img_masked$reduceRegion(
        reducer=ee$Reducer$mean()
                     geometry= img_masked$geometry(),
                     scale= 5500)
    ee$Image$constant(stat)
})

ckit$getInfo()
library(tidyrgee)
ck <- chirps %>% 
    ee_extract_tidy(y = hajjah_poly_ee,stat = "mean",scale = 5500)

ckit$getInfo()
chirps_masked2$reduceRegion(reducer=ee$Reducer$mean())$getInfo()
ee$Image$constant(1)$clip(ee$Feature(hajjah_poly_ee$geometry())$mask()$not())
mask = ee$Image$constant(1)$clip(ee$Feature(hajjah_poly_ee$geometry)$mask()$not())

image = image.updateMask(mask)
hajjah_poly_img_ee = ee$FeatureCollection(hajjah_poly_ee)$
    reduceToImage(
    reducer= ee$Reducer$first()
    )
leaflet() %>% 
    addTiles() %>% 
    addPolygons(data=high_risk_hulls[[1]])
terra::rasterize(x = high_risk_hulls[[1]])

?terra::as.raster()






tar_load(adm_cods)
