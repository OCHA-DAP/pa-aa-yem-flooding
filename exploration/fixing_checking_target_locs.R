
shared_target_loc_fp <- file.path(Sys.getenv("AA_DATA_DIR"),
                                  "private",
                                  "exploration",
                                  "yem",
                                  "Coords - Anticipatory Action target locations.xlsx" )

shared_target_df <- readxl::read_excel(shared_target_loc_fp,skip = 1) %>% 
    clean_names()

# table shared was filtered to hajjah
target_locs <- shared_target_df %>% 
    filter(governorate_name =='Hajjah') %>% 
    rename(latitude= coordinates,
           longitude=x11)


master_coordinates <- cccm_wb$`ML- Flooding Available data` %>% 
    select(site_id, 
           latitude=available_coordinates_latitude,
           longitude=available_coordinates_longitude) %>% 
    filter(!is.na(latitude),!is.na(longitude))

dms_to_decimal <- function(x){
    
    x_dms <- str_subset(x,pattern ="°|'")

        x_split_num <- str_split(x_dms,pattern ="°|'" ) %>% 
        map(~parse_number(.x))
    deg <- x_split_num %>% 
        map_dbl(1)
    # x_split_num %>% 
    #     keep(~length(.x)>1)
    minu <- x_split_num %>% 
        map_dbl(2)
    sec <-  x_split_num %>% 
        map_dbl(3)
    deg + (minu/60)+(sec/3600)
}



new_site_coords <- target_locs %>% 
    filter(!site_id %in% master_coordinates$site_id) 

new_site_coords_dd <- new_site_coords %>% 
    mutate(
        
        latitude = ifelse(str_detect(latitude,"°|'"),
                          dms_to_decimal(latitude),
                          latitude),
        longitdue =ifelse(str_detect(longitude,"°|'"),
                          dms_to_decimal(longitude),
                          latitude)
    )

targ_locs_matching_master <- target_locs %>% 
    filter(site_id %in% master_coordinates$site_id)

target_locations_fixed <- bind_rows(targ_locs_matching_master,
          new_site_coords_dd) %>% 
    mutate(
        across(c("longitude","latitude"),~parse_number(.x))
    ) 

target_locs_sf <- target_locations_fixed %>% 
    st_as_sf(coords=c("longitude","latitude"),crs=4326)

leaf_map_targs <- leaflet(target_locs_sf) %>% 
    addCircleMarkers()

# st_write(target_locs_sf,dsn = "../../qgis_projects/aa_yem_flood.gpkg","aa_hajjah_target_sites",append=T)

# lets' check hydrosheds
library(rgee)
ee_Initialize()
targ_loc_centroid_ee <- target_locs_sf %>% 
    summarise() %>% 
    st_centroid() %>% 
    sf_as_ee()
Map$centerObject(targ_loc_centroid_ee,9)
hydro_basin <- ee$FeatureCollection("WWF/HydroSHEDS/v1/Basins/hybas_8")
m_basin <- Map$addLayer(hydro_basin) 
m_basin %>% 
    addCircles(data=target_locations_fixed)
