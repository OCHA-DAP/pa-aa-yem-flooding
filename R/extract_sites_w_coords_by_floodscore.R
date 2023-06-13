#' extract_sites_w_coords_by_floodscore
#'
#' @param master_site_list master site list from cccm
#' @param floodscore_db 
#' @param floodscore 
#'
#' @return
#' @export
#'
#' @examples
extract_sites_w_coords_by_floodscore <-  function(master_site_list,floodscore_db, floodscore="High Hazard"){
    master_db_coords <- master_site_list$`ML- Flooding Available data` %>% 
        select(
            longitude = available_coordinates_longitude,
            latitude = available_coordinates_latitude, site_id
        ) %>%
        filter(!is.na(longitude), !is.na(latitude))
    
    high_risk_db_sp <- floodscore_db %>% 
        filter(site_flood_hazard_score ==floodscore ) %>%
        left_join(master_db_coords) %>%
        filter(!is.na(longitude), !is.na(latitude)) %>% 
        st_as_sf(coords=c("longitude","latitude"),crs=4326)
    
    return(high_risk_db_sp)
}



#' gen_district_hulls
#'
#' @param df_sf 
#' @param adm 
#' @param district_list 
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' 
#' library(tidyverse)
#' library(targets)
#' library(sf)
#' 
#' tar_load(high_risk_site_pts)
#' tar_load(adm_sf)
#' gen_district_hulls(df_sf= high_risk_site_pts, 
#'                    adm = adm_sf$yem_admbnda_adm2_govyem_cso_20191002, 
#'                    district_list= list(Hajjah="Abs",
#'                                        Marib=c("Ma'rib","Ma'rib City")
#'                                        )
#'                    )
#'}

gen_district_hulls <- function(df_sf,
                               adm,
                               district_list= list(Hajjah="Abs",Marib=c("Ma'rib","Ma'rib City"))
){
    
    adm_clean <- adm %>% 
        select(matches("^adm\\d_[ep]"))
    
    df_sf_j <- df_sf %>% 
        st_join(adm_clean)
    
    district_hulls <- district_list %>%
        imap_dfr(
            \(district,gov){
                df_sf_filt <- df_sf_j %>%
                    filter(
                        adm2_en %in% district
                    ) 
                    
                print(df_sf_filt %>% 
                          st_drop_geometry() %>% 
                          count(adm2_en))
                st_convex_hull(st_union(df_sf_filt)) %>%
                    st_as_sf() %>%
                    mutate(governorate_name = gov) %>%
                    rename(geometry = x)
            }
        ) 
    return(district_hulls)
    
}
