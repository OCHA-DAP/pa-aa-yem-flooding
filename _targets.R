# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:

req_CRAN_pkgs <- c("targets",
                   "tidyverse",
                   "sf",
                   "terra",
                   "exactextractr",
                   "lubridate",
                   "readxl",
                   "janitor",
                   "zoo",
                   "rgee",
                   "ggrepel",
                   "tidyrgee")

new_CRAN_pkgs<- req_CRAN_pkgs[!(req_CRAN_pkgs %in% installed.packages()[,"Package"])]
if(length(new_CRAN_pkgs)>0){
    install.packages(new_CRAN_pkgs)
}
have_gghdx <- c("gghdx") %in% installed.packages()[,"Package"]
if(!have_gghdx){
    remotes::install_github("caldwellst/gghdx")
    install.packages("showtext")
}


library(targets)
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(lubridate)
library(readxl)
library(janitor)
library(zoo)
library(rgee)
library(ggrepel)
library(tidyrgee)
library(gghdx)

ee_Initialize(drive= T)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
    packages = c("tibble"), # packages that your targets need to run
    format = "rds" # default storage format
    # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint


chirps_dir <- file.path(Sys.getenv("AA_DATA_DIR"),
                        "public",
                        "processed",
                        "yem","chirps")

chirps_gefs_dir <- file.path(Sys.getenv("AA_DATA_DIR"),
                             "public",
                             "raw",
                             "yem",
                             "chirps_gefs") 


# Replace the target list below with your own:
list(
    
    # Flood Impact data -------------------------------------------------------------
    
    ## Track & Load data ----
    
    ###  Yemen COD db file path ----
    tar_target(
        cod_db_fp,
        command = file.path(Sys.getenv("AA_DATA_DIR"),
                            "public","raw","yem","cod_ab","yem_cod.gpkg"),
        format = "file"
    ),
    
    # load all admin CODs for yemen
    tar_target(
        name= adm_sf,
        command = st_layers(cod_db_fp)$name %>% 
        map(
            ~st_read(cod_db_fp,layer=.x) %>% 
                clean_names()
        ) %>% 
        set_names(st_layers(cod_db_fp)$name)
    ),
    
    ### CCCM flood report db/workbook ----
    #track path   
    tar_target(
        ccccm_fp,
        command = file.path(Sys.getenv("AA_DATA_DIR"),
                            "private",
                            "exploration",
                            "yem",
                            "Yemen -  CCCM Cluster -December ML - Flooding available data.xlsx") ,format = "file"
    ),
    # load workbook
    tar_target(
        name = cccm_wb,
        command =load_cccm_wb(ccccm_fp)
    ),
    ### CCCM-REACH flood score workbook ----
    # path
    tar_target(
        ccccm_floodscore_fp,
        command =  file.path(Sys.getenv("AA_DATA_DIR"),
                             "public",
                             "exploration",
                             "yem",
                             "REACH_YEM_Dataset_CCCM National IDP Site Flood Risk Analysis_February2023.xlsx"),
        format = "file"
    ),
    # load workbook
    tar_target(
        name = cccm_floodscore_df,
        command = read_xlsx(path = ccccm_floodscore_fp,
                            sheet = "2023 Flood Hazard Scores",
                            skip = 1) %>% 
            clean_names() %>% 
            rename(
                site_flood_hazard_score= "x2023_cccm_idp_site_flood_hazard_score_integrating_2023_sncc_feedback"
            )
    ),
    
    ## Begin Impact Data Manipulation/Analysis ----
    
    
    ### Unique geographic locations ----
    # extract unique locations with coordinates
    tar_target(
        name= cccm_flood_report_sites,
        command = get_site_locs(cccm_wb)
    ),
    
    ### Clean up flood report db ----
    # clean up flood report data 
    tar_target(
        name= cccm_flood_impact_data,
        command = clean_cccm_impact_data(cccm_wb,floodsites = cccm_flood_report_sites)
        
    ),
    
    # produce flood reports with coords for flood scan validations
    tar_target(
        name= cccm_flood_impact_data_w_coords, # shared this for floodscan verification
        command = cccm_flood_impact_data %>% 
            left_join(
                cccm_flood_report_sites %>% 
                    mutate(lon = sf::st_coordinates(.)[,1],
                           lat = sf::st_coordinates(.)[,2]) %>% 
                    st_drop_geometry(),
                by="site_id"
            ) 
    ),
    
    ### Site prioritization (flood report db) ----
    # chose 4 different metrics to investigate which sites we could focus on
    # 1.) # shelters affected
    # 2.) # reports
    # 3.) % population affected
    # 4.) Ratio # of shelters:%pop
    tar_target(
        name = high_priority_sites,
        command= find_priority_sites(wb =cccm_wb,
                                     floodlist =cccm_flood_report_sites,
                                     n=10)
        
    ),
    ### CCCCM high flood risk pop ----
    tar_target(
        name = high_risk_flood_stats_by_cod ,
        command = floodscore_pop_stats_by_admin(floodscores=cccm_floodscore_df,
                                                flood_category="High risk",
                                                by = list(governorate=c("governorate_name"),
                                                          governorate_district= c("governorate_name","district_pcode"),
                                                          district_subdistrict= c("district_pcode","sub_district_pcode")
                                                ),
                                                adm_cods=adm_sf)
    ),
    
    
    # Rainfall - CHIRPS -------------------------------------------------------
    
    # extract daily chirps data to site locations
    # this takes about 10 minutes
    tar_target(
        name= cccm_site_chirps,
        command= chirps_daily_to_sites(raster_dir = chirps_dir,
                                       pt= cccm_flood_report_sites)
    ),
    
    # calculate various rolling stats at site locations
    tar_target(
        name= cccm_site_chirp_stats,
        command = calc_rolling_precip_sites(df = cccm_site_chirps)
    ),
    
    
    
    # Rainfall + Impact -------------------------------------------------------
    
    # produce a bunch of plots over the timespan (2021-2022) of CCCM reporting
    # comparing reported events against different window accumualations.
    # also plot some historical averages against 2021-22 rainfall
    
    tar_target(name= p_timeseries_rainfall,
               command= plot_rainfall_impact_timeseries(site_rainfall = cccm_site_chirp_stats,
                                                        flood_report = cccm_flood_impact_data,
                                                        prioritization_list = high_priority_sites,
                                                        prioritize_by = "by_affected_shelters")
    ),
    
    ## Additional Remote Sensing ----
    
    # Idea is that there could be a different relationship between rain & flooding at different locations
    # and that this could be driven by environmental/geographic factors. Therefore will attempte to 
    # extract these to sites for use later when examining performance/thresholds
    
    ## Geomorphology ----
    
    # peaks, valleys, ridges, slopes, etc.
    tar_target(
        name= cccm_report_sites_with_geomorph,
        command = recode_srtm_alos_categorical(
            extract_geomorph_landform_indicators(cccm_flood_report_sites,
                                                 img_scale=90)
        )
    ),
    
    ## Closest Detected Water ----
    
    # extract distance to closest water pixel ever detected from JRC data set.
    # perhaps will help ID more riverine type sites.
    
    tar_target(
        name= cccm_report_sites_with_fl,
        command = ee_dist_jrc_max_extent(pt=cccm_flood_report_sites, boolean="=",val=1,scale= 30,tidy_extract = T,via="drive")
    ),
    
    # Rainfall vs Rainfall Forecast -------------------------------------------
    
    ## Extract Forecast ----
    # Load CHIRPS-GEFS forecast data (so far 10 days only)-- this takes a solid 1+ hours
    # Once loaded extract values to sites as well as 1000 random points (for later testing)
    
    tar_target(
        name = gefs_chirps_pts,
        command = extract_chirps_gefs_to_pts(raster_dir =chirps_gefs_dir,forecast = 10,sites =cccm_flood_report_sites  )
    ),
    
    ## Compare CHIRPS to CHIRPS-GEFS ----
    # Now align CHIRPS-GEFS with CHIRPS and plot together for all sites as well as random
    # site as example. `TODO` map 1000 random points to chirps
    tar_target(
        name = p_chirps_vs_gefs,
        command = plot_chirps_gefs_comparison(gef_values= gefs_chirps_pts,chirps_values=cccm_site_chirp_stats,gef_forecast_window = 10)
    )
)