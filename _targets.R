# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
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


# Replace the target list below with your own:
list(
    # track cccm data
    tar_target(
        ccccm_fp,
        command = file.path(Sys.getenv("AA_DATA_DIR"),
                            "private",
                            "exploration",
                            "yem",
                            "Yemen -  CCCM Cluster -December ML - Flooding available data.xlsx") ,format = "file"
    ),
    # load ccccm workbook
  tar_target(
    name = cccm_wb,
    command =load_cccm_wb(ccccm_fp)
  ),
  # extract unique locations with coords
  tar_target(
      name= cccm_flood_report_sites,
      command = get_site_locs(cccm_wb)
  ),
  # clean up flood report data 
  tar_target(
      name= cccm_flood_impact_data,
      command = clean_cccm_impact_data(cccm_wb,floodsites = cccm_flood_report_sites)
      
  ),
  # spit out flood reports with coords for flood scan validations
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
  # extract daily chirps data to site locations
  # this takes about 10 minutes
  tar_target(
      name= cccm_site_chirps,
      command= chirps_daily_to_sites(raster_dir = chirps_dir,
                                     pt= cccm_flood_report_sites)
  ),
  tar_target(
      name= cccm_site_chirp_stats,
      command = calc_rolling_precip_sites(df = cccm_site_chirps)
  ),
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
  tar_target(name= p_timeseries_rainfall,
             command= plot_rainfall_impact_timeseries(site_rainfall = cccm_site_chirp_stats,
                                                            flood_report = cccm_flood_impact_data,
                                                            prioritization_list = high_priority_sites,
                                                            prioritize_by = "by_affected_shelters")
             ),
  # getting creative with some RS. Are there certain locations that are more vulnearable where 
  # rainfall->impact relationship is different?
  
  # geomorph
  tar_target(
      name= cccm_report_sites_with_geomorph,
      command = recode_srtm_alos_categorical(
          extract_geomorph_landform_indicators(cccm_flood_report_sites,
                                               img_scale=90)
      )
  ),
  
  # closest detected water
  tar_target(
      name= cccm_report_sites_with_fl,
      command = ee_dist_jrc_max_extent(pt=cccm_flood_report_sites, boolean="=",val=1,scale= 30,tidy_extract = T,via="drive")
      )
)