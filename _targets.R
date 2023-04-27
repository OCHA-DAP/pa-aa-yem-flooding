# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:

req_CRAN_pkgs <- c(
    "targets",
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
    "glue",
    "ggiraph",
    "tidyrgee", "extRemes"
)

new_CRAN_pkgs <- req_CRAN_pkgs[!(req_CRAN_pkgs %in% installed.packages()[, "Package"])]
if (length(new_CRAN_pkgs) > 0) {
    install.packages(new_CRAN_pkgs)
}
have_gghdx <- c("gghdx") %in% installed.packages()[, "Package"]
if (!have_gghdx) {
    remotes::install_github("caldwellst/gghdx")
    install.packages("showtext")
}


library(targets)
# library(tidyverse)
# library(sf)
# library(terra)
# library(exactextractr)
# library(lubridate)
# library(readxl)
# library(janitor)
# library(zoo)
library(rgee) # `ee_Initialize()` is outside of pipeline.
ee_Initialize(drive= T)
# library(ggrepel)
# library(tidyrgee)
# library(gghdx)
# 


# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
    # i think loading the packages w/ library() call above is fine
    # until you want to try parallel compute
    packages = c(req_CRAN_pkgs,"gghdx"), # packages that your targets need to run
    format = "rds" # default storage format
    # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint


chirps_gefs_dir <- file.path(
    Sys.getenv("AA_DATA_DIR"),
    "public",
    "raw",
    "yem",
    "chirps_gefs"
)
ecmwf_mars_dir <-  file.path(
    Sys.getenv("AA_DATA_DIR"),
    "private",
    "processed",
    "yem",
    "ecmwf",
    "strange_coords"
)
ecmwf_era5_dir <-  file.path(Sys.getenv("AA_DATA_DIR"),
                             "public",
                             "raw",
                             "yem",
                             "ecmwf")



# Replace the target list below with your own:
list(
    
    # Flood Impact data -------------------------------------------------------------
    
    ## Track & Load data ----
    
    ###  Yemen COD db file path ----
    tar_target(
        cod_db_fp,
        command = file.path(
            Sys.getenv("AA_DATA_DIR"),
            "public", "raw", "yem", "cod_ab", "yem_cod.gpkg"
        ),
        format = "file"
    ),
    
    # load all admin CODs for yemen
    tar_target(
        name = adm_sf,
        command = st_layers(cod_db_fp)$name %>%
            map(
                ~ st_read(cod_db_fp, layer = .x) %>%
                    clean_names()
            ) %>%
            set_names(st_layers(cod_db_fp)$name)
    ),
    
    ### CCCM flood report db/workbook ----
    
    # track path
    tar_target(
        ccccm_fp,
        command = file.path(
            Sys.getenv("AA_DATA_DIR"),
            "private",
            "exploration",
            "yem",
            "Yemen -  CCCM Cluster -December ML - Flooding available data.xlsx"
        ), format = "file"
    ),
    # load workbook
    tar_target(
        name = cccm_wb,
        command = load_cccm_wb(ccccm_fp)
    ),
    
    ### CCCM-REACH flood score workbook ----
    
    # path
    tar_target(
        ccccm_floodscore_fp,
        command =  file.path(Sys.getenv("AA_DATA_DIR"),
                             "public",
                             "exploration",
                             "yem",
                             "REACH_YEM_Dataset_CCCM-National-IDP-Site-Flood-Hazard-Analysis_February2023.xlsx"),
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
                                                flood_category="High Hazard",
                                                by = list(governorate=c("governorate_name"),
                                                          governorate_district= c("governorate_name","district_pcode"),
                                                          district_subdistrict= c("district_pcode","sub_district_pcode")
                                                ),
                                                adm_cods=adm_sf)
    ),
    
    # Rainfall - CHIRPS -------------------------------------------------------
    # suggested by bruno -- but then we have to use `{targets}` dynamic branching
    # https://stackoverflow.com/questions/69652540/how-should-i-use-targets-when-i-have-multiple-data-files
    # tar_files(
    #     chirps_file_paths,
    #     file.path(
    #         Sys.getenv("AA_DATA_DIR"),
    #         "public",
    #         "processed",
    #         "yem", "chirps"
    #     ) %>%
    #         list.files(full.names = TRUE)
    # ),
    # tar_target(
    #     processed_files,
    #     file_paths%>%
    #         readxl::read_excel() %>% # can be anything read csv, parquet etc.
    #         janitor::clean_names() %>% # start processing
    #         mutate_at(vars(a,b,c), as.Date, format = "%Y-%m-%d"), # can be really complex operations
    #     pattern = map(file_paths)
    # ),
    
    
    # i dont see why i wouldnt just use this the directory - like this - not clear how
    # it's tracking, but i know the directory is complete now
    
    tar_target(
        chirps_dir,
        file.path(
            Sys.getenv("AA_DATA_DIR"),
            "public",
            "processed",
            "yem", "chirps"
        )),
    # extract daily chirps data to site locations
    # this takes about 10 minutes
    
    tar_target(
        name= cccm_site_chirps,
        command= chirps_daily_to_sites(raster_dir = chirps_dir,
                                       pt= cccm_flood_report_sites,batch_by_year=T)
    ),
    
    # calculate various rolling stats at site locations
    tar_target(
        name= cccm_site_chirp_stats,
        command = calc_rolling_precip_sites(df = cccm_site_chirps)
    ),
    
    # we want to get historical Return Periods for sites -- so we are going to use the convex hulls for
    # for the zonal stats
    tar_target(
        name= high_risk_hulls,
        command = high_risk_convex_hulls(master_site_list = cccm_wb$`ML- Flooding Available data`,
                                         floodscore_db = cccm_floodscore_df)
    ),
    tar_target(
        name= zonal_stats_high_risk_hull,
        command = extract_zonal_stats_chirps(raster_dir=chirps_dir,
                                             zonal_boundary=high_risk_hulls,
                                             roll_windows=c(3,5,10,15,20,25,30))
    ),
    tar_target(
        name= return_period_levels,
        command = zonal_stats_high_risk_hull %>% 
            map(\(rainfall_df){
                rainfall_df %>% 
                    group_by(governorate_name,year=year(date)) %>% 
                    summarise(
                        across(c("mean","median"),~max(.x,na.rm = T)),.groups = "drop"
                    ) %>% 
                    split(.$governorate_name) %>% 
                    map(\(gov_df){
                        yearly_max_vec <- gov_df %>% 
                            pull(mean)
                        
                        gev_fit <- fevd(yearly_max_vec, type = "GEV")
                        
                        ari_rps <- c(2,3,4,5,10)
                        return_levels_ci<- return.level(x = gev_fit, 
                                                        return.period =ari_rps,
                                                        do.ci = TRUE, 
                                                        alpha = 0.05)
                        tibble(
                            RP =ari_rps,
                            estimate_low= xyz.coords(return_levels_ci)$x,
                            estimate_upp= xyz.coords(return_levels_ci)$z,
                            estimate= xyz.coords(return_levels_ci)$y
                        )
                    })
                
            }
            )
    ),
    
    # ERA5 Historical ---------------------------------------------------------
    
    # this data set ("ECMWF/ERA5/DAILY") only goes up to "2020-07-09"
    tar_target(
        name = era5_daily_latest_re_high_risk_hulls,
        command = load_era_daily(poly = high_risk_hulls)
    ),
    # therefore we use "ECMWF/ERA5_LAND/DAILY_RAW"
    tar_target(
        name = era5_daily_raw_high_risk_hulls,
        command = load_era_daily(poly = high_risk_hulls,ds = "raw")
    ),
    tar_target(
        name = era5_w_rolling,
        command = era5_daily_raw_high_risk_hulls %>% 
            mutate(
                value = value *1000, # convert to mm 
                era_roll3 = rollsum(value, fill= NA, k=3, align ="right"),
                era_roll5 = rollsum(value, fill= NA, k=5, align ="right"),
                era_roll10 = rollsum(value, fill= NA, k=10, align ="right")
            ) %>% 
            rename(
                era_precip_daily = value
            ) %>% 
            select(-parameter)
    ),
    
    # Above uses GEE and runs zonal stats -> rolling calcs
    # here we use local files and run rolling calcs -> zonal stats
    tar_target(
        name = era5_rolling_zonal_local,
        command= ecmwf_era5_rolling_zonal_local(ecmwf_era5_dir, 
                                                pattern="\\.grib2$",
                                                zonal_boundary = high_risk_hulls,
                                                roll_windows=c(3,5,10,15,20,25,30))
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
    
    # somehow these got invalidated -- need to skip for now and let run when less busy
    tar_target(
        name = gefs_chirps_pts,
        command = extract_chirps_gefs_to_pts(raster_dir =chirps_gefs_dir,
                                             forecast = 10,
                                             sites =cccm_flood_report_sites  )
    ),
    
    ## Compare CHIRPS to CHIRPS-GEFS ----
    # Now align CHIRPS-GEFS with CHIRPS and plot together for all sites as well as random
    # site as example. `TODO` map 1000 random points to chirps
    
    tar_target(
        name = p_chirps_vs_gefs,
        command = plot_chirps_gefs_comparison(gef_values= gefs_chirps_pts,
                                              chirps_values=cccm_site_chirp_stats,
                                              gef_forecast_window = 10)
    ),
    
    # Performance Testing -----------------------------------------------------
    
    # merge flood and rainfall data 
    tar_target(
        name = rainfall_impact_tbl,
        command = merge_rainfall_cccm_impact(site_rainfall = cccm_site_chirp_stats,
                                             site_flooding = cccm_flood_impact_data)
    ),
    
    ## Plot all sites with a dummy threshold to make sure it behaving as desired
    tar_target(
        name= p_all_sites_events_pred_classifications,
        # 5th element in map causing issues-- have a lok 
        command= plot_performance_all_sites(df=rainfall_impact_tbl, 
                                            x="precip_roll10",
                                            event = "fevent",
                                            thresh=25,
                                            day_window=60
        )
    ),
    
    ## Site level Performance ----
    # do performance classification frequencies (i.e count TP,FN,FPs) for every site
    tar_target(
        name = thresh_class_freq_b7f3,
        command = c(
            "precip_roll3",
            "precip_roll5",
            "precip_roll10",
            "precip_roll15",
            "precip_roll20",
            "precip_roll25",
            "precip_roll30",
            "precip_roll3_c",
            "precip_roll5_c",
            "precip_roll10_c",
            "precip_roll15_c",
            "precip_roll20_c",
            "precip_roll25_c",
            "precip_roll30_c"
        ) %>%
            map(\(rainfall_regime){
                cat(crayon::green(rainfall_regime),"\n")
                max_rainfall <- ceiling(max(rainfall_impact_tbl[[rainfall_regime]],na.rm=T))
                performance_frequencies_by_threshold(df = rainfall_impact_tbl,
                                                     x=rainfall_regime,
                                                     by="site_id",
                                                     event="fevent",
                                                     look_back = 7,
                                                     look_ahead = 3,
                                                     thresholds = seq(0,max_rainfall,1))
            }
            ) %>% set_names( c(
                "precip_roll3",
                "precip_roll5",
                "precip_roll10",
                "precip_roll15",
                "precip_roll20",
                "precip_roll25",
                "precip_roll30",
                "precip_roll3_c",
                "precip_roll5_c",
                "precip_roll10_c",
                "precip_roll15_c",
                "precip_roll20_c",
                "precip_roll25_c",
                "precip_roll30_c"
            ) )
    ),
    # interested in running with 7 day lookback and forward
    tar_target(
        name = thresh_class_freq_b7f7,
        command = c(
            "precip_roll3",
            "precip_roll5",
            "precip_roll10",
            "precip_roll15",
            "precip_roll20",
            "precip_roll25",
            "precip_roll30",
            "precip_roll3_c",
            "precip_roll5_c",
            "precip_roll10_c",
            "precip_roll15_c",
            "precip_roll20_c",
            "precip_roll25_c",
            "precip_roll30_c"
        ) %>%
            map(\(rainfall_regime){
                cat(crayon::green(rainfall_regime),"\n")
                max_rainfall <- ceiling(max(rainfall_impact_tbl[[rainfall_regime]],na.rm=T))
                performance_frequencies_by_threshold(df = rainfall_impact_tbl,
                                                     x=rainfall_regime,
                                                     by="site_id",
                                                     event="fevent",
                                                     look_back = 7,
                                                     look_ahead = 7,
                                                     thresholds = seq(0,max_rainfall,1))
            }
            ) %>% set_names( c(
                "precip_roll3",
                "precip_roll5",
                "precip_roll10",
                "precip_roll15",
                "precip_roll20",
                "precip_roll25",
                "precip_roll30",
                "precip_roll3_c",
                "precip_roll5_c",
                "precip_roll10_c",
                "precip_roll15_c",
                "precip_roll20_c",
                "precip_roll25_c",
                "precip_roll30_c"
            ) )
    ),
    # aggregate site level performance to overall
    tar_target(
        name = tbl_performance_overall,
        command =  thresh_class_freq_b7f3 %>% 
            map(\(freq_tbl){
                calculate_performance_metrics(df =freq_tbl ,
                                              cccm_wb = cccm_wb,
                                              level="site",
                                              by = c("thresh","class"))
            }
            ) %>% set_names(names(thresh_class_freq_b7f3))
    ),
    # aggregate site level performance to governorate
    tar_target(
        name = tbl_performance_overall_gov,
        command =  thresh_class_freq_b7f3 %>% 
            map(\(freq_tbl){
                calculate_performance_metrics(df =freq_tbl ,
                                              cccm_wb = cccm_wb,
                                              level="site",
                                              by = c("governorate_name","thresh","class"))
            }
            ) %>% set_names(names(thresh_class_freq_b7f3))
    ),
    ## Area level performance ----
    # get max and mean rainfall everyday per governorate. Then if any flood event occured, date as T for 
    # fevent
    tar_target(
        name = gov_area_rainfall_impact_tbl,
        command =   rainfall_impact_tbl %>% 
            group_by(governorate_name, date) %>% 
            summarise(
                across(matches("^precip_.+"), list(max=max,mean=mean)),
                fevent = any(fevent),.groups="drop"
            ) 
    ),
    # use above table to calculate performance classifications
    tar_target(
        name= tbl_performance_gov_area_level,
        command= c(
            "precip_roll3_max",
            "precip_roll5_max",
            "precip_roll10_max",
            "precip_roll15_max",
            "precip_roll20_max",
            "precip_roll25_max",
            "precip_roll30_max",
            "precip_roll3_mean",
            "precip_roll5_mean",
            "precip_roll10_mean",
            "precip_roll15_mean",
            "precip_roll20_mean",
            "precip_roll25_mean",
            "precip_roll30_mean",
            "precip_roll3_c_max",
            "precip_roll5_c_max",
            "precip_roll10_c_max",
            "precip_roll15_c_max",
            "precip_roll20_c_max",
            "precip_roll25_c_max",
            "precip_roll30_c_max",
            "precip_roll3_c_mean",
            "precip_roll5_c_mean",
            "precip_roll10_c_mean",
            "precip_roll15_c_mean",
            "precip_roll20_c_mean",
            "precip_roll25_c_mean",
            "precip_roll30_c_mean"
        ) %>% 
            map(\(rainfall_regime){
                cat(crayon::green(rainfall_regime),"\n")
                # by getting max rainfall we can adjust threshold sequence to shorten the computatation
                max_rainfall <- ceiling(max(gov_area_rainfall_impact_tbl[[rainfall_regime]],na.rm=T))
                
                performance_frequencies_by_threshold(df = gov_area_rainfall_impact_tbl %>% 
                                                         # filter to just govs of interest
                                                         filter(governorate_name %in% c("Hajjah","Marib")),
                                                     x = rainfall_regime,
                                                     by = "governorate_name",
                                                     event = "fevent",
                                                     look_back = 7,
                                                     look_ahead = 3,
                                                     thresholds = seq(0,max_rainfall,by=1))
            }
            ) %>% set_names( c(
                "precip_roll3_max",
                "precip_roll5_max",
                "precip_roll10_max",
                "precip_roll15_max",
                "precip_roll20_max",
                "precip_roll25_max",
                "precip_roll30_max",
                "precip_roll3_mean",
                "precip_roll5_mean",
                "precip_roll10_mean",
                "precip_roll15_mean",
                "precip_roll20_mean",
                "precip_roll25_mean",
                "precip_roll30_mean",
                "precip_roll3_c_max",
                "precip_roll5_c_max",
                "precip_roll10_c_max",
                "precip_roll15_c_max",
                "precip_roll20_c_max",
                "precip_roll25_c_max",
                "precip_roll30_c_max",
                "precip_roll3_c_mean",
                "precip_roll5_c_mean",
                "precip_roll10_c_mean",
                "precip_roll15_c_mean",
                "precip_roll20_c_mean",
                "precip_roll25_c_mean",
                "precip_roll30_c_mean"
            ) )
    ),
    tar_target(
        name = tbl_performance_area_marib_hajjah,
        command =  tbl_performance_gov_area_level %>% 
            map(\(freq_tbl){
                calculate_performance_metrics(df =freq_tbl,
                                              cccm_wb = cccm_wb,
                                              level="governorate_name",
                                              by = c("governorate_name","class","thresh"))
            }
            ) %>% set_names(names(tbl_performance_gov_area_level))
    ),
    ## Clustering - Area level ####
    # Clean up high risk sites we have impact data and coordinates for 
    tar_target(
        name = cccm_flood_marib_hajjah_impact,
        command = cccm_flood_impact_data_w_coords %>% 
            mutate(date= as_date(date_of_episode)) %>% 
            group_by(governorate_name, site_name, site_id, lon, lat,date) %>% 
            summarise(
                # this just removes duplicate reporting in flood db.
                across(starts_with("num_"),~mean(.x,na.rm=T)),
                .groups="drop"
            ) %>% 
            filter(
                # when mapped this site does not fall in Marib or Hajjah.
                site_id !="YE1712_0643",
                # only interested in these 2 for now
                governorate_name %in% c("Marib","Hajjah")
            ) 
    ),
    
    # cluster Marib & Hajjah events on coordinates and date. Do Marib & Hajjah separately, then combine results
    tar_target(
        name= marib_hajjah_cccm_flood_clustered5,
        command = cccm_flood_marib_hajjah_impact %>% 
            # cluster
            spatial_pt_clusters(df=.,
                                date = "date",
                                lon = "lon",
                                lat="lat", 
                                k=5, # number of clusters
                                event = NULL, # all records in data.frame are events
                                scale=F # don't scale vars (explained in 05_clustering_events.rmd)
            ) %>% 
            map(
                ~st_drop_geometry(.x)
            ) %>% 
            bind_rows() 
        
    ),
    # run performance calculations  on clustered data
    tar_target(
        name =tbl_performance_area_clustered5_b7f7,
        command = names(zonal_stats_high_risk_hull) %>% 
            map(\( nm_tbl){
                clustered_performance_calcs(
                    impact = marib_hajjah_cccm_flood_clustered5,
                    rainfall = zonal_stats_high_risk_hull,
                    precip_regime = nm_tbl,
                    date = "date",
                    look_ahead = 7,
                    look_back = 7
                )  
            }
            ) %>% 
            set_names(names(zonal_stats_high_risk_hull))
    ),
    # run performance calculations  on clustered data
    tar_target(
        name =tbl_performance_area_clustered5_b7f7_wind_rm,
        command = names(zonal_stats_high_risk_hull) %>% 
            map(\( nm_tbl){
                clustered_performance_calcs(
                    impact = marib_hajjah_cccm_flood_clustered5 %>% 
                        filter(
                            # this whole cluster appears to be wind damage rather than rainfall
                            cluster !="Hajjah_2",
                            # confirmed that this particular event was wind rather than rainfall
                            !(site_id =="YE2613_1961"  & date =="2022-04-30")
                        ),
                    rainfall = zonal_stats_high_risk_hull,
                    precip_regime = nm_tbl,
                    date = "date",
                    look_ahead = 7,
                    look_back = 7
                )  
            }
            ) %>% 
            set_names(names(zonal_stats_high_risk_hull))
    ),
    # Impact vs Rainfall & Return period ------------------------------------------------
    
    # in this section we make plots showing a.) rainfall over flood reporting period, b.  2,3,4,5 return return period
    # levels calculated form CHIRPS 1981-2022, c.) reported events & impact
    
    ## Individual events impact ####
    
    # In these plots each point is an specific event
    tar_target(
        name = p_rainfall_rp_impact_num_shelters,
        command = zonal_stats_high_risk_hull %>%
            names() %>% 
            map(\(precip_windows){
                plot_rainfall_rps_impact(impact_data=cccm_flood_marib_hajjah_impact,
                                         historical_rainfall=zonal_stats_high_risk_hull,
                                         precip_regime= precip_windows, # map through each precip regime
                                         impact_var = "num_shelters_affected",
                                         rp_year= c(2,3,4,5, 10), # return period years
                                         scale=F, # don't scale vars before kmeans
                                         k=5, # 5 clusters 
                                         aggregate_impact = NULL # plot individual event impact -not aggregated
                )
                
            }) %>% 
            set_names(zonal_stats_high_risk_hull %>%
                          names() )
    ),
    
    ## Events Clustered and Aggregated ####
    # in these plots points represent events that have been aggregated together mostly by date, but also coords
    tar_target(
        name = p_rainfall_rp_impact_num_shelters_agg,
        command = zonal_stats_high_risk_hull %>%
            names() %>% 
            map(\(precip_windows){
                plot_rainfall_rps_impact(impact_data=cccm_flood_marib_hajjah_impact,
                                         historical_rainfall=zonal_stats_high_risk_hull,
                                         precip_regime= precip_windows,
                                         impact_var = "num_shelters_affected",
                                         rp_year= c(2,3,4,5, 10), 
                                         scale=F, 
                                         k=5, 
                                         remove_wind = F,
                                         aggregate_impact = "total" # aggregate impact_var by cluster
                )
                
            }) %>% 
            set_names(zonal_stats_high_risk_hull %>%
                          names() )
        ),
        tar_target(
            name = p_rainfall_rp_impact_num_shelters_agg_wind_rm,
            command = zonal_stats_high_risk_hull %>%
                names() %>% 
                map(\(precip_windows){
                    plot_rainfall_rps_impact(impact_data=cccm_flood_marib_hajjah_impact ,
                                             historical_rainfall=zonal_stats_high_risk_hull,
                                             precip_regime= precip_windows,
                                             impact_var = "num_shelters_affected",
                                             rp_year= c(2,3,4,5, 10), 
                                             scale=F, 
                                             k=5, 
                                             remove_wind = T,
                                             aggregate_impact = "total" # aggregate impact_var by cluster
                    )
                    
                }) %>% 
                set_names(zonal_stats_high_risk_hull %>%
                              names() )
        ),
        tar_target(
            name = p_rainfall_rp_impact_num_events_agg,
            command = zonal_stats_high_risk_hull %>%
                names() %>% 
                map(\(precip_windows){
                    plot_rainfall_rps_impact(impact_data=cccm_flood_marib_hajjah_impact,
                                             historical_rainfall=zonal_stats_high_risk_hull,
                                             precip_regime= precip_windows,
                                             impact_var = "num_shelters_affected",
                                             rp_year= c(2,3,4,5, 10),
                                             scale=F,k=5,
                                             aggregate_impact = "num_events"
                    )
                    
                }) %>% 
                set_names(zonal_stats_high_risk_hull %>%
                              names() )
        ),
    # Forecast Analysis -------------------------------------------------------
    
    # ECMWF HRES MARS ####
    tar_target(
        name = ecmwf_mars_high_risk_hulls,
        command = ecmwf_mars_historical_zonal_stats(raster_dir = ecmwf_mars_dir,
                                                    zonal_boundary = high_risk_hulls)
    ),
    tar_target(
        name = ecmwf_mars_clean,
        command = ecmwf_mars_high_risk_hulls %>% 
            rename(band="date") %>% 
            mutate(across(.cols=c("mean","median"),
                          ~.x*1000 # convert to mm
                          )) %>% 
            # rm 0 step
            filter(!str_detect(band,"tp_step=0$")) %>% 
            mutate(
                # rgx to clean up parameters needed
                date_forecast_made= as_date(str_extract(band,"\\d{4}-\\d{2}-\\d{2}")),
                leadtime =as.numeric(str_extract(band, "(?<=tp_step=)\\d+"))/24,
                date_forecasted= date_forecast_made + leadtime
            ) %>% 
            group_by(governorate_name, date_forecast_made) %>% 
            arrange(governorate_name,date_forecast_made,leadtime) %>%
            mutate(
                mean=ifelse(leadtime==1,mean,mean-lag(mean)),
                median=ifelse(leadtime==1,median,median-lag(median))
                   ) %>% 
            ungroup()
    ),
    tar_target(
        name= ecmwf_mars_leads_split_rolled,
        command = ecmwf_mars_clean %>%
            select(-median,
                   governorate_name, 
                   date_forecasted,
                   leadtime,mean) %>%
            group_by(governorate_name,leadtime) %>% 
            arrange(date_forecasted) %>%
            mutate(
                roll3=  rollsum(mean, fill= NA, k=3, align ="right"),
                roll5=  rollsum(mean, fill= NA, k=5, align ="right"),
                roll10= rollsum(mean, fill= NA, k=10, align ="right")
            ) %>% 
            ungroup() %>% 
            pivot_longer(cols = c("roll3","roll5","roll10",mean)) %>% 
            mutate(
                name = str_replace(name, "mean","daily"),
                regime_id = paste0("lead_",leadtime,"_",name)) %>% 
            select(governorate_name,regime_id, date_forecasted,value) %>% 
            pivot_wider(id_cols = c(governorate_name, date_forecasted),names_from = regime_id,values_from = value) %>% 
            rename(date_forecast_predict = "date_forecasted")
    ),
    
    tar_target(
        name = ecmwf_hres_zonal_rolling2,
        command = ecmwf_hres_rolling_zonal(raster_dir = ecmwf_mars_dir,
                                 zonal_boundary = high_risk_hulls,
                                 roll_windows=c(3,5,10),
                                 lead_times=c(1:10)
        )
    )

    )
    
    
    