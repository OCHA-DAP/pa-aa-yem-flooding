#' these are utility functions to facilitate targets pipeline

####################################
# Note: ############################
####################################
#' They should not be though of as generic functions, rather individual scripts that take
#' a very specific input and produce a specific outline  to facilitate the work flow
#' sometimes features/arguments are added to the functions to make them more generalizeable if that is seen
#' as beneficial to reaching the goals of this project, however this is secondary to creating useful outputs.
####################################


#' load_cccm_wb
#'
#' @param path \code{character} path from AA_DATA_DIR to cccm xlsx wb
#'
#' @return list of named data.frames each containing 1 sheet from wb

load_cccm_wb <- function(path) {
  sheet_names <- excel_sheets(path)
  ss_skip_vals <- c(1, rep(0, 3))
  wb <- sheet_names %>%
    map2(
      .y = ss_skip_vals,
      ~ read_excel(path, sheet = .x, skip = .y) %>%
        clean_names()
    ) %>%
    set_names(sheet_names)
}



#' get_site_locs
#'
#' @param wb workbook list object created from `load_cccm_wb()`
#'
#' @return sf spatial data frame containing unique sites with coordinates in epsg:4326

get_site_locs <- function(wb) {
  master <- wb[["ML- Flooding Available data"]]
  flood_reports <- wb[["CCCM FLOOD REPORT IN IDP SITES"]]

  master_w_coords <- master %>%
    rename(
      longitude = "available_coordinates_longitude",
      latitude = "available_coordinates_latitude"
    ) %>%
    filter(!is.na(longitude), !is.na(latitude))

  lat_lon_lookup <- master_w_coords %>%
    select(
      site_id,
      longitude,
      latitude,
      site_population
    )

  sites_ids_w_loc_in_master <- flood_reports %>%
    rename(site_id = "cccm_idp_sites_master_list_site_id") %>%
    filter(site_id %in% master_w_coords$site_id) %>%
    distinct(site_id)


  st_as_sf(
    sites_ids_w_loc_in_master %>%
      left_join(lat_lon_lookup,
        by = "site_id"
      ),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
}

#' clean_cccm_impact_data
#'
#' @param wb workbook list object created from `load_cccm_wb()`
#' @param floodsites output from `get_site_locs()` (`cccm_flood_report_sites`)
#' @return flood report data with some minor cleaning and columns added (obj: `cccm_flood_impact_data`)

clean_cccm_impact_data <- function(wb, floodsites) {
  wb[["CCCM FLOOD REPORT IN IDP SITES"]] %>%
    rename(
      site_id = "cccm_idp_sites_master_list_site_id",
      site_name = "cccm_idp_sites_master_list_site_name"
    ) %>%
    filter(site_id %in% floodsites$site_id) %>%
    left_join(floodsites %>%
      select(site_id, site_population)) %>%
    rename(
      num_init_hhs = "initial_figures_h_hs",
      num_verified_hhs = "verified_figures_h_hs",
      num_shelters_affected = "how_many_shelters_were_affected",
      num_hhs_lost_documentation = "if_yes_how_many_h_hs_did_they_lose_their_personal_legal_documentations_due_to_the_flooding",
    ) %>%
    mutate(

      # hh size 7 from: https://fscluster.org/sites/default/files/documents/operational_guidance_note_-_minimum_expenditure_basket_september_2022.pdf
      pop_affected = num_shelters_affected * 7,
      pct_pop_affected = pop_affected / site_population,
      # pct_pop_affected > 1 in many cases, just using 10-pct_pop in denom to
      # ensure ratio should get bigger with higher %
      ratio_shelters_to_pct_pop = num_shelters_affected / (10 - pct_pop_affected)
    )
}


#' find_priority_sites
#' @description find top-n priority sites based on several metrics
#'  1. number of shelters damaged reported 2021-2022
#'  2. number of reports 2021-2022
#'  3. pct population affected
#'  4. ratio of shelters damaged to pct population
#'
#'  So far  only number of shelters damaged reported has been used in subsequent analysis/visualization
#'
#' @param wb workbook list object created from `load_cccm_wb()`
#' @param floodlist cleaned up floodlist output from `clean_cccm_impact_data()` (obj:`cccm_flood_impact_data`)
#' @param n \code{integer} number of sites to return
#'
#' @return list each containing n number of sites based on different metric

find_priority_sites <- function(wb, floodlist, n) {
  top_sites <- list()
  wb$`ML- Flooding Available data`$site_population
  site_level_stats <- wb$`CCCM FLOOD REPORT IN IDP SITES` %>%
    rename(
      site_id = cccm_idp_sites_master_list_site_id
    ) %>%
    filter(site_id %in% floodlist$site_id) %>%
    left_join(
      floodlist %>%
        st_drop_geometry(),
      by = "site_id"
    ) %>%
    rename(
      num_init_hhs = initial_figures_h_hs,
      num_verified_hhs = verified_figures_h_hs,
      num_shelters_affected = how_many_shelters_were_affected,
      num_hhs_lost_documentation = if_yes_how_many_h_hs_did_they_lose_their_personal_legal_documentations_due_to_the_flooding,
      site_name = cccm_idp_sites_master_list_site_name
    ) %>%
    mutate(
      # https://fscluster.org/sites/default/files/documents/operational_guidance_note_-_minimum_expenditure_basket_september_2022.pdf
      pop_affected = num_shelters_affected * 7,
      pct_pop_affected = pop_affected / site_population,
      ratio_shelters_to_pct_pop = num_shelters_affected / (10 - pct_pop_affected)
    ) %>%
    group_by(governorate_name, district_name, sub_district_name, site_name, site_id) %>%
    summarise(
      num_flood_reports = n(),
      max_affected_storm = max(num_shelters_affected),
      across(starts_with("num_"), ~ sum(.x, na.rm = T)),
      site_population = unique(site_population),
      mean_pct_pop_affected = mean(pct_pop_affected, na.rm = T),
      avg_ratio_shelters_to_pct_pop = mean(ratio_shelters_to_pct_pop, na.rm = T),
      max_pct_pop_affected = max(pct_pop_affected),
      .groups = "drop"
    )
  top_sites[["by_num_reports"]] <- site_level_stats %>%
    arrange(desc(num_flood_reports)) %>%
    slice(1:n)
  top_sites[["by_affected_shelters"]] <- site_level_stats %>%
    arrange(desc(num_shelters_affected)) %>%
    slice(1:n)

  top_sites[["by_pct_pop_affected"]] <- site_level_stats %>%
    arrange(desc(mean_pct_pop_affected)) %>%
    slice(1:n)

  top_sites[["by_shelters_to_pct_ratio"]] <- site_level_stats %>%
    arrange(desc(avg_ratio_shelters_to_pct_pop)) %>%
    slice(1:n)
  return(top_sites)
}


#' @title calc_rolling_precip_sites
#' @description calculate rolling sums of rainfall at site locations.
#'  Currently 3,5,10,30 day "right" & "center" aligned sums are hardcoded
#'
#' @param df spatial data.frame containing sites in epsg:4326 - output from `chirps_daily_to_sites()` (obj:`cccm_site_chirps`)
#'
#' @return data.frame site locations and rolling sums for each date (obj:`cccm_site_chirp_stats`)


calc_rolling_precip_sites <- function(df) {
  df %>%
    group_by(site_id) %>%
    arrange(site_id, date) %>%
    mutate(
      precip_roll3 = rollsum(x = precip_daily, fill = NA, k = 3, align = "right"),
      precip_roll5 = rollsum(x = precip_daily, fill = NA, k = 5, align = "right"),
      precip_roll10 = rollsum(x = precip_daily, fill = NA, k = 10, align = "right"),
      precip_roll15 = rollsum(x = precip_daily, fill = NA, k = 15, align = "right"),
      precip_roll20 = rollsum(x = precip_daily, fill = NA, k = 20, align = "right"),
      precip_roll25 = rollsum(x = precip_daily, fill = NA, k = 25, align = "right"),
      precip_roll30 = rollsum(x = precip_daily, fill = NA, k = 30, align = "right"),
      precip_roll3_c = rollsum(x = precip_daily, fill = NA, k = 3, align = "center"),
      precip_roll5_c = rollsum(x = precip_daily, fill = NA, k = 5, align = "center"),
      precip_roll10_c = rollsum(x = precip_daily, fill = NA, k = 11, align = "center"),
      precip_roll15_c = rollsum(x = precip_daily, fill = NA, k = 15, align = "center"),
      precip_roll20_c = rollsum(x = precip_daily, fill = NA, k = 21, align = "center"),
      precip_roll25_c = rollsum(x = precip_daily, fill = NA, k = 25, align = "center"),
      precip_roll30_c = rollsum(x = precip_daily, fill = NA, k = 31, align = "center")
    ) %>%
    ungroup()
}


#' plot_rainfall_impact_timeseries
#'
#' @param site_rainfall output from `calc_rolling_precip_sites()` (obj: `cccm_site_chirp_stats`)
#' @param flood_report "cleaned" flood impact data (obj: `cccm_flood_impact_data`)
#' @param prioritization_list  list of priority sites output from `find_priority_sites()` (obj:`high_priority_sites`)
#' @param prioritize_by choose which prioritzation you want to use for plotting sites
#'   currently only doing "by_affected_shelters", but added option to use otehr prioritization schema for later
#'   target objects if desired
#'
#' @return 6 ggplot time series plots showing rainfall with reported flood events



plot_rainfall_impact_timeseries <- function(site_rainfall,
                                            flood_report,
                                            prioritization_list = high_priority_sites,
                                            prioritize_by = "by_affeted_shelters") {
  plot_list <- list()

  high_impact_events <- flood_report %>%
    filter(
      site_id %in% prioritization_list[[prioritize_by]]$site_id
    ) %>%
    mutate(
      date = ymd(date_of_episode)
    )

  cccm_site_chirps_stats_long <- site_rainfall %>%
    filter(
      # site_id %in% high_priority_sites$by_affected_shelters$site_id,
      # year(date) %in% c(2021,2022)
    ) %>%
    pivot_longer(cols = contains("roll"), names_to = "period", values_to = "precip") %>%
    mutate(
      fevent = paste0(site_id, date) %in% paste0(
        flood_report$site_id,
        flood_report$date_of_episode
      ),
      date = ymd(date)
    )
  monthly_site_stats <- cccm_site_chirps_stats_long %>%
    group_by(site_id, mo = month(date), period) %>%
    summarise(
      precip = mean(precip, na.rm = T),
      .groups = "drop"
    )

  cccm_site_historical_doy <- cccm_site_chirps_stats_long %>%
    group_by(site_id, doy = day(date), period) %>%
    summarise(
      precip_mean = mean(precip, na.rm = T),
      precip_med = median(precip, na.rm = T),
      .groups = "drop"
      # median_precip=median(precip,na.rm=T)
    )

  chirps_doy_anom <- cccm_site_chirps_stats_long %>%
    filter(year(date) %in% c(2021, 2022)) %>%
    mutate(
      doy = day(date)
    ) %>%
    left_join(
      cccm_site_historical_doy,
      by = c("site_id", "period", "doy")
    ) %>%
    mutate(
      precip_anom_mean = precip - precip_mean,
      precip_anom_med = precip - precip_med
    )
  chirps_top10_anom_dat <- chirps_doy_anom %>%
    filter(
      site_id %in% prioritization_list[[prioritize_by]]$site_id
    ) %>%
    left_join(high_impact_events %>% distinct(site_id, site_name), by = "site_id")

  p_top10_shelt_affected_anom_doy_ts <- chirps_top10_anom_dat %>%
    ggplot(aes(x = date, y = precip_anom_mean, color = period)) +
    geom_line() +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    # scale_y_continuous(breaks = seq(0,400,by=25))+
    geom_vline(data = high_impact_events, aes(xintercept = date, group = site_id), color = "black", linetype = "dashed") +
    geom_text_repel(data = high_impact_events, aes(x = date, label = num_shelters_affected, y = 100), color = "black", angle = 90) +
    facet_wrap(~site_name) +
    theme_hdx() +
    labs(
      y = "Precipitation anomaly (mm)",
      title = "Precipitation anomaly vs Flood Events",
      subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)",
      caption = "1. Black dashed line mark date of reported flood event. Vertical label indicates # of shelters reported damaged\n2. Colored lines in timeseries represent daily precipitation anomaly for each accumulation window. Anomaly generated by calculating: comparing 2021/2022 rolling windows\nagainst historical averages for each window."
    ) +
    theme(
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = 90),
      # plot.caption.position = "plot"
      plot.caption = element_text(hjust = 0)
      # legend.position="none"
    )

  ### zoom in
  p_zoom_in_historical <- chirps_top10_anom_dat %>%
    filter(
      snakecase::to_snake_case(site_name) == "al_sowayda",
      !str_detect(period, "_c$")
    ) %>%
    select(-precip_anom_mean, -precip_anom_med, -doy) %>%
    pivot_longer(cols = c("precip_mean", "precip_med", "precip"), names_to = "precip_type") %>%
    ggplot(aes(x = date, y = value, color = precip_type, group = precip_type)) +
    geom_line() +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    facet_wrap(~period) +
    theme_hdx() +
    labs(
      y = "Precipitation (mm)",
      title = "Precipitation at different accumulation windows vs historical",
      subtitle = "Yemen: Al Sowayda site",
      caption = ""
    ) +
    theme(
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = 90),
      # plot.caption.position = "plot"
      plot.caption = element_text(hjust = 0)
      # legend.position="none"
    )
  p_zoom_in_historical_pseudolog <- p_zoom_in_historical +
    scale_y_continuous(trans = scales::pseudo_log_trans())



  cccm_chirps_monthly_anomaly <- cccm_site_chirps_stats_long %>%
    filter(year(date) %in% c(2021, 2022)) %>%
    mutate(mo = month(date)) %>%
    left_join(monthly_site_stats %>% rename(precip_historical = "precip"), by = c("site_id", "mo", "period")) %>%
    mutate(precip_anom_mean = precip - precip_historical)


  p_top10_shelt_affected <- cccm_chirps_monthly_anomaly %>%
    filter(
      site_id %in% prioritization_list[[prioritize_by]]$site_id
    ) %>%
    left_join(
      high_impact_events %>%
        distinct(site_id, site_name),
      by = "site_id"
    ) %>%
    mutate(
      date = ymd(date)
    ) %>%
    ggplot(aes(x = date, y = precip, color = period)) +
    geom_line() +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, 400, by = 25)) +
    geom_vline(data = high_impact_events, aes(xintercept = date, group = site_id), color = "black", linetype = "dashed") +
    geom_text_repel(data = high_impact_events, aes(x = date, label = num_shelters_affected, y = 100), color = "black", angle = 90) +
    facet_wrap(~site_name) +
    theme_hdx() +
    labs(
      y = "Precipitation: 10 day rolling sum (mm)",
      title = "Precipitation vs Flood Events",
      subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)"
    ) +
    theme(
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = 90)
      # legend.position="none"
    )

  p_top10_shelt_affected_anom_mo_ts <- cccm_chirps_monthly_anomaly %>%
    filter(site_id %in% prioritization_list[[prioritize_by]]$site_id) %>%
    left_join(high_impact_events %>% distinct(site_id, site_name)) %>%
    ggplot(aes(x = date, y = precip_anom_mean, color = period)) +
    geom_line() +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    # scale_y_continuous(breaks = seq(0,400,by=25))+
    geom_vline(data = high_impact_events, aes(xintercept = date, group = site_id), color = "black", linetype = "dashed") +
    geom_text_repel(data = high_impact_events, aes(x = date, label = num_shelters_affected, y = 100), color = "black", angle = 90) +
    facet_wrap(~site_name) +
    theme_hdx() +
    labs(
      y = "Precipitation anomaly (mm)",
      title = "Precipitation anomaly vs Flood Events",
      subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)",
      caption = "1. Black dashed line mark date of reported flood event. Vertical label indicates # of shelters reported damaged\n2. Colored lines in timeseries represent daily precipitation anomaly for each accumulation window. Anomaly generated by calculating: a.) rolling statistics daily, b.) averaging each rolling statistic per month\nthroughout the historical record (1998-2022), c.) subtracting daily rolling statistic for monthly average"
    ) +
    theme(
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = 90),
      # plot.caption.position = "plot"
      plot.caption = element_text(hjust = 0)
      # legend.position="none"
    )




  p_ts_chirs_vs_rainfall <- cccm_chirps_monthly_anomaly %>%
    filter(!str_detect(period, "_c$")) %>%
    mutate(
      site_period = paste0(site_id, period)
    ) %>%
    ggplot(aes(x = date, y = precip, group = site_id)) +
    geom_line(alpha = 0.1) +
    geom_vline(
      data = cccm_site_chirps_stats_long %>%
        filter(
          fevent,
          !str_detect(period, "_c$")
        ),
      aes(xintercept = date), alpha = 0.2
    ) +
    facet_wrap(~period) +
    scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
    scale_y_continuous(breaks = seq(0, 400, by = 25)) +
    theme_hdx() +
    labs(
      y = "Rolling precip (mm)",
      title = "Precipitation vs Flood Events",
      subtitle = "Yemen: 260 IDP sites with 390 reported flood events"
    ) +
    theme(
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(angle = 90),
      legend.position = "none"
    )

  plot_list[["top10_sites_shelter_affected"]] <- p_top10_shelt_affected
  plot_list[["top10_sites_shelter_affected_anom_mo"]] <- p_top10_shelt_affected_anom_mo_ts
  plot_list[["top10_sites_shelter_affected_anom_doy"]] <- p_top10_shelt_affected_anom_doy_ts
  plot_list[["p_zoom_in_historical"]] <- p_zoom_in_historical
  plot_list[["p_zoom_in_historical_pseudolog"]] <- p_zoom_in_historical_pseudolog
  plot_list[["all_sites_chirps"]] <- p_ts_chirs_vs_rainfall
  return(plot_list)
}


#' extract_chirps_gefs_to_pts
#' @description extract CHIRPS-GEFs forecast values to all site locations as well as 1000 random points generated
#'  in convex hull of sites. `NOTE:` this function takes well over an hour to run on my local.
#' @param raster_dir \code{character} raster directory containing CHIRPS-GEFS 5,10,15 day sub-folder
#' @param forecast \code{integer} forecast length of interest (5,10,15), currently only looking at 10 day, but
#'  this function should be re-useable for others
#' @param sites spatial data frame containing sites in epsg:4326 - output of `get_site_locs()` (obj: `cccm_flood_report_sites`)
#'
#' @return a list of 2 data.frames containing, 1. all site_ids with there daily CHIRPS-GEFS values,
#'  2. 1000 random points with id, coordinate, and daily CHIRPS-GEFS values. Random points generated
#'  within convex_hull made from site locations (obj: `gefs_chirps_pts`)



extract_chirps_gefs_to_pts <- function(raster_dir, forecast, sites = cccm_flood_report_sites) {
  forecast_str <- as.character(forecast)
  sub_dir <- switch(forecast_str,
    "10" = "10days",
    "5" = "05days",
    "15" = "15days"
  )
  raster_dir_full <- file.path(raster_dir, sub_dir)
  file_names_short <- list.files(raster_dir_full)
  file_names_full <- list.files(raster_dir_full, full.names = T)

  rast_dates <- str_extract_all(string = file_names_short, "\\d+") %>%
    map(2) %>%
    unlist() %>%
    ymd()

  # I'm going to batch read in the rasters by year to get an understanding
  # of time
  file_tbl <- tibble(
    file_names_short,
    file_names_full,
    rast_dates,
    rast_year = year(rast_dates)
  )
  file_tbl_split <- split(file_tbl, file_tbl$rast_year)

  rstack_list <- file_tbl_split %>%
    map(~ {
      yr_temp <- unique(.x$rast_year)
      cat(yr_temp, "\n")
      rstack <- terra:::rast(raster::stack(.x$file_names_full))
      terra::set.names(rstack, .x$rast_dates)
      return(rstack)
    })

  gef_chirps_sites <- rstack_list %>%
    map_dfr(
      ~ {
        gefs_sites <- terra::extract(x = .x, y = sites)
        cbind(site_id = sites$site_id, gefs_sites) %>%
          select(-ID) %>%
          pivot_longer(
            -site_id,
            names_to = "date",
            values_to = paste0("gefs_chirps_", forecast_str)
          )
      }
    )
  chull <- st_convex_hull(sites %>% summarise())
  set.seed(700)
  rnd_sample <- st_sample(chull, size = 1000) %>%
    st_sf() %>%
    mutate(samp_id = row_number())

  gef_chirps_sample <- rstack_list %>%
    map_dfr(
      ~ {
        gefs_sample <- terra::extract(x = .x, y = rnd_sample)
        cbind(site_id = rnd_sample$samp_id, st_coordinates(rnd_sample), gefs_sample) %>%
          select(-ID) %>%
          pivot_longer(
            -c(site_id, X, Y),
            names_to = "date",
            values_to = paste0("gefs_chirps_", forecast_str)
          )
      }
    )
  ret <- list()
  ret$chirps_gefs_sites <- gef_chirps_sites
  ret$chirps_gefs_sample <- gef_chirps_sample

  return(ret)
}




#' plot_chirps_gefs_comparison
#' @description plots daily CHIRPS against CHIRPS-GEFS forecast based on rolling sum and forecast window
#'  dates are aligne by: 1.  `gef_forecast_window`-1 days added CHIRPS-GEFS date, 2.  "right" aligned rolling some of `gef_forecast_window` length
#' @param gef_values data.frame containing daily CHIRPS-GEFS values extracted to points - output of
#'  `extract_chirps_gefs_to_pts()`(obj:`gefs_chirps_pts`)
#' @param chirps_values data.frame containing site_id and rainfall statistics - output of `calc_rolling_precip_sites()` (obj: `cccm_site_chirp_stats`)
#' @param gef_forecast_window \code{integer} forecast window of CHIRPS-GEFS data
#'
#' @return 2 ggplot objects: one containing the daily CHIRPS & CHIRPS-GEFS values averaged across all sites plotted against eachother
#'  2. all daily CHIRPS & CHIRPS-GEFS data for a random site plotted against each other



plot_chirps_gefs_comparison <- function(gef_values, chirps_values, gef_forecast_window = 10) {
  ret <- list()

  day_adj_gefs <- gef_forecast_window - 1
  rainfall_and_forecast_site <- gef_values$chirps_gefs_sites %>%
    mutate(
      date = ymd(date),
      date = date + day_adj_gefs
    ) %>%
    left_join(
      chirps_values %>%
        mutate(date = ymd(date)),
      by = c("site_id", "date")
    )

  p_gefs_chirps_site_avg <- rainfall_and_forecast_site %>%
    group_by(date) %>%
    summarise(
      mean_gefs10 = mean(gefs_chirps_10, na.rm = T),
      mean_chirps10 = mean(precip_roll10, na.rm = T)
    ) %>%
    ggplot(aes(x = mean_gefs10, y = mean_chirps10)) +
    geom_point(alpha = 0.1, color = "#1EBFB3") +
    labs(
      x = "CHIRPS-GEFS 10-day accumulation Forecast (mm)",
      y = "CHIRPS 10 day accumulation (mm)",
      title = "CHIRPS-GEFS Forecast compared to CHIRPS Historical",
      subtitle = "Yemen - daily averages of CHIRPS & CHIRPS GEFS calculated across 260 CCCM sites",
      caption = "Dates in data sets aligned by the following manipulation:\n a.) CHIRPS 10-day rolling sum ('right aligned'), b.) CHIRPS-GEFS - added 9 days to raster date"
    ) +
    geom_smooth(method = "lm", formula = y ~ x) +
    theme_hdx() +
    theme(
      plot.caption = element_text(hjust = 0)
    )

  p_gef_chirps_rnd_site <- rainfall_and_forecast_site %>%
    filter(site_id == "YE2002_1161") %>%
    ggplot(aes(x = gefs_chirps_10, y = precip_roll10)) +
    geom_point(alpha = 0.2, color = "#1EBFB3") +
    labs(
      x = "CHIRPS-GEFS 10-day accumulation Forecast (mm)",
      y = "CHIRPS 10 day accumulation (mm)",
      title = "CHIRPS-GEFS Forecast compared to CHIRPS Historical",
      subtitle = "Yemen CCCM Site YE2002_1161: 2000-2022",
      caption = "Dates in data sets aligned by the following manipulation:\n
        a.) CHIRPS 10-day rolling sum ('right aligned')\nb.) CHIRPS-GEFS - added 9 days to raster date\n"
    ) +
    geom_smooth(method = "lm", formula = y ~ x) +
    theme_hdx() +
    theme(
      plot.caption = element_text(hjust = 0)
    )

  ret$averaged_across_sites <- p_gefs_chirps_site_avg
  ret$rnd_site <- p_gef_chirps_rnd_site
  return(ret)
}


# Not using ####################
#################################


load_local_chirps_stack <- function(fps, names) {
  r_stack <- terra:::rast(raster::stack(fps))
  terra::set.names(x = r_stack, names)
  r_stack[r_stack == -9999] <- NA
  return(wrap(r_stack))
}

roll_wrapped <- function(x) {
  x_unwrapped <- unwrap(x)
  x_roll <- roll(x_unwrapped,
    n = 10,
    fun = sum,
    type = "to"
  )
  return(wrap(x_roll))
}

#' chirps_rollsum_to_sites
#'
#' @param raster_dir \code{character} path AA_DATA_DIR to folder containing daily chirps
#' @param pt
#' @param roll_window
#'
#' @return
#' @export
#'
#' @examples
chirps_rollsum_to_sites <- function(raster_dir, pt, roll_window) {
  full_fps <- list.files(raster_dir, full.names = T)
  rast_names <- list.files(raster_dir)

  rast_dates <- str_extract(
    string = rast_names,
    pattern = "(?<=yem_chirps_daily_).*?(?=_r0)"
  ) %>%
    str_replace_all("_", "-")

  cat("loading chirps local data sets")
  chirps_daily_full <- terra:::rast(raster::stack(full_fps))
  terra::set.names(x = chirps_daily_full, rast_dates)

  cat("replacing -9999 with NA")
  chirps_daily_full[chirps_daily_full == -9999] <- NA

  ret <- list()
  for (i in seq_along(roll_window)) {
    window_temp <- roll_window[i]
    cat("running window ", window_temp, " days")
    chirps_roll <- terra::roll(chirps_daily_full,
      n = window_temp,
      fun = sum,
      type = "to"
    )
    roll_df <- terra::extract(x = chirps_roll, y = pt)
    gc(chirps_roll)

    ret[[paste0("precip_roll", window_temp)]] <- cbind(site_id = pt$site_id, roll_df) %>%
      select(-ID) %>%
      pivot_longer(
        -site_id,
        names_to = "date",
        values_to = paste0("precip_roll", window_temp)
      )
  }
  return(
    reduce(ret, left_join, by = "site_id")
  )
}



#' grp_data_by
#' @description  helper function for batching cross-tabs with dplyr
#' @param df data.frame
#' @param by variables in data.frame you want to group by
#'
#' @return
#' @export
#'
#' @examples
grp_data_by <- function(df, by) {
  ret <- list()
  if (length(by) == 0 | is.null(by)) {
    ret$df <- df
  }
  if (length(by) == 1) {
    by_sym <- sym(by)
    ret$by <- by_sym
    ret$df <- df %>%
      group_by(!!by_sym)
  }
  if (length(by) > 1) {
    by_sym <- syms(by)
    ret$df <- df %>%
      group_by(!!!by_sym)
    ret$by <- by_sym
  }
  return(ret)
}


#' floodscore_pct_by
#'
#' @param df data.frame containing flood scores. In this example we filter the data.frame first to the high flood score sites (obj: `cccm_floodscore_df`)
#' @param by \code{character} vector containing variables to group/calcualte by
#'
#' @return
#' @export
#'
#' @examples
floodscore_pct_by <- function(df, by) {
  df_grp <- df %>%
    grp_data_by(by = by)

  stats <- df_grp$df %>%
    summarise(
      hhs = sum(number_of_households, na.rm = T),
      pop = sum(site_population, na.rm = T),
      avg_hh_size = pop / hhs,
      hhs_site_managed = sum(ifelse(site_managed == "Yes", number_of_households, NA), na.rm = T),
      pop_site_managed = sum(ifelse(site_managed == "Yes", site_population, NA), na.rm = T),
      avg_hh_size_site_managed = pop_site_managed / hhs_site_managed,
      .groups = "drop_last"
    ) %>%
    mutate(
      pct_all_hhs = hhs / sum(hhs),
      pct_all_pop = pop / sum(pop),
      pct_all_hhs_site_managed = hhs_site_managed / sum(hhs_site_managed, na.rm = T),
      pct_all_pop_site_managed = pop_site_managed / sum(pop_site_managed, na.rm = T)
    )
  if (length(df_grp$by) > 1) {
    stats %>%
      arrange(!!!df_grp$by, desc(pct_all_hhs)) %>%
      ungroup()
  } else {
    stats %>%
      arrange(!!df_grp$by, desc(pct_all_hhs)) %>%
      ungroup()
  }
}


#' floodscore_pop_stats_by_admin
#' @description
#' feed in flood scores, flood category, what to make calculation `by` , and admin CODs, and return statistics
#' about population & hhs at each admin level in the specified flood category.
#' @param df data.frame containing flood scores (obj: `cccm_floodscore_df`)
#' @param flood_category \code{character} flood category matching one of the categories in `site_flood_hazard_score` column of df
#'   (default = "High risk")
#' @param by list of by arguments to crunch stats by
#' @param adm_cods list of admin CODs (obj: `adm_sf`)
#'
#' @return list of 3 spatial data.frames containing adm1, 2, and 3 CODs and hh/pop statistics about % and pop in the specified flood category
#' @note
#' this are the outputs for qgis maps contained: ... enter path

floodscore_pop_stats_by_admin <- function(floodscores = cccm_floodscore_df,
                                          flood_category = "High Hazard",
                                          by = list(
                                            governorate = c("governorate_name"),
                                            governorate_district = c("governorate_name", "district_pcode"),
                                            district_subdistrict = c("district_pcode", "sub_district_pcode")
                                          ),
                                          adm_cods = adm_sf) {
  ret <- list()

  admin_stats_df <- by %>%
    map(
      ~ {
        print(.x)
        floodscores %>%
          filter(
            site_flood_hazard_score == flood_category
          ) %>%
          floodscore_pct_by(by = .x)
      }
    ) %>%
    set_names(c("gov", "district", "subdistrict"))

  # unfortunately a little clean up of gov admin names this cccm-reach db required
  admin_stats_df$gov <- admin_stats_df$gov %>%
    mutate(
      governorate_name = snakecase::to_snake_case(governorate_name),
      governorate_name = case_when(
        governorate_name == "marib" ~ "ma_rib",
        governorate_name == "sadah" ~ "sa_dah",
        governorate_name == "sanaa" ~ "sana_a",
        governorate_name == "sanaa_city" ~ "sana_a_city",
        governorate_name == "taiz" ~ "ta_iz",
        TRUE ~ governorate_name
      )
    )


  ret$adm1 <- adm_cods$yem_admbnda_adm1_govyem_cso_20191002 %>%
    mutate(
      adm1_en_clean = snakecase::to_snake_case(adm1_en)
    ) %>%
    left_join(admin_stats_df$gov, by = c("adm1_en_clean" = "governorate_name"))

  ret$adm2 <- adm_cods$yem_admbnda_adm2_govyem_cso_20191002 %>%
    left_join(admin_stats_df$district, by = c("adm2_pcode" = "district_pcode"))

  ret$adm3 <- adm_cods$yem_admbnda_adm3_govyem_cso_20191002 %>%
    left_join(admin_stats_df$subdistrict, by = c("adm3_pcode" = "sub_district_pcode"))
  return(ret)
}


plot_performance_all_sites <- function(df,
                                       x,
                                       event,
                                       thresh = 25,
                                       day_window = 60) {
  p_sites_level_performance <- df$site_id %>%
    unique() %>%
    map(\(site_id_temp){
      print(site_id_temp)
      plot_site_events_classified(
        df = df %>%
          filter(site_id == site_id_temp) %>%
          arrange(date),
        plot_title = site_id_temp,
        x = x,
        event = event,
        thresh = thresh, day_window = 60
      )
    }) %>%
    set_names(df$site_id %>% unique())
  return(p_sites_level_performance)
}


high_risk_convex_hulls <- function(master_site_list, floodscore_db) {
  all_site_coords <- master_site_list %>%
    select(
      longitude = available_coordinates_longitude,
      latitude = available_coordinates_latitude, site_id
    ) %>%
    filter(!is.na(longitude), !is.na(latitude))

  all_high_risk_site_coords <- floodscore_db %>%
    filter(site_flood_hazard_score == "High Hazard") %>%
    left_join(all_site_coords) %>%
    filter(!is.na(longitude), !is.na(latitude))

  high_risk_convex_hulls <- c("Hajjah", "Marib") %>%
    map(
      \(gov){
        pts_sf <- all_high_risk_site_coords %>%
          filter(
            governorate_name == gov,
            # i did a spatial check w/ adm1 COD -- this is the only one outisde
            site_id != "YE1712_0643"
          ) %>%
          st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
        st_convex_hull(st_union(pts_sf)) %>%
          st_as_sf() %>%
          mutate(governorate_name = gov) %>%
          rename(geometry = x)
      }
    ) %>%
    bind_rows()
  return(high_risk_convex_hulls)
}
