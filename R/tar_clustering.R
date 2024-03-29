#' map_pt_clusters
#' @description
#' project/data specific wrapper to create leaflet map showing events symbolized by cluster for both Hajjah and Marib.
#'  kmeans clustering is run on a data.frame of events using coordinates and date. The data.frame is then converted to
#'  an `sf` class object and an html style label column is added. The final step of the function creates a leaflet map.
#'  This function wraps the two functions below: `spatial_pt_clusters()` and `kmeans_cluster_events()` and is used in
#'  `05_clustering_events.rmd`.
#' @param df data.frame containing impact data which include date and coordinates. If all rows do not represent events then
#'  there must also be a logical column indicating whether row represents an event.
#' @param date \code{character} column name containing date
#' @param lon \code{character} column name containing longitude
#' @param lat \code{character} column name containing latitude
#' @param event \code{character} if data.frame is not all events, there must be a logical column indicating whether row
#'  is event. Put that column name here (default = NULL signifies that all data in data.frame are events)
#' @param k \code{integer} number of clusters to output from kmeans algorithm
#' @param scale \code{logical} whether or not to scale/center `date`,`lon`,`lat` before clustering (default = F)
#'
#' @return leaflet map showing flood events as point locations symbolized by cluster for Hajjah and Marib.
#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' tar_source()
#'
#' tar_load(cccm_flood_impact_data_w_coords)
#' cccm_impact_c1 <- cccm_flood_impact_data_w_coords %>%
#'   group_by(governorate_name, site_name, site_id, lon, lat, date = date_of_episode) %>%
#'   summarise(
#'     across(starts_with("num_"), ~ mean(.x, na.rm = T)),
#'     .groups = "drop"
#'   ) %>%
#'   filter(
#'     # when mapped this site does not fall in Marib or Hajjah.
#'     site_id != "YE1712_0643",
#'     governorate_name %in% c("Marib", "Hajjah")
#'   )
#'
#' map_pt_clusters(
#'   df = cccm_impact_c1,
#'   date = "date",
#'   lon = "lon",
#'   "lat",
#'   event = NULL, # set to NULL if every record is event. If not, specify the lgl col that indicates
#'   scale = F,
#'   k = 7
#' ) %>%
#'   addScaleBar()
#' }
map_pt_clusters <- function(df,
                            date = "date",
                            lon = "lon",
                            lat = "lat",
                            event = "fevent", scale = F,
                            k = k) {
  pts_clustered <- spatial_pt_clusters(
    df = df,
    date = date,
    lon = lon,
    lat = lat,
    event = event,
    scale = scale,
    k = k
  )
  pal_hajjah <- colorFactor(palette = "Dark2", domain = pts_clustered$Hajjah$cluster)
  pal_marib <- colorFactor(palette = "Accent", domain = pts_clustered$Marib$cluster)

  leaflet() %>%
    addProviderTiles(provider = leaflet::providers$Stamen.Toner) %>%
    addCircleMarkers(
      data = pts_clustered$Hajjah,
      popup = ~cluster,
      label = ~labels,
      # clusterOptions = markerClusterOptions(),
      color = ~ pal_hajjah(cluster)
    ) %>%
    addCircleMarkers(
      data = pts_clustered$Marib,
      popup = ~cluster,
      label = ~labels,
      # clusterOptions = markerClusterOptions(),
      color = ~ pal_marib(cluster)
    )
}


#' spatial_pt_clusters
#' @description
#' project/data specific wrapper to take data run kmeans on flood events per governorate on `date` and `coordinates`,
#'  then turn object to `sf` class object with html labels for easy mapping .
#' @param df data.frame containing impact data which include date and coordinates. If all rows do not represent events then
#'  there must also be a logical column indicating whether row represents an event.
#' @param date \code{character} column name containing date
#' @param lon \code{character} column name containing longitude
#' @param lat \code{character} column name containing latitude
#' @param event \code{character} if data.frame is not all events, there must be a logical column indicating whether row
#'  is event. Put that column name here (default = NULL signifies that all data in data.frame are events)
#' @param k \code{integer} number of clusters to output from kmeans algorithm
#' @param scale \code{logical} whether or not to scale/center `date`,`lon`,`lat` before clustering (default = F)
#'
#' @return original data.frame with new column, "cluster" indicating cluster/grouping
#'
#' @examples \dontrun{
#' library(targets)
#' library(tidyverse)
#' tar_source()
#'
#' tar_load(cccm_flood_impact_data_w_coords)
#' cccm_impact_c1 <- cccm_flood_impact_data_w_coords %>%
#'   group_by(governorate_name, site_name, site_id, lon, lat, date = date_of_episode) %>%
#'   summarise(
#'     across(starts_with("num_"), ~ mean(.x, na.rm = T)),
#'     .groups = "drop"
#'   ) %>%
#'   filter(
#'     # when mapped this site does not fall in Marib or Hajjah.
#'     site_id != "YE1712_0643",
#'     governorate_name %in% c("Marib", "Hajjah")
#'   )
#' df_clusters5 <- spatial_pt_clusters(
#'   df = cccm_impact_c1,
#'   date = "date",
#'   lon = "lon",
#'   "lat",
#'   event = NULL,
#'   scale = F,
#'   k = 5
#' )
#' }
spatial_pt_clusters <- function(df = df,
                                date = "date",
                                lon = "lon",
                                lat = "lat",
                                event = "fevent",
                                scale = F,
                                k = k) {
  split(df, df$governorate_name) %>%
    imap(\(df, nm){
      km <- kmeans_cluster_events(
        df = df,
        date = date,
        lon = lon,
        lat = lat,
        event = event, scale = scale,
        k = k
      )
      df$cluster <- paste0(nm, "_", as.character(km$cluster))
      df_sf <- st_as_sf(df,
        coords = c("lon", "lat"),
        crs = 4326
      ) %>%
        st_jitter(amount = 0.05)
      df_sf$labels <- paste0(
        "Site_ID:", df_sf$site_id,
        "<br> date:", df_sf$date,
        "<br> cluter:", df_sf$cluster
      ) %>% lapply(htmltools::HTML)

      return(df_sf)
    })
}



#' kmeans_cluster_events
#' @description
#' project/data specific wrapper for kmeans algorithm. It clusters events based on date, latitude, and longitude
#' @param df data.frame containing impact data which include date and coordinates. If all rows do not represent events then
#'  there must also be a logical column indicating whether row represents an event.
#' @param date \code{character} column name containing date
#' @param lon \code{character} column name containing longitude
#' @param lat \code{character} column name containing latitude
#' @param event \code{character} if data.frame is not all events, there must be a logical column indicating whether row
#'  is event. Put that column name here (default = NULL signifies that all data in data.frame are events)
#' @param k \code{integer} number of clusters to output from kmeans algorithm
#' @param scale \code{logical} whether or not to scale/center `date`,`lon`,`lat` before clustering (default = F)
#'
#' @return original data.frame with new column, "cluster" indicating cluster/grouping


kmeans_cluster_events <- function(df,
                                  date,
                                  lon,
                                  lat,
                                  event = NULL,
                                  k = 5,
                                  scale = F) {
  if (!is.null(event)) {
    df_event <- df %>%
      filter(!!sym(event))
  }
  # if no event column listed then they are all events -- good to have this flexibility for
  # different data sets int this analysis
  if (is.null(event)) {
    df_event <- df
  }

  df_sel <- df_event %>%
    mutate(
      date = as.numeric(date)
    ) %>%
    select(date, lon, lat)

  if (scale) {
    df_sel <- df_sel %>%
      mutate(
        across(
          everything(), ~ scale(.x, center = T, scale = T)
        )
      )
  }
  matrix_input <- df_sel %>%
    as.matrix()
  set.seed(100) # add this in for plotting. Otherwise names change and effect plots
  km <- kmeans(matrix_input, centers = k, nstart = 20)
  df_event$cluster <- as.character(km$cluster)
  return(df_event)
}


# Performance of Clusters -------------------------------------------------


#' get_exclusion_dates
#' @description  Running performance testing on clustered events requires us to find dates between clusters to use
#'  for searching for FPs. This is a helper function for that purpose
#' @param df data.frame output from pt_spatial_clusters. Contains dates of events and cluster grouping ("cluster")
#' @param date date column (default = "date")
#' @param look_back how many days around event date to look back when evaluating threshold crossing (default = 7)
#' @param look_ahead how many days around event date to look forward when evaluating threshold crossing (default = 7)
#' @return vector of dates in YMD format

get_exclusion_dates <- function(df,
                                date = "date",
                                look_back = 7, look_ahead = 7) {
  event_dates <- df %>%
    mutate(
      date = as_date(!!sym(date))
    ) %>%
    group_by(cluster) %>%
    summarise(
      min_date = min(date) - look_back,
      max_date = max(date) + look_ahead, groups = "drop",
      dates_included = list(seq(min_date, max_date, by = "day"))
    ) %>%
    pull(dates_included) %>%
    unlist() %>%
    unique() %>%
    as_date() %>%
    sort()
}



#' classify_cluster
#' @description
#' after events are clustered by date and coordinates the clusters need to be classified. This is done by:
#'  1. defining a window of dates to look for threshold crossings:
#'  calculate min & max dates per cluster then subtract/add the look_back/look_ahead, respectively
#'  2. If rainfall value crosses specified threshold in this date range the cluster is classified as a TP. If not, a FN.
#' Currently this function is only used inside `clustered_performance_calcs()` which also calculates FPs
#' @param df data.frame containing clustered flood events
#' @param rainfall \code{character} indicate which rainfall regime data set to classify
#' @param rainfall_value \code{character} indicate name of column containing numeric rainfall value (default="mean")
#' @param thresh \code{numeric} threshold to use for classification
#' @param date \code{character} name of date column
#' @param look_ahead \code{integer} number of days to look ahead of event to look for threshold crossing
#' @param look_back \code{integer} number of days to look behind of event to look for threshold crossing
#' @return data.frame containing threshold column and classification of cluster (TP or FN)

classify_cluster <- function(df,
                             rainfall,
                             rainfall_value = "mean",
                             thresh = 25,
                             date = "date",
                             look_ahead = 7,
                             look_back = 7) {
  start_search <- as_date(min(as_date(df[[date]]), na.rm = T) - look_back)
  end_search <- as_date(max(as_date(df[[date]]), na.rm = T) + look_ahead)
  date_incl <- seq(start_search, end_search, by = "day")

  rainfall_filt <- rainfall %>%
    filter(as_date(date) %in% date_incl)

  if (any(rainfall_filt[[rainfall_value]] >= thresh)) {
    data.frame(
      threshold = thresh,
      classification = "TP"
    )
  } else {
    data.frame(
      threshold = thresh,
      classification = "FN"
    )
  }
}

#' clustered_performance_calcs
#'
#' @param impact data.frame containing events, dates, and clusters
#' @param rainfall named list of data.frames containing rainfall values. Names are based on precip regime
#' @param precip_regime \code{character} which precipitation regime to use. Options are:
#'  'precip_daily'
#'  'roll3'
#'  'roll5'
#'  'roll10' (default)
#'  'roll15'
#'  'roll20'
#'  'roll25'
#'  'roll30'
#' @param date \code{character} name of date column
#' @param look_back how many days around event date to look back when evaluating threshold crossing (default = 7)
#' @param look_ahead how many days around event date to look forward when evaluating threshold crossing (default = 7)
#' @return list of data.frames containing aggregated performance metrics for each threshold:
#'   including : a.) # of TPs, FPs, FNs, b.)   f1_score, recall, accuracy,

clustered_performance_calcs <- function(impact,
                                        rainfall,
                                        precip_regime = "roll10",
                                        date,
                                        look_back,
                                        look_ahead,
                                        plot = F) {
  rainfall_split <- rainfall[[precip_regime]] %>%
    split(.$governorate_name)

  impact_split <- impact %>%
    split(.$governorate_name)

  performance_metrics <- map2(impact_split, rainfall_split, \(cluster_gov_df, rainfall_df){
    rainfall_df <- rainfall_df %>%
      arrange(date) %>%
      mutate(
        idx = row_number()
      )

    cluster_gov_df <- cluster_gov_df %>%
      mutate(date = as_date(!!sym(date)))

    excl_dates <- get_exclusion_dates(
      df = cluster_gov_df,
      date = date,
      look_back = look_back,
      look_ahead = look_ahead
    )

    date_range <- range(cluster_gov_df[[date]], na.rm = T)
    date_range_incl <- seq(date_range[1] - look_back, date_range[2] + look_ahead, by = "days")

    # df filtered for TP/FN classification
    rainfall_filt <- rainfall_df %>%
      mutate(
        date = as_date(!!sym(date))
      ) %>%
      filter(date %in% date_range_incl)

    # df  filtered for FP classification
    rainfall_fp_df <- rainfall_filt %>%
      filter(!date %in% excl_dates) %>%
      arrange(date)

    max_rainfall <- ceiling(max(rainfall_filt$mean, na.rm = T))

    # classification of events (TP/FN) is done per cluster
    events_classified <- cluster_gov_df %>%
      split(.$cluster) %>%
      # and for all thresholds
      imap_dfr(\(single_cluster_df, clust_name){
        seq(0, max_rainfall, by = 1) %>%
          map_dfr(\(threshold){
            classify_cluster(
              df = single_cluster_df,
              rainfall = rainfall_filt,
              thresh = threshold,
              look_ahead = look_ahead,
              look_back = look_back
            ) %>%
              mutate(cluster = clust_name)
          })
      }) # end double inner mapper

    tpfn_summary <- events_classified %>%
      mutate(classification = as_factor(classification)) %>%
      group_by(thresh = threshold, classification, .drop = F) %>%
      summarise(
        n = n(),
        .groups = "drop"
      )

    # need a separate mapping process for FPs
    fp_summary <- seq(0, max_rainfall, by = 1) %>%
      map_dfr(\(threshold){
        # use index to break vector into lists where each item
        # contains the consecutive run.
        idx <- rainfall_fp_df$idx
        # find break points
        idx_split_idx <- c(
          1, which(diff(idx) > (1)) + 1,
          length(idx) + 1
        )
        idx_lists <- split(idx, cumsum(seq_along(idx) %in% idx_split_idx))

        # for each list apply criteria for FP classsificatin
        fp_df <- idx_lists %>%
          map_dfr(\(idx_set){
            gte_thresh <- rainfall_fp_df[idx %in% idx_set, ][["mean"]] >= threshold
            idx_gte_thresh <- idx_set[gte_thresh]
            starts_lgl <- diff(c(0, idx_gte_thresh)) > 1
            idx_starts <- idx_gte_thresh[starts_lgl]
            num_fps <- length(idx_starts)
            tibble(
              thresh = threshold,
              classification = "FP",
              n = num_fps
            )
          }) # end inner mapper
        fp_df %>%
          group_by(thresh, classification) %>%
          summarise(
            n = sum(n, na.rm = T), .groups = "drop"
          )
      }) # end outer mapper

    class_freq <- bind_rows(tpfn_summary, fp_summary) %>%
      arrange(thresh)

    class_freq_wide <- class_freq %>%
      # group_by(threshold) %>%
      pivot_wider(
        id_cols = thresh,
        names_from = classification,
        values_from = n
      ) %>%
      mutate(
        precision = TP / (TP + FP),
        recall = TP / (TP + FN),
        f1_score = 2 * ((precision * recall) / (precision + recall))
      )


    return(class_freq_wide)
  }) # end outer map

  return(
    performance_metrics %>%
      imap(\(tbl_temp, nm_tbl){
        tbl_temp %>%
          mutate(
            governorate_name = nm_tbl
          )
      }) %>% bind_rows()
  )
}



#' viz_cluster_classification
#' @description just a function to visualize results of cluster performance calculations and make sure they make sense.
#'  Not being used other than sanity check.
#' @param impact data.frame containing events, dates, and clusters
#' @param rainfall named list of data.frames containing rainfall values. Names are based on precip regime
#' @param precip_regime \code{character} which precipitation regime to use. Options are:
#'  'precip_daily'
#'  'roll3'
#'  'roll5'
#'  'roll10' (default)
#'  'roll15'
#'  'roll20'
#'  'roll25'
#'  'roll30'
#' @param date \code{character} name of date column
#' @param threshold \code{numeric} threshold to make classifications based on.
#' @param look_back how many days around event date to look back when evaluating threshold crossing (default = 7)
#' @param look_ahead how many days around event date to look forward when evaluating threshold crossing (default = 7)
#' @return list of data.frames containing aggregated performance metrics for each threshold:
#'   including : a.) # of TPs, FPs, FNs, b.)   f1_score, recall, accuracy,
#' @return ggplot showing rainfall, threshold, events classification, and FPs.


viz_cluster_classification <- function(impact,
                                       rainfall,
                                       precip_regime = "roll10",
                                       date,
                                       threshold,
                                       look_back,
                                       look_ahead) {
  rainfall_split <- rainfall[[precip_regime]] %>%
    split(.$governorate_name)

  impact_split <- impact %>%
    split(.$governorate_name)

  plot_rainfall_classifications <- map2(impact_split, rainfall_split, \(cluster_gov_df, rainfall_df){
    rainfall_df <- rainfall_df %>%
      arrange(date) %>%
      mutate(
        idx = row_number()
      )

    cluster_gov_df <- cluster_gov_df %>%
      mutate(date = as_date(!!sym(date)))

    excl_dates <- get_exclusion_dates(
      df = cluster_gov_df,
      date = date,
      look_back = look_back,
      look_ahead = look_ahead
    )

    date_range <- range(cluster_gov_df[[date]], na.rm = T)
    date_range_incl <- seq(date_range[1] - look_back, date_range[2] + look_ahead, by = "days")

    # df filtered for TP/FN classification
    rainfall_filt <- rainfall_df %>%
      mutate(
        date = as_date(!!sym(date))
      ) %>%
      filter(date %in% date_range_incl)

    # df  filtered for FP classification
    rainfall_fp_df <- rainfall_filt %>%
      filter(!date %in% excl_dates) %>%
      arrange(date)


    # classification of events (TP/FN) is done per cluster
    events_classified <- cluster_gov_df %>%
      split(.$cluster) %>%
      # and for all thresholds
      imap_dfr(\(single_cluster_df, clust_name){
        classify_cluster(
          df = single_cluster_df,
          rainfall = rainfall_filt,
          thresh = threshold,
          look_ahead = look_ahead,
          look_back = look_back
        ) %>%
          mutate(cluster = clust_name)
      }) # end inner cluster mapping

    event_dates_classified <- cluster_gov_df %>%
      group_by(cluster) %>%
      summarise(
        start_date = min(date),
        end_date = max(date),
        date = as_date(median(date)),
        .groups = "drop"
      ) %>%
      left_join(events_classified, by = "cluster")

    # need a separate mapping process for FPs
    fp_idx <-
      # use index to break vector into lists where each item
      # contains the consecutive run.
      idx <- rainfall_fp_df$idx
    # find break points
    idx_split_idx <- c(
      1, which(diff(idx) > (1)) + 1,
      length(idx) + 1
    )
    idx_lists <- split(idx, cumsum(seq_along(idx) %in% idx_split_idx))

    # for each list apply criteria for FP classsificatin
    fp_idx <- idx_lists %>%
      map(\(idx_set){
        gte_thresh <- rainfall_fp_df[idx %in% idx_set, ][["mean"]] >= threshold
        idx_gte_thresh <- idx_set[gte_thresh]
        starts_lgl <- diff(c(0, idx_gte_thresh)) > 1
        idx_starts <- idx_gte_thresh[starts_lgl]
        if (length(idx_starts) >= 1) {
          ret <- idx_starts
        }
        if (length(idx_starts) == 0) {
          ret <- NULL
        }
        return(ret)
      }) %>%
      unlist() %>%
      unname()

    rainfall_fp_dates <- rainfall_filt %>%
      filter(idx %in% fp_idx)
    p1 <- ggplot(
      rainfall_filt,
      aes(x = date, y = mean)
    ) +
      geom_line() +
      geom_point(data = rainfall_fp_dates) +
      geom_vline(
        data = event_dates_classified,
        aes(xintercept = date), color = "red"
      ) +
      geom_label(data = event_dates_classified, aes(
        x = date,
        y = 15,
        label = classification
      )) +
      geom_hline(yintercept = threshold) +
      theme_hdx()
    return(p1)
  }) # end outer mapper


  return(plot_rainfall_classifications)
}
