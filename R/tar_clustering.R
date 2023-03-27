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


# Performance of Clusters -------------------------------------------------


#' get_exclusion_dates
#' @description  Running performance testing on clustered events requires us to find dates between clusters to use
#'  for searching for FPs. This is a helper function for that purpose
#' @param df data.frame output from pt_spatial_clusters. Contains dates of events and cluster grouping ("cluster")
#' @param date date column (default = "date")
#' @param look_back how many days around event date to look back when evaluating threshold crossing (default = 7)
#' @param look_ahead how many days around event date to look forward when evaluating threshold crossing (default = 7)
#' @return vector of dates in YMD format

get_exclusion_dates <-  function(df=events_clustered$Hajjah %>% 
                                     st_drop_geometry(),
                                 date="date", 
                                 look_back=7, look_ahead=7){
    event_dates <- df %>% 
        mutate(
            date= as_date(!!sym(date))
        ) %>% 
        group_by(cluster) %>% 
        summarise(
            min_date = min(date)- look_back,
            max_date = max(date)+look_ahead,groups="drop",
            dates_included = list(seq(min_date,max_date, by ="day"))
        ) %>% 
        pull(dates_included) %>% 
        unlist()%>% 
        unique() %>% 
        as_date() %>% 
        sort()
}



classify_cluster <- function(df,
                             rainfall,
                             rainfall_value= "mean",
                             thresh=25,
                             date= "date",
                             look_ahead=7,
                             look_back=7){
    
    start_search <- as_date(min(as_date(df[[date]]),na.rm=T)-look_back)
    end_search <- as_date(max(as_date(df[[date]]),na.rm=T)+look_ahead)
    date_incl <- seq(start_search,end_search,by="day")
    
    rainfall_filt <- rainfall %>% 
        filter(as_date(date) %in% date_incl)
    
    if(any(rainfall_filt[[rainfall_value]]>=thresh)){
        data.frame(
            threshold = thresh,
            classification = "TP"
            
        )
    }else{
        data.frame(
            threshold = thresh,
            classification = "FN"
            
        )
        
    }
}

#' Title
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
                                precip_regime="roll10",
                                date,
                                look_back,
                                look_ahead,
                                plot=F
){
    
    rainfall_split <-  rainfall[[precip_regime]] %>% 
        split(.$governorate_name) 
    
    impact_split <-  impact %>% 
        split(.$governorate_name) 
    
    performance_metrics <- map2(impact_split,rainfall_split,\(cluster_gov_df,rainfall_df){
        rainfall_df <- rainfall_df %>% 
            arrange(date) %>% 
            mutate(
                idx = row_number()
            )
        
        cluster_gov_df <- cluster_gov_df %>% 
            mutate(date= as_date(!!sym(date)))
        
        excl_dates<- get_exclusion_dates(df = cluster_gov_df,
                                         date = date, 
                                         look_back = look_back,
                                         look_ahead = look_ahead )
        
        date_range <-  range(cluster_gov_df[[date]],na.rm=T)
        date_range_incl <-  seq(date_range[1]-look_back,date_range[2]+look_ahead, by ="days")
        
        # df filtered for TP/FN classification
        rainfall_filt <- rainfall_df %>% 
            mutate(
                date= as_date(!!sym(date))
            ) %>% 
            filter(date %in% date_range_incl)
        
        # df  filtered for FP classification
        rainfall_fp_df <- rainfall_filt %>% 
            filter(!date %in% excl_dates) %>% 
            arrange(date)
        
        max_rainfall <-  ceiling(max(rainfall_filt$mean,na.rm=T))
        
        # classification of events (TP/FN) is done per cluster
        events_classified <- cluster_gov_df %>% 
            split(.$cluster) %>% 
            # and for all thresholds
            imap_dfr(\(single_cluster_df,clust_name){
                seq(0,max_rainfall, by =1) %>% 
                    map_dfr(\(threshold){
                        classify_cluster(df= single_cluster_df,
                                         rainfall= rainfall_filt,
                                         thresh = threshold,
                                         look_ahead = look_ahead ,
                                         look_back=look_back) %>% 
                            mutate(cluster= clust_name)
                    })
            }) # end double inner mapper
        
        tpfn_summary <- events_classified %>% 
            mutate(classification=as_factor(classification)) %>% 
            group_by(threshold,classification,.drop=F) %>% 
            summarise(
                n= n() ,
                .groups="drop"
            )
        
        # need a separate mapping process for FPs
        fp_summary <- seq(0,max_rainfall, by =1) %>% 
            map_dfr(\(threshold){
                
                # use index to break vector into lists where each item 
                # contains the consecutive run.
                idx <- rainfall_fp_df$idx
                # find break points
                idx_split_idx <- c(1, which(diff(idx) > (1)) + 1, 
                                   length(idx) + 1)
                idx_lists <- split(idx,cumsum(seq_along(idx) %in% idx_split_idx))
                
                # for each list apply criteria for FP classsificatin
                fp_df <- idx_lists %>% 
                    map_dfr(\(idx_set){
                        gte_thresh <- rainfall_fp_df[idx%in% idx_set,][["mean"]]>=threshold
                        idx_gte_thresh<- idx_set[gte_thresh]
                        starts_lgl <- diff(c(0,idx_gte_thresh))>1
                        idx_starts <- idx_gte_thresh[starts_lgl]
                        num_fps<- length(idx_starts)
                        tibble(
                            threshold = threshold,
                            classification = "FP",
                            n = num_fps
                        )
                    }) # end inner mapper
                fp_df %>% 
                    group_by(threshold, classification) %>% 
                    summarise(
                        n= sum(n,na.rm = T),.groups="drop"
                    )
                
            }) # end outer mapper
        
        class_freq <- bind_rows(tpfn_summary,fp_summary) %>% 
            arrange(threshold)
        
        metric_ratios <- class_freq %>% 
            # group_by(threshold) %>% 
            pivot_wider(id_cols = threshold,
                        names_from = classification,
                        values_from = n
            ) %>% 
            mutate(precision = TP/(TP+FP),
                   recall = TP/(TP+FN)) %>%
            select(threshold, precision, recall) %>% 
            pivot_longer(cols=precision:recall,
                         names_to = "metric",
                         values_to= "value")
        ret <- list()
        ret$class_freq <- class_freq
        ret$performance_metric <- metric_ratios
        return(ret)
        
    }) # end outer map
    
    return(performance_metrics)
}



viz_cluster_classification <- function(impact,
                                rainfall,
                                precip_regime="roll10",
                                date,
                                threshold,
                                look_back,
                                look_ahead,
                                plot=F
){
    
    rainfall_split <-  rainfall[[precip_regime]] %>% 
        split(.$governorate_name) 
    
    impact_split <-  impact %>% 
        split(.$governorate_name) 
    
    plot_rainfall_classifications <- map2(impact_split,rainfall_split,\(cluster_gov_df,rainfall_df){
        
        rainfall_df <- rainfall_df %>% 
            arrange(date) %>% 
            mutate(
                idx = row_number()
            )
        
        cluster_gov_df <- cluster_gov_df %>% 
            mutate(date= as_date(!!sym(date)))
        
        excl_dates<- get_exclusion_dates(df = cluster_gov_df,
                                         date = date, 
                                         look_back = look_back,
                                         look_ahead = look_ahead 
                                         )
        
        date_range <-  range(cluster_gov_df[[date]],na.rm=T)
        date_range_incl <-  seq(date_range[1]-look_back,date_range[2]+look_ahead, by ="days")
        
        # df filtered for TP/FN classification
        rainfall_filt <- rainfall_df %>% 
            mutate(
                date= as_date(!!sym(date))
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
            imap_dfr(\(single_cluster_df,clust_name){
                        classify_cluster(df= single_cluster_df,
                                         rainfall= rainfall_filt,
                                         thresh = threshold,
                                         look_ahead = look_ahead ,
                                         look_back=look_back) %>% 
                            mutate(cluster= clust_name)
                    }) # end inner cluster mapping
        
    event_dates_classified <- cluster_gov_df %>% 
        group_by(cluster) %>% 
        summarise(
            start_date= min(date),
            end_date= max(date),
            date = as_date(median(date)),
            .groups="drop"
        ) %>% 
        left_join(events_classified, by="cluster")
        
        # need a separate mapping process for FPs
        fp_idx <- 
                # use index to break vector into lists where each item 
                # contains the consecutive run.
                idx <- rainfall_fp_df$idx
                # find break points
                idx_split_idx <- c(1, which(diff(idx) > (1)) + 1, 
                                   length(idx) + 1)
                idx_lists <- split(idx,cumsum(seq_along(idx) %in% idx_split_idx))
                
                # for each list apply criteria for FP classsificatin
                fp_idx <- idx_lists %>% 
                    map(\(idx_set){
                        gte_thresh <- rainfall_fp_df[idx%in% idx_set,][["mean"]]>=threshold
                        idx_gte_thresh<- idx_set[gte_thresh]
                        starts_lgl <- diff(c(0,idx_gte_thresh))>1
                        idx_starts <- idx_gte_thresh[starts_lgl]
                        if(length(idx_starts)>=1){
                            ret <- idx_starts
                        }
                        if(length(idx_starts)==0){
                            ret <- NULL
                        }
                        return(ret)
                    }) %>% unlist() %>% unname()
                
                rainfall_fp_dates <- rainfall_filt %>% 
                    filter(idx %in% fp_idx)
                p1 <- ggplot(rainfall_filt,
                             aes(x=date ,y = mean))+
                    geom_line()+
                    geom_point(data=rainfall_fp_dates
                               )+
                    geom_vline(data=event_dates_classified,
                               aes(xintercept=date
                                   ), color="red"
                               )+
                    geom_label(data = event_dates_classified,aes(x= date,
                                                                 y=15,
                                                                 label= classification)
                               )+
                    geom_hline(yintercept= threshold)+
                    theme_hdx()
                return(p1)
            }) # end outer mapper

    
    return(plot_rainfall_classifications)
}



