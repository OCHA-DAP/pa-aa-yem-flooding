

#' calculate_performance_metrics 
#' @description 
#' helper function used in target pipeline which is run on:
#' 1. data.frames stored thresh_class_freq_b7f3 to create targets: tbl_performance_overall_gov, tbl_performance_overall
#' 2. data.frames stored in : tbl_performance_gov_area_level to create targets: tbl_performance_area_marib_hajjah
#' The actual classification of events (TP & FN) and FPs is done the prior step using the function: `performance_frequencies_by_threshold()` which relies 
#' heavily on `calc_TPFPFN()` used internally.
#' @param df data.frame
#' @param cccm_wb target data.fraem (cccm_wb) which contains list of data.frames. 
#' @param level \code{character} level of input data set. For example when we use `thesh_class_freq_b7f3` the classificaitons were done at the site level. 
#'   Therefore, to aggregate to the governorate level we have need to have the governorate column added.
#' @param by \code{character} levels to aggregate performance frequencies (TP, FP, FN) by.
#'
#' @return summary table for aggregated performance class frequencies at each threshold as well as `precision` and `recall`
#' 
calculate_performance_metrics <- function(df,
                                          cccm_wb,
                                          level = "site",
                                          by = c("governorate_name", "thresh", "class")) {
  if (level == "site") {
    df <- df %>%
      # should remove dependency on this
      left_join(cccm_wb$`ML- Flooding Available data` %>%
        select(site_id, contains("governorate")))
  }
  df_summarised <- df %>%
    mutate(
      across(all_of(by), ~ factor(.x))
    ) %>%
    group_by(across(all_of(by)), .drop = F) %>%
    summarise(
      n = sum(n, na.rm = T), .groups = "drop"
    )

  if ("governorate_name" %in% by) {
    df_summarised_wide <-
      df_summarised %>%
      pivot_wider(
        id_cols = c("governorate_name", "thresh"),
        names_from = class, values_from = n
      )
  } else {
    df_summarised_wide <-
      df_summarised %>%
      pivot_wider(id_cols = thresh, names_from = class, values_from = n)
  }

  df_summarised_wide <-
    df_summarised_wide %>%
    mutate(
      precision = TP / (TP + FP),
      recall = TP / (TP + FN),
      f1_score = 2 * ((precision * recall) / (precision + recall))
    )

  return(df_summarised_wide)
}


#' plot_performance_metrics
#' @description
#' helper function used in `_targets`pipeline to plot performance classifications aggregated by threshold as well as performance metrics such as precision and recall
#' @param df
#' @param governorate \code{character} indicate which governorate to plot data for. If NULL (default), the plot is created for all together
#' @param pseudo_log \code{logical} if TRUE pseudolog scale y-axis. If, FALSE (default) a linear continuous scale is used.
#' @return ggplot style  dual y-axis plot.  e x-axis = threshold, left y-axis corresponds to the number of TPs, FPs, FNs at each threshold , 
#'  and the right y-axis corresponds to f1_score and precision ratios
#'
#' @examples \dontrun{
#' library(targets)
#' library(tiddyverse)
#' tar_source()
#' tar_load(tbl_performance_5d_gov)
#' plot_performance_metrics(df = tbl_performance_5d_gov, 
#'                          governorate = "Hajjah",
#'                          pseudo_log = F,
#'                          x_axis_title = "asdfa")
#' }



plot_performance_metrics <- function(df,
                                     governorate = NULL,
                                     pseudo_log = F,
                                     x_axis_title) {
  if (!is.null(governorate)) {
    df <- df %>%
      filter(governorate_name == governorate)
    plot_title <- paste0("Yemen: ", governorate)
  }
  if (is.null(governorate)) {
    plot_title <- "Yemen"
  }
  thresh_of_first_0_TP <- df[which(df[["TP"]] == 0)[1], ][["thresh"]]
  df <- df %>%
    filter(thresh %in% c(0:thresh_of_first_0_TP))

  fnfptp_long <- df %>%
    select(any_of(c("governorate_name", "thresh", "FN", "FP", "TP"))) %>%
    pivot_longer(
      cols = c("FN", "FP", "TP"),
      names_to = "class",
      values_to = "class_value"
    )

  metrics_long <- df %>%
    select(any_of(c("governorate_name", "thresh", "precision", "recall", "f1_score"))) %>%
    pivot_longer(cols = c("precision", "recall", "f1_score"), names_to = "metric", values_to = "metric_value")

  # we want to scale plots according to max count values except in the case where FPs are so large
  # in early thresholds ... in those cases let's set the max to 500
  max_count <- ceiling(max(fnfptp_long[["class_value"]], na.rm = T))
  y_scalar <- ifelse(max_count >= 500, 500, max_count)

  # if(pseudo_log){
  # y_breaks = c(seq(0,40,by=10), seq(50,y_scalar,by=50))
  # }
  # if(!pseudo_log){
  # y_breaks= seq(0,y_scalar,10)
  # }

  p1 <- fnfptp_long %>%
    ggplot(aes(x = as.numeric(thresh), y = class_value, color = class, group = class)) +
    geom_line() +
    geom_line(
      data = metrics_long,
      aes(
        x = as.numeric(thresh),
        y = metric_value * y_scalar,
        group = metric,
        color = metric
      )
    ) +
    scale_color_manual(values = c(
      "FN" = "red",
      "f1_score" = "purple",
      "FP" = "orange",
      "recall" = "cyan",
      "precision" = "black",
      "TP" = "green"
    ))
  if (pseudo_log) {
    p2 <- p1 + scale_y_continuous(
      labels = scales::comma,
      # breaks = y_breaks,
      trans = scales::pseudo_log_trans(),
      sec.axis = sec_axis(
        trans = ~ . * (1 / y_scalar),
        breaks = seq(0, 1, .1)
      )
    )
  }
  if (!pseudo_log) {
    p2 <- p1 + scale_y_continuous(
      labels = scales::comma,
      # breaks = y_breaks,
      limits = c(0, y_scalar),
      sec.axis = sec_axis(
        trans = ~ . * (1 / y_scalar),
        breaks = seq(0, 1, .1)
      )
    )
  }

  p2 +
    scale_x_continuous(breaks = seq(0, 300, 10)) +
    labs(x = x_axis_title, y = "Number", title = plot_title) +
    theme_hdx() +
    theme(
      axis.text.x = element_text(angle = 90)
    )
}


summarise_rainfall_impact_to_gov <- function(df) {
  df %>%
    group_by(governorate_name, date) %>%
    summarise(
      across(matches("^precip_.+"), list(max = max, mean = mean)),
      fevent = any(fevent), .groups = "drop"
    )
}






#' plot_impact_by_threshold
#' @description iterate through threshold and plot the % impact at each threshold and above. So far this is only used in 
#'  07_checkpoint_1a_overview.rmd
#' @param impact data.frame containing impact (flood events) and numeric variable which quantify impact (i.e number shelters affected)
#' @param rainfall data.frame containing rainfall statistics for area of interest during the time period of interest
#' @param window \code{integer} for each reported incident we will look at the date +/ this window (in days) to find the maximum rainfall 
#'  event that occured (default = 5)
#' @param by \code{character} should we do the calculations at the overall level (if so by is NULL, this is default) or 
#'  by goernorate ("governorate_name")
#' @param rainfall_val \code{character} name of column containing rainfall values to use (default = "mean")
#' @param impact_val \code{character} name of column contianing impact value to use (default = "num_verified_hhs")
#'
#' @return plot with x-axis = threshold/precip amount (mm) and y axis= % imapct value representing the % impact at each threshold and above.

plot_impact_by_threshold <-  function(impact,
                                     rainfall,
                                     window,
                                     by=NULL,
                                     rainfall_val = "mean",
                                     impact_val = "num_verified_hhs"
){
    

    assertthat::assert_that(is.null(by) || by == "governorate_name",
                msg = "Error: `by` must be NULL or 'governorate_name'")
    
    ylab<- switch(impact_val,
                  "num_shelters_affected"= "% Shelters Affected",
                  "num_verified_hhs"="% Verified HHs Affected")
    
    events_with_max_rainfall<- max_rainfall_around_event(
        impact = impact,
        rainfall = rainfall,
        window=window
    )
    
    event_impact_w_max_rainfall<- cccm_flood_marib_hajjah_impact %>% 
        left_join(events_with_max_rainfall,
                  by = c("governorate_name","date")
        ) 
    
    if(is.null(by)){
        impact_by_thresh_df <- impact_by_thresh(
            df = event_impact_w_max_rainfall,
            val = rainfall_val,
            impact_val = impact_val
        )
        
        p <- impact_by_thresh_df %>% 
            ggplot(aes(x=value,
                       y=pct_impact
            ))+
            geom_line()+
            scale_y_continuous_hdx(labels= scales::percent, limits=c(0,1))+
            labs(x= "threshold (mm)", 
                 y = ylab,
                 title = glue("{ylab} at and above each rainfall threshold (mm)")
            )+
            theme_hdx()
    }
    if(!is.null(by)){
        impact_by_thresh_df <-  event_impact_w_max_rainfall %>% 
            split(.$governorate_name) %>% 
            imap_dfr(\(df_temp,nm ){
                impact_by_thresh(df = df_temp,
                                 val = "mean",
                                 impact_val = "num_verified_hhs") %>% 
                    mutate(governorate_name =nm)
            })
        
        p <- impact_by_thresh_df %>% 
            ggplot(aes(x=value,
                       y=pct_impact,
                       color=governorate_name, 
                       group=governorate_name))+
            geom_line()+
            scale_y_continuous_hdx(labels= scales::percent, limits=c(0,1))+
            labs(x= "threshold (mm)", 
                 y = ylab,
                 title = glue("{ylab} at and above each rainfall threshold (mm)"))+
            theme_hdx()
        
    }
    
    return(p)   
}

#' impact_by_thresh
#' @description iterate through rainfall values and aggregate impact data based how much impact occurred
#'  at that rainfall level or higher. This function is used inside `plot_impact_by_threshold()` which currently 
#'  is only used inside `07_checkpoint_1a_overview.rmd`
#' @param df data.frame containing impact data with rainfall value attached
#' @param val \code{character} name of rainfall value column 
#' @param impact_val \code{character} name of numeric impact column to use 
#'  (i.e num_shelters affected, num_verified_hhs)
#'
#' @return data.frame aggregated by rainfall values. For each rainfall value the impact data
#'  is aggregated based on all events that occured at that rainfall level or higher. The data.frame contains the 
#'  following columns:
#'      value = rainfall threshold (mm)
#'      impact_sum = total impact (i.e num_shelters_affected) by rainfall greater than or equal to value
#'      pct_impact = impact_sum/total impact in db
#'      pct_impact_inv = 1- pct_impact (just helpful for a different view of data.)

impact_by_thresh <- function(
        df, 
        val,
        impact_val){
    
    max_thresh<- max(df[[val]],na.rm=T) %>% ceiling()
    iter_seq <- seq(0, max_thresh,by = 1)
    iter_seq %>% 
        map_dfr(\(val_temp){
            total_impact <- sum(df[[impact_val]],na.rm = T)
            df %>% 
                filter(!!sym(val)>=val_temp) %>% 
                summarise(
                    value =val_temp,
                    impact_sum = sum(!!sym(impact_val),na.rm = T),
                    pct_impact = impact_sum/total_impact,
                    pct_impact_inv = 1- pct_impact
                )
            
        })
}




#' max_rainfall_around_event
#' @description 
#' provide: a.) impact data set of incidents/events and their dates, b.) historical rainfall data set with dates
#' overlapping impact data, a search window in days and get the maximum rainfall within that window around event
#' @param impact data.frame containing events/incidident dates
#' @param rainfall data.frame containing historical rainfal data from the time period of interest
#' @param window window to search on before and after incident dates for max rainfall value in rainfall data set
#'
#' @return data.frame containing the following columns:
#'  `governorate_name`
#'   `date` (corresponding to incident dates)
#'   `mean` (max rainfall value)
#' @details so far only used within `plot_impact_by_threshold()` inside `07_checkpoint_1a_overview.rmd`

max_rainfall_around_event <- function(impact, 
                                      rainfall, 
                                      window = 5
){
    
    rainfall_split <- rainfall %>% 
        split(.$governorate_name)
    
    impact_split <-  impact %>% 
        split(.$governorate_name)
    
    map2(impact_split, rainfall_split,\(impact_temp, rainfall_temp){
        impact_temp$date %>% 
            unique() %>%
            sort() %>% 
            map_dfr(
                \(dt){
                    start_date <- dt-window
                    end_date <- dt+window
                    start_to_end <-  seq(start_date, end_date,by =1)
                    rainfall_temp %>% 
                        mutate(
                            date= ymd(date)
                        ) %>% 
                        group_by(governorate_name) %>% 
                        
                        filter(
                            date %in% start_to_end
                        ) %>% 
                        summarise(
                            date= dt,
                            mean = max(mean,na.rm=T)
                        ) 
                }
            )
    }) %>% 
        bind_rows()
}

