#' calc_TPFPFN
#' @description provided a data.frame with sequential values and associated events, classify records as either True Positive (TP)
#'     True Negative (TN), or False Negative (FN) based on user-specified value threshold
#' @param df data.frame containing sequential numeric values and logical value indicating occurence of event of interest
#' @param x \code{character} Column name for precipitation data.
#' @param event \code{character} Column name for event data
#' @param thresh \code{integer} threshold value to use for classification
#' @param look_back \code{integer} number of days to look back from date of event
#' @param look_ahead \code{integer} number of days to look ahead from date of event
#'
#' @return list containing a vector of FPs, and a data.frame containing the TP and FN classifications
#'
#' @examples \dontrun{
#' library(targets)
#' library(tidvyerse)
#' tar_source()
#' df <- data.frame(
#'   precipitation = rnorm(100),
#'   fevent = as.logical(rbinom(100, 1, 0.5)),
#'   admin1 = rep(c("A", "B", "C", "D"), each = 25)
#' )
#' calc_TPFPFN(
#'   df = df %>%
#'     filter(admin1 == "C"),
#'   x = "precipitation", look_back = 1, look_ahead = 2,
#'   event = "fevent", thresh = 1
#' )
#' }
calc_TPFPFN <- function(df,
                        x = "precip_roll10",
                        event = "fevent",
                        thresh = 10,
                        look_back = 7,
                        look_ahead = 3) {
  x <- df %>% pull(x)
  event <- df %>% pull(event)
  full_idx <- seq(1, length(x), by = 1)
  x_gte <- x >= thresh
  event_idx <- which(event)
  # classify event as TP or FP....
  event_classification <- event_idx %>%
    map_dfr(
      \(idx){
        if (idx <= look_back) {
          look_back <- (idx - 1)
        }
        if ((idx + look_ahead) >= length(x)) {
          look_ahead <- length(x) - idx
        }
        search_window <- (idx - look_back):(idx + look_ahead)
        thresh_in_window <- any(x[search_window] >= thresh)

        end_idx <- idx + look_ahead
        start_idx <- idx - look_back

        if (thresh_in_window) {
          # might be good to put this sequence in it's own `helper` function as it is
          # highly confusing and is reused with modification in other parts of this project
          # ... but to explain:

          # 1. get all values after TP classification gte threshold
          gte_after_TP_idx <- which(x_gte & (full_idx >= end_idx))

          # 2. we only want the first run of these values - so this finds break points
          # where consecutive values stop being gte threshold
          consec_gte_after_TP_idx_split_pts <- c(
            1, which(diff(gte_after_TP_idx) > (1)) + 1,
            length(gte_after_TP_idx) + 1
          )
          # then split the runs - we are only intersted in the first one after the TP of interest
          consec_gte_after_TP_idx_list <- split(
            gte_after_TP_idx,
            cumsum(seq_along(gte_after_TP_idx) %in%
              consec_gte_after_TP_idx_split_pts)
          )

          # if we have these consecutive runs - get the first one
          # we also want to exclude the the TP search window (start_idx:end_idx)
          if (length(consec_gte_after_TP_idx_list) > 0) {
            idx_exclude <- unique(c(consec_gte_after_TP_idx_list$`1`, start_idx:end_idx))
          }
          # if no consecutive runs - just exclude TP search window
          if (length(consec_gte_after_TP_idx_list) == 0) {
            idx_exclude <- start_idx:end_idx
          }
        }
        # if no TPs we still want exclude TP search window
        if (!thresh_in_window) {
          idx_exclude <- start_idx:end_idx
        }

        event_df <- tibble(
          idx = idx,
          start_idx = start_idx,
          end_idx = end_idx,
          positive = thresh_in_window,
          TPFN = ifelse(thresh_in_window, "TP", "FN"),
          idx_exclude = list(idx_exclude)
        )
      }
    )
  # need to wipe out possibility of FP after TP when consecute gte threshold


  idx_exclude_from_fp_search <- event_classification %>%
    select(idx_exclude) %>%
    unnest_longer(idx_exclude) %>%
    pull(idx_exclude) %>%
    unique()

  # make full T vec to flip off
  fp_search_lgl <- rep(T, length(full_idx))
  fp_search_lgl[idx_exclude_from_fp_search] <- F # flip off to exclude
  x_gte_in_range <- x_gte & fp_search_lgl
  consec_runs <- diff(cumsum(c(0, x_gte_in_range))) > 0

  # this code gives me the starts of the runs
  consec_run_starts <- which(diff(c(0, as.integer(consec_runs))) == 1)
  # now flip it all FALSE and put run starts as true
  fp_search_lgl <- rep(F, length(full_idx))
  fp_search_lgl[consec_run_starts] <- T


  # a little condition on the TPs
  tp_idx <- event_classification$idx[which(event_classification$positive)]
  # if more -- than 1 TP classification within look_back + look_ahead window of eachother - just take the first one.
  if (length(tp_idx) > 1) {
    # Split the list based on the specified difference
    tp_split_indices <- c(1, which(diff(tp_idx) > (look_back + look_ahead)) + 1, length(tp_idx) + 1)
    # tp_split_indices <- c(1, which(diff(tp_idx) > look_back) , length(tp_idx)+1)
    tp_idx_lists <- split(tp_idx, cumsum(seq_along(tp_idx) %in% tp_split_indices))

    # it would be simpler if we could just take the min of each of the runs.... can we?
    # I guess the value could fluctuate below the thresh inbetween, but i think that's fine
    tp_idx_keep <- tp_idx_lists %>%
      map_int(~ min(.x))
    event_classification <- event_classification %>%
      filter(!positive |
        (positive & idx %in% tp_idx_keep))
  }
  ret <- list()
  ret$FPs <- fp_search_lgl
  ret$event_classification <- event_classification
  return(ret)
}




#' Calculate performance frequencies by threshold
#'
#' This function calculates the performance frequencies of a given event by threshold for each group in a specified column of a dataframe.
#'   It uses the calc_TPFPFN function to calculate the true positives, false positives, and false negatives for a given event and threshold.
#'   The output is a dataframe with columns for the specified group, threshold, and class (TP, FN, or FP) and the number of occurrences for each class.
#'
#' @param df A dataframe containing the data to be analyzed
#' @param x The column containing the data to be analyzed
#' @param by The name of the column containing the groups to be analyzed (default = "governorate_name")
#' @param event The name of the event to be analyzed
#' @param look_back The number of days to look back for the event (default = 7)
#' @param look_ahead The number of days to look ahead for the event (default = 3)
#' @param thresholds A vector of threshold values to be used in the analysis
#'
#' @return A dataframe with columns for the specified group, threshold, and class (TP, FN, or FP) and the number of occurrences for each class.
#'
#' @examples \dontrun{
#' library(targets)
#' library(tidvyerse)
#' tar_source()
#' df <- data.frame(
#'   precipitation = rnorm(100),
#'   fevent = as.logical(rbinom(100, 1, 0.5)),
#'   admin1 = rep(c("A", "B", "C", "D"), each = 25)
#' )
#' calc_TPFPFN(
#'   df = df %>%
#'     filter(admin1 == "C"),
#'   x = "precipitation", look_back = 1, look_ahead = 2,
#'   event = "fevent", thresh = 1
#' )
#'
#' # Calculate performance frequencies by threshold, grouped by admin1
#' ck <- performance_frequencies_by_threshold(
#'   df,
#'   x = "precipitation",
#'   by = "admin1",
#'   event = "fevent",
#'   thresholds = seq(0, 100, by = 1)
#' )
#' tar_load(rainfall_impact_tbl)
#' max_rainfall <- ceiling(max(rainfall_impact_tbl$precip_roll10, na.rm = T))
#' pred_class_freq_tbl <- performance_frequencies_by_threshold(
#'   df = rainfall_impact_tbl,
#'   x = "precip_roll10",
#'   by = "site_id",
#'   event = "fevent",
#'   look_back = 7,
#'   look_ahead = 3,
#'   thresholds = seq(0, max_rainfall, 1)
#' )
#' }
performance_frequencies_by_threshold <- function(df,
                                                 x,
                                                 by = "governorate_name",
                                                 event,
                                                 look_back = 7,
                                                 look_ahead = 3,
                                                 thresholds = NULL) {
  performance <- df[[by]] %>%
    unique() %>%
    map_dfr(
      \(loc){
        cat(loc, "\n")
        df_loc <- df %>%
          filter(!!sym(by) == loc)

        thresholds %>%
          map_dfr(\(thresh_temp){
            cat(thresh_temp, "\n")
            stats_temp <- calc_TPFPFN(
              df = df_loc,
              x = x,
              event = event,
              thresh = thresh_temp
            )

            num_FPs <- data.frame(class = "FP", n = sum(stats_temp$FPs))
            num_TPFN <- stats_temp$event %>%
              count(TPFN) %>%
              rename(class = "TPFN")
            bind_rows(num_TPFN, num_FPs) %>%
              mutate(!!sym(by) := loc,
                thresh = thresh_temp
              )
          })
      }
    )
}




plot_site_events_classified <- function(df,
                                        x,
                                        event,
                                        thresh = thresh,
                                        day_window,
                                        plot_title,
                                        just_events= F,
                                        mark_first = T
                                            ) {
  site_classification_list <- calc_TPFPFN(
    df = df,
    x = x,
    event = event,
    thresh = thresh
  )
  assertthat::assert_that("FPs" %in% names(site_classification_list), msg = "stop wheres FPs")

  site_class_simp <- site_classification_list$event %>%
    select(idx, TPFN)

  tp_idx <- site_class_simp$idx[site_class_simp$TPFN == "TP"]
  tp_grp_date <- df$date[tp_idx]
  df_p <- df %>%
    mutate(
      FPs = site_classification_list$FPs,
      idx = row_number()
    ) %>%
    left_join(site_class_simp, by = "idx")

  all_events_plot <- df_p[[event]]

  p_ts <- plot_site_events(
      df = df_p,
      x = x,
      event = event,
      thresh = thresh, 
      day_window = day_window
  ) +
      ggtitle(plot_title)
  if(!just_events){
      p_ts <- p_ts+
          geom_point(data = . %>% filter(FPs)) +
          geom_label(
              data = . %>% filter(!is.na(TPFN)),
              aes(label = TPFN, y = 15)
          )  
  }
 
  if (length(all_events_plot) > 1 & mark_first) {
    p_ts <- p_ts +
      geom_vline(xintercept = tp_grp_date, color = "black", linewidth = 2)
  }



  return(p_ts)
}




plot_site_events <- function(df,
                             x,
                             event,
                             thresh,
                             day_window) {
  event_str <- tidyselect::vars_pull(names(df), !!enquo(event))
  x_str <- tidyselect::vars_pull(names(df), !!enquo(x))

  event_dates <- df %>%
    filter(!!sym(event_str)) %>%
    pull(date)
  date_range <- range(event_dates)
  min_date <- date_range[1] - day_window
  max_date <- date_range[2] + day_window
  df_plot <- df %>%
    filter(date >= min_date, date <= max_date)
  col_grid <- rgb(235, 235, 235, 70, maxColorValue = 255)
  event_label_y_pos <- max(df_plot %>% pull(x), na.rm = T) * .75
  
  p <- df_plot %>%
    ggplot(aes(x = date, y = !!sym(x_str))) +
    scale_x_date(
      date_breaks = "5 days",
      date_labels = "%b %d",
      date_minor_breaks = "1 day"
    ) +
    scale_y_continuous(breaks = seq(0, 90, 5)) +
    geom_line() +
    # geom_point(data=. %>% filter(idx_test))+
    # geom_label(data=. %>% filter(idx_test),aes(label=idx))+
    geom_vline(
      data = . %>%
        filter(!!sym(event_str)),
      aes(xintercept = date),
      color = "red"
    ) +
    # geom_text(
    #   data = . %>%
    #     filter(!!sym(event)),
    #   aes(
    #     x = ymd(date) - 2,
    #     y = event_label_y_pos,
    #     label = "Reported event"
    #   ), angle = 90
    # ) +
    labs(y = "10 day rain accumulation (mm)",subtitle= "vertical red lines represent reported flood event dates") +
    theme_hdx() +
    theme(
        axis.title.x = element_blank(),
      panel.grid.minor.x = element_line(color = col_grid),
      panel.grid.major.x = element_line(color = "darkgrey"),
      panel.grid.minor.y = element_line(),
      panel.grid.major.y = element_line(),
      axis.text.x = element_text(angle = 90)
    )
  if(!is.null(thresh)){
      p <- p+
          geom_hline(aes(yintercept = thresh), linetype = "dashed", color = "blue") 
  }
  return(p)
}


#' merge_rainfall_cccm_impact
#'
#' @param site_rainfall
#' @param site_flooding
#'
#' @return site_rainfall data.frame filtered to the time range of ccccm and fevent logical column
#'  indicating if a flood occured at the site and date specified in each row.
#' @export
#'
#' @examples
merge_rainfall_cccm_impact <- function(site_rainfall, site_flooding) {
  site_lookup <- site_flooding %>%
    distinct(governorate_name, site_id, site_name)

  site_report_date_range <- site_flooding$date_of_episode %>% range(na.rm = T)

  rainfall_time_constrained <- site_rainfall %>%
    mutate(date = ymd(date)) %>%
    filter(
      date >= site_report_date_range[1],
      date <= site_report_date_range[2]
    ) %>%
    mutate(
      fevent = paste0(site_id, date) %in% paste0(
        site_flooding$site_id,
        site_flooding$date_of_episode
      )
    ) %>%
    left_join(site_lookup)
  return(rainfall_time_constrained)
}
