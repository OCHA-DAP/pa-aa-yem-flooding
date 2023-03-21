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
#' @examples
#' df <- tibble(date = seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day"),
#' precip_roll10 = c(10, 20, 30, 15, 5, 25, 35, 40, 10, 20),
#' fevent = c(0, 0, 1, 0, 0, 1, 0, 1, 0, 0))
#' calc_TPFPFN(df, "precip_roll10", "fevent", thresh = 25, look_back = 3, look_ahead = 2)

# library(targets)
# library(tidyverse)
# library(lubridate)
# tar_load(gov_area_rainfall_impact_tbl)
# df_roi <- gov_area_rainfall_impact_tbl %>%
#     filter(governorate_name %in% c("Hajjah"))
# ck <- calc_TPFPFN(df = df_roi %>% arrange(date),
#             x = "precip_roll10_mean",
#             event = "fevent",
#             look_back = 7,
#             look_ahead = 3,
#             thresh = 30
#             )
# df_roi2 <- df_roi
# df_roi$fp <- ck$FPs
# df_roi %>% filter(fp) %>% glimpse()
# # plot_site_events(df = df_roi,
# #                  x = "precip_roll10_mean",
# #                  event = 'fevent',
# #                  thresh = 50,
# #                  day_window = 60)
# 
# # # 
# plot_site_events_classified(df = df_roi %>%
#                                 arrange(date),
#                             x = "precip_roll10_mean",
#                             event = "fevent",
#                             thresh = 30,
#                             day_window = 30,plot_title = "test")

calc_TPFPFN <- function(df,
                         x="precip_roll10" ,
                         event="fevent",
                         thresh= 10,
                         look_back = 7,
                         look_ahead=3
                        
                         ){
    
    # x <- df$precip_roll10
    # event <- df$fevent
    x <- df %>% pull(x)
    event <- df %>% pull(event)
    x_gte <- x >= thresh
    event_idx <-  which(event)
    
    # classify event as TP or FP....
    event_classification <- event_idx %>% 
        map_dfr(
            \(idx){
                if(idx<=look_back){
                    look_back <- (idx-1)
                }
                if((idx+look_ahead)>=length(x)){
                    look_ahead <- length(x)-idx
                }
                search_window<- (idx-look_back):(idx+look_ahead)
                thresh_in_window<- any(x[search_window]>=thresh)
                tibble(idx= idx,
                       start_idx=idx-look_back,
                       end_idx= idx+look_ahead,
                       positive=thresh_in_window,
                       TPFN = ifelse(thresh_in_window,"TP","FN")
                )  
            }
        )
    # 
    # event_classificaion <- event_classification %>%
    #     mutate(
    #         overlap_lead_tmp =lead(start_idx)<=end_idx & positive==T,
    #         overlap_lag_tmp = lag(end_idx)>= start_idx & positive==T,
    #         overlap = ifelse(!is.na(overlap_lead_tmp),overlap_lead_tmp, overlap_lag_tmp),
    #     ) %>%
    #     select(-ends_with("_tmp") )
     tp_idx<- event_classification$idx[which(event_classification$positive)]
     
     # if more -- than 1 TP we can check for overlapping dates-- if previ
     if(length(tp_idx)>1){
         # Split the list based on the specified difference
         
         tp_split_indices <- c(1, which(diff(tp_idx) > (look_back+look_ahead)) + 1, length(tp_idx) + 1)
         # tp_split_indices <- c(1, which(diff(tp_idx) > look_back) , length(tp_idx)+1)
         tp_idx_lists <- split(tp_idx, cumsum(seq_along(tp_idx) %in% tp_split_indices))
         
         # it would be simpler if we could just take the min of each of the runs.... can we? 
         # I guess the value could fluctuate below the thresh inbetween, but i think that's fine
         tp_idx_keep <- tp_idx_lists %>% 
             map_int(~min(.x))
         
         # tp_idx_keep <- tp_idx_lists %>% 
         #     map_int(\(tp_run){
         #         tp_range<- min(tp_run):max(tp_run)
         #         if(length(tp_range)>1){
         #             tp_range_gte <- x_gte[tp_range]
         #             tp_gte_consec <- diff(cumsum(c(0,tp_range_gte)))>0
         #             tp_gte_consec_starts <- which(diff(c(0, as.integer(tp_gte_consec))) == 1)
         #             # the problem is that the above might land on idx between events since we expanded range 
         #             # to include additional values
         #             tp_diff <- tp_run-tp_range[tp_gte_consec_starts]
         #             tp_diff_gte0<- tp_diff[tp_diff>=0 ]
         #             min_tp<- min(tp_diff_gte0)
         #             tp_start_idx <- tp_run[min_tp]    
         #         }
         #         if(length(tp_range)==1){
         #             tp_start_idx <- tp_run
         #         }
         #         
         #         return(tp_start_idx)
         #     })
         event_classification <- event_classification %>% 
             filter(!positive|
                        (positive & idx %in% tp_idx_keep)
             )
         
     }
     
    

  
    
    event_classification <- event_classification %>% 
        mutate(
            post_search_end_fp = replace_na(lead(start_idx),length(x)),
            pre_search_start_fp = replace_na(lag(end_idx)+1,0)
        )
    
    FP_list<- event_classification %>% 
        # filter(positive) %>% 
        pmap(function(...){
            # iterate through each row of the event classification table
            # here we define the windows to be excluded from FP consideration
            ec <- tibble(...)
            # make all T --- will make them F based on conditions
            FP_search_vector <- rep(T, length(x))
            # based on user defined look_ahead/look_back
            TP_search_window <- ec$start_idx:ec$end_idx
            
            # allowed_FP_search_window <- c(pre_search_window,post_search_window)
            # can't look in TP window
            FP_search_vector[TP_search_window] <- F
            
            # after event we define the window to look for FPs 
            # it starts from the end_idx + 1 (if end_idx is at end of vector, theres no window)
            if(ec$end_idx==length(x)){
                post_search_window <- ec$end_idx
            }
            else{
                # if this is backwards it should also be null!?
                post_search_window <- (ec$end_idx+1) :(ec$post_search_end_fp)     
                
            }
            # pre search window -- where to look for FPs before event
            pre_search_window <- (ec$pre_search_start_fp+1) :(ec$start_idx-1)     
            
            gte_pre_event <- x_gte[pre_search_window]
            gte_consec_pre_event <- diff(cumsum(c(0,gte_pre_event)))>0
            
            # this code gives me the starts of the runs 
            gte_consec_pre_event_start <- which(diff(c(0, as.integer(gte_consec_pre_event))) == 1)
            FP_search_vector[pre_search_window] <- F
            FP_search_vector[pre_search_window][gte_consec_pre_event_start] <- T
            
            # this is basically the same as above -- maybe this conditionals are unnecessary?
            # ALMOST except for line marked with ***
            if(ec$positive){
                
                # define window where we can look for TP after a TP
                # get end of current TP window
                last_TP_idx<- max(which(x_gte[TP_search_window]))
                
                # if post search window is NULL it means that there are overlapping pre- searches from consecutive events
                if(!is.null(post_search_window)){
                    TP_search_post <- c(TP_search_window[last_TP_idx]:(min(post_search_window)-1),post_search_window)    
                }
                if(is.null(post_search_window)){
                    TP_search_post <- NULL    
                }
                gte_post_TP <- x_gte[TP_search_post]
                gte_consec_post_TP <- diff(cumsum(c(0,gte_post_TP)))>0
                gte_consec_post_event_start <- which(diff(c(0, as.integer(gte_consec_post_TP))) == 1)
                #****
                gte_consec_post_event_start<- gte_consec_post_event_start[-1]
                FP_search_vector[TP_search_post] <- F
                FP_search_vector[TP_search_post][gte_consec_post_event_start] <- T
                # FP_search_vector[post_search_window] <- !gte_consec_post_TP & x_gte[post_search_window]
            }
            if(!ec$positive){
                gte_post_event <- x_gte[post_search_window]
                gte_consec_post_event <- diff(cumsum(c(0,gte_post_event)))>0
                gte_consec_post_event_start <- which(diff(c(0, as.integer(gte_consec_post_event))) == 1)
                FP_search_vector[post_search_window] <- F
                FP_search_vector[post_search_window][gte_consec_post_event_start] <- T
            }
            return(FP_search_vector)
        }
        )
    
    FP_ret_combined<- Reduce('&',FP_list)

    
    ret <- list()
    ret$FPs <- FP_ret_combined
    ret$event <- event_classification
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
#' @examples
# df <- data.frame(precipitation = rnorm(100),
#                  fevent = as.logical(rbinom(100, 1, 0.5)),
#                  admin1 = rep(c("A", "B", "C", "D"), each = 25))
# df %>% 
#     filter(admin1=="C")
# calc_TPFPFN(df = df %>% 
#                 filter(admin1=="C"),
#             x = "precipitation",
#             event = "fevent",thresh = 25)

#'
#' # Calculate performance frequencies by threshold, grouped by admin1
# performance_frequencies_by_threshold(df, x = "precipitation",
#                                      by = "admin1",
#                                      event = "fevent",
#                                      thresholds = seq(0,100, by =1))
#'
#'


performance_frequencies_by_threshold <-  function(df,
                                             x ,
                                             by="governorate_name",
                                             event,
                                             look_back = 7,
                                             look_ahead=3,
                                             thresholds=NULL){
    performance<- df[[by]] %>%
        unique() %>%
        map_dfr(
            \(loc){
                cat(loc,"\n")
            df_loc <- df %>%
                filter(!!sym(by) ==loc)
            
            thresholds %>%
                map_dfr(\(thresh_temp){
                    cat(thresh_temp,"\n")
                    stats_temp<- calc_TPFPFN(df = df_loc,
                                              x = x,
                                              event = event,
                                              thresh = thresh_temp)
                    
                    num_FPs <- data.frame(class="FP",n=sum(stats_temp$FPs))
                    num_TPFN <- stats_temp$event %>%
                        count(TPFN) %>%
                        rename(class = "TPFN")
                    bind_rows(num_TPFN,num_FPs) %>%
                        mutate(!!sym(by) := loc,
                               thresh=thresh_temp)
                })
        })
}




plot_site_events_classified <- function(df, 
                                        x,
                                        event,
                                        thresh=thresh,
                                        day_window,
                                        plot_title){
    site_classification_list<- calc_TPFPFN(df = df,
                                           x = x,
                                           event = event,
                                           thresh = thresh)
    assertthat::assert_that("FPs" %in% names(site_classification_list),msg = "stop wheres FPs")
    
    site_class_simp <- site_classification_list$event %>% 
        select(idx,TPFN)
    
    tp_idx <- site_class_simp$idx[site_class_simp$TPFN=="TP"]
    tp_grp_date <- df$date[tp_idx]
    df_p <- df %>% 
        mutate(
            FPs=site_classification_list$FPs,
            idx = row_number()
        ) %>% 
        left_join(site_class_simp, by="idx")
    
    all_events_plot<- df_p[[event]]
    
    p_ts <- plot_site_events(df=df_p,
                           x=x,
                           event = event,
                           thresh=thresh, day_window=day_window)+
    
        geom_point(data=. %>% filter(FPs))+
        geom_label(data=. %>% filter(!is.na(TPFN)),
                   aes(label=TPFN,y=15)
        )+
        ggtitle(plot_title)
    if(length(all_events_plot)>1){
       p_ts <-  p_ts+
            geom_vline(xintercept= tp_grp_date,color="black",lwd=2)
    }
        
        
    
    return(p_ts)
    
    
}




plot_site_events <- function(df,
                             x,
                             event,
                             thresh,
                             day_window){
    event_str <- tidyselect::vars_pull(names(df), !!enquo(event))
    x_str <- tidyselect::vars_pull(names(df), !!enquo(x))
    
    event_dates <- df %>% 
        filter(!!sym(event_str)) %>% 
        pull(date)
    date_range <- range(event_dates)
    min_date <- date_range[1]-day_window
    max_date <-  date_range[2]+day_window
    df_plot <- df %>% 
        filter(date>=min_date,date<=max_date)
    col_grid <- rgb(235, 235, 235, 70, maxColorValue = 255)
    event_label_y_pos <- max(df_plot %>% pull(x),na.rm=T)*.75
    df_plot %>% 
        ggplot(aes(x= date, y=!!sym(x_str))) +
        scale_x_date(date_breaks =  "5 days",
                     date_labels = "%b %d",
                     date_minor_breaks = "1 day")+
        scale_y_continuous(breaks=seq(0,90, 5))+
        geom_line()+
        # geom_point(data=. %>% filter(idx_test))+
        # geom_label(data=. %>% filter(idx_test),aes(label=idx))+
        geom_vline(data=. %>% 
                       filter(!!sym(event_str)),
                   aes(xintercept=date), 
                   color="red")+
        geom_hline(aes(yintercept = thresh), linetype ="dashed", color="blue")+
        # geom_text(aes(x=ymd("2022-08-20"), y=17,label ="threshold a: 15 mm"))+
        # geom_text(aes(x=ymd("2022-08-20"), y=27,label ="threshold b: 25 mm"))+
        geom_text(data=. %>% 
                      filter(!!sym(event)),
                  aes(x=ymd(date)-2, 
                      y=event_label_y_pos,
                      label ="Reported event"),angle = 90)+
        # theme_hdx()+
        labs(y= "10 day rain accumulation (mm)")+
        theme_hdx()+
        theme(
            panel.grid.minor.x =element_line(color =col_grid), 
            panel.grid.major.x =element_line(color = "darkgrey"), 
            panel.grid.minor.y = element_line(),
            panel.grid.major.y = element_line(),
            axis.text.x = element_text(angle=90)
        )
    
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
merge_rainfall_cccm_impact <-  function(site_rainfall, site_flooding){
    site_lookup <- site_flooding %>% 
        distinct(governorate_name,site_id, site_name)
    
    site_report_date_range <-  site_flooding$date_of_episode %>% range(na.rm=T)
    
    rainfall_time_constrained <- site_rainfall %>%
        mutate(date= ymd(date)) %>% 
        filter(date>=site_report_date_range[1],
               date<=site_report_date_range[2]) %>% 
        mutate(
            fevent= paste0(site_id,date)%in% paste0(site_flooding$site_id,
                                                    site_flooding$date_of_episode)
        ) %>% 
        left_join(site_lookup)
    return(rainfall_time_constrained)
    
    
}




# site level performance stats
#' Title
#'
#' @param site_rainfall 
#' @param site_flooding 
#' @param x 
#' @param event 
#' @param thresh 
#' @param day_window 
#'
#' @return
#' @export
#'
#' @examples
#' 
# library(lubridate)
# ck <- site_level_performance(site_rainfall=cccm_site_chirp_stats, 
#                              site_flooding=cccm_flood_impact_data,
#                              x=precip_roll10,
#                              event = fevent,
#                              thresh=25
# )
# site_level_performance <- function(df,
#                                    x=precip_roll10,
#                                    event = fevent,
#                                    site_id ="site_id",
#                                    thresh=25
#                                    
# ){
#     
#     
#     
#     
#     sites_level_performance<- df[[site_id]] %>% 
#         unique() %>% 
#         map_dfr(
#             ~{
#                 df_temp <- df %>% 
#                     filter(site_id ==.x)
#                 gov_name <- unique(df_temp$governorate_name)
#                 
#                 if(iterate_thresholds){
#                     max_x <- df_temp %>% 
#                     pull({{x}}) %>% 
#                     max(na.rm=T)    
#                     max_x <- ceiling(max_x)
#                     
#                     seq(0,max_x,by=1) %>% 
#                         map(
#                             stats_temp<- calc_TPFPFN(df = df_temp,
#                                                       x = {{x}},
#                                                       event = {{event}},
#                                                       thresh = thresh)
#                         )
#                 }
#                 
#                 
#                 stats_temp<- calc_TPFPFN(df = df_temp,
#                                           x = {{x}},
#                                           event = {{event}},
#                                           thresh = thresh)
#                 num_FPs <- data.frame(class="FP",n=sum(stats_temp$FPs))
#                 num_TPFN <- stats_temp$event %>%
#                     count(TPFN) %>% 
#                     rename(class = "TPFN")
#                 bind_rows(num_TPFN,num_FPs) %>% 
#                     mutate(site_id = .x,
#                            governorate_name = gov_name)
#                 
#             }
#         )
#     return(sites_level_performance)
#     
# }
# iterate_site_perf <- function(df){
#     
#     
# }
# site_classifcation_freq_tbl <-  function(){
#    
#                 gov_name <- unique(df_temp$governorate_name)
#                 
#                 
#                 stats_temp<- calc_TPFPFN(df = df_temp,
#                                           x = {{x}},
#                                           event = {{event}},
#                                           thresh = thresh)
#                 num_FPs <- data.frame(class="FP",n=sum(stats_temp$FPs))
#                 num_TPFN <- stats_temp$event %>%
#                     count(TPFN) %>% 
#                     rename(class = "TPFN")
#                 bind_rows(num_TPFN,num_FPs) %>% 
#                     mutate(site_id = .x,
#                            governorate_name = gov_name)
#                 
#             }
#         )
#     
# }
# 
# merge_rainfall_cccm_impact(site_rainfall =cccm_site_chirp_stats,
#                            site_flooding = cccm_flood_impact_data)
# 
# loop_thresholds <- function(){
#     rain_impact_merged <- merge_rainfall_cccm_impact(site_rainfall =site_rainfall,
#                                                      site_flooding = site_flooding)
#     
#     
# } site_level_performance(site_rainfall=cccm_site_chirp_stats, 
#                                            site_flooding=cccm_flood_impact_data,
#                                            x=precip_roll10,
#                                            event = fevent,
#                                            thresh=.x
# ) %>% 
#     mutate(thresh =.x)
# }
# 

