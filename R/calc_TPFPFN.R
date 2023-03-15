calc_TPFPFN <- function(df,
                        x=precip_roll10 ,
                        event=fevent,
                        thresh= 25,
                        look_back = 7,
                        look_ahead=3){
    
    
    x <- df %>% pull({{x}})
    event <- df %>% pull({{event}})
    x_gte <- x >= thresh
    event_idx <-  which(event)
    
    # classify event as TP or FP....
    event_classification <- event_idx %>% 
        map_dfr(
            \(idx){
                if(idx<look_back){
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
    event_classification <- event_classification %>% 
        mutate(
            post_search_end_fp = replace_na(lead(start_idx),length(x)),
            pre_search_start_fp = replace_na(lag(end_idx)+1,0)
        )
    
    # now FNs hmmm
    # well we can remove the search window!
    
    # then to get rid of middle we do 
    # deriv =deltay/delta_x to get % increase
    # we smooth it to remove minor blips
    # then apply conditional
    # if smoothed_deriv>0 |  # smoothed deriv will cancel values up to peak
    #    x_gte | # this will take care of rest until back at thres.
    
    # make lgl FP search vector to include dates idxs to look for FPs 
    # will add F based on below
    FP_search_vector <- rep(T, length(x))
    allowed_search_gte <- FP_search_vector& x_gte 
    
    FP_list<- event_classification %>% 
        pmap(function(...){
            temp_df <- tibble(...)
            TP_search_window<- temp_df$start_idx :temp_df$end_idx 
            post_search_window <- (temp_df$end_idx+1) :(temp_df$post_search_end_fp-1) 
            pre_search_window <- (temp_df$pre_search_start_fp+1) :(temp_df$start_idx-1) 
            allowed_FP_search_window <- c(pre_search_window,post_search_window)
            # if it is in the TP window - make it F
            FP_search_vector[TP_search_window] <- F
            # if in allowed make it true
            FP_search_vector[allowed_FP_search_window] <- T
            # however they must be >= threshold
            FP_search_window <- FP_search_vector& x_gte 
            
            # if post or pre search windows -- should have a look
            if(!(temp_df$end_idx>=temp_df$post_search_end_fp)|temp_df$positive){
                # consecutive values over threshold after event should not be FPs
                post_consecutive_gte <- diff(cumsum(c(0,x_gte[post_search_window])))>0
                FP_search_window[post_search_window] <- !post_consecutive_gte & x_gte
            }
            if(!temp_df$positive){
                FP_search_window <- x_gte
            }
            return(FP_search_window)
        }
        
        )
    
    
    FP_ret_combined<- Reduce('&',FP_list)
    ret <- list()
    ret$FPs <- FP_ret_combined
    ret$event <- event_classification
    
    return(ret)
    
}


calc_TPFPFN2 <- function(df,
                         x="precip_roll10" ,
                         event="fevent",
                         thresh= 25,
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
                if(idx<look_back){
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
    event_classification <- event_classification %>% 
        mutate(
            post_search_end_fp = replace_na(lead(start_idx),length(x)),
            pre_search_start_fp = replace_na(lag(end_idx)+1,0)
        )
    # example for debugging ---------------------------------------------------
    FP_list<- event_classification %>% 
        pmap(function(...){
            
            ec <- tibble(...)
            FP_search_vector <- rep(T, length(x))
            TP_search_window <- ec$start_idx:ec$end_idx
            if(ec$end_idx==length(x)){
                post_search_window <- ec$end_idx
            }else{
                post_search_window <- (ec$end_idx+1) :(ec$post_search_end_fp)     
            }
            
            pre_search_window <- (ec$pre_search_start_fp+1) :(ec$start_idx-1) 
            allowed_FP_search_window <- c(pre_search_window,post_search_window)
            FP_search_vector[TP_search_window] <- F
            
            gte_pre_event <- x_gte[pre_search_window]
            gte_consec_pre_event <- diff(cumsum(c(0,gte_pre_event)))>0
            
            # this code gives me the starts of the runs 
            gte_consec_pre_event_start <- which(diff(c(0, as.integer(gte_consec_pre_event))) == 1)
            FP_search_vector[pre_search_window] <- F
            FP_search_vector[pre_search_window][gte_consec_pre_event_start] <- T
            
            # this is basically the same as above -- maybe this conditionals are unnecessary?
            # ALMOST except for line marked with ***
            if(ec$positive){
                
                # have to combine the TP search window with post search window in the case of TP
                # we want to drop conse
                last_TP_idx<- max(which(x_gte[TP_search_window]))
                TP_search_post <- c(TP_search_window[last_TP_idx]:(min(post_search_window)-1),post_search_window)
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





test_threshold_performance_all_sites <-  function(df,
                                                  x ,
                                                  event,
                                                  look_back = 7,
                                                  look_ahead=3,
                                                  thresholds=NULL){
    sites_level_performance<- df$site_id %>%
        unique() %>%
        map_dfr(
            \(site){
                cat(site,"\n")
            df_site <- df %>%
                filter(site_id ==site)
            
            max_x <- df_site %>%
                pull(x) %>%
                max(na.rm=T)
            max_x <- ceiling(max_x)
            if(is.null(thresholds)){
               thresholds_iter <- seq(0,max_x,by=1) 
            }
            if(!is.null(thresholds)){
                thresholds_iter <- thresholds
            }
            thresholds_iter %>%
                map_dfr(\(thresh_temp){
                    cat(thresh_temp,"\n")
                    stats_temp<- calc_TPFPFN2(df = df_site,
                                              x = x,
                                              event = event,
                                              thresh = thresh_temp)
                    
                    num_FPs <- data.frame(class="FP",n=sum(stats_temp$FPs))
                    num_TPFN <- stats_temp$event %>%
                        count(TPFN) %>%
                        rename(class = "TPFN")
                    bind_rows(num_TPFN,num_FPs) %>%
                        mutate(site_id = site,
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
    site_classification_list<- calc_TPFPFN2(df = df,x = x,event = event,thresh = thresh)
    site_class_simp <- site_classification_list$event %>% 
        select(idx,TPFN)
    df_p <- df %>% 
        mutate(
            FPs=site_classification_list$FPs,
            idx = row_number()
        ) %>% 
        left_join(site_class_simp)
    
    p1 <- plot_site_events(df=df_p,
                           x=x,
                           event = event,
                           thresh=thresh, day_window=day_window)+
        geom_point(data=. %>% filter(FPs))+
        geom_label(data=. %>% filter(!is.na(TPFN)),
                   aes(label=TPFN,y=15)
        )+
        ggtitle(plot_title)
    
    return(p1)
    
    
}




plot_site_events <- function(df, x,event,thresh,day_window){
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
                      filter({{event}}),
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
#                             stats_temp<- calc_TPFPFN2(df = df_temp,
#                                                       x = {{x}},
#                                                       event = {{event}},
#                                                       thresh = thresh)
#                         )
#                 }
#                 
#                 
#                 stats_temp<- calc_TPFPFN2(df = df_temp,
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
#                 stats_temp<- calc_TPFPFN2(df = df_temp,
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
