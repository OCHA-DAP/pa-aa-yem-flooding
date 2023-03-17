
#' Title
#'
#' @param x \code{numeric} vector containing values to be evaluated 
#' @param y \code{logical} vector containing logical whether or not flooding was reported
#' @param thresh \code{numeric}
#'
#' @return
#' @export
#'

df <- tibble(
    num = c(3,5,10,11,12,10,9,11,12,13,7,3,5,3,2,1,0,7,10,11,12,13,14,20,21,22) ,
    event = c(F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F)
)

 df %>%
     mutate(
       include = consecutive_inclusion(x = num,
                                       y=event, 
                                       thresh = 10,
                                       max_consec_days = 5)
    ) %>% 
     print(n=nrow(.))
 

consecutive_inclusion <- function(x,
                                  y,
                                  thresh=20,
                                  max_consec_days=NULL
){
    x_gte <- x >= thresh
    x_gte_event <-  x_gte & y # TPs
    event_df<- tibble(
        x,
        y,
        x_gte = x_gte,
        x_gte_event= x_gte_event,
        event = ifelse(x_gte_event,paste0("event_",y+row_number()-1),NA)
    )
    event_df <- event_df %>% 
        tidyr::fill(event) 
    
    event_df_incl <- event_df %>% 
        group_by(event, .add = T) %>% 
        mutate(
            x_gte_roll=rollsum(x = x_gte,k = 2,align = 'right',fill = 0) ,
            x_incl = case_when( 
                # we want to include the date of the event
                x_gte_event ~ TRUE,
                # but not include dates after event that are gte thresh
                any(x_gte_event) & (x_gte_event + x_gte_roll)>1 ~ FALSE,
                TRUE ~ TRUE
            )
        )
    
    if(!is.null(max_consec_days)){
        event_df_incl <- event_df_incl %>% 
            mutate(
                x_rm =  !(x_incl),
                # roll count values to be removed up to day + 1. At day +1 if the rollsum is > max days
                # that means we have passed the maximum # consecutive days and should start including
                x_rm_count = rollsum(x= x_rm,k=max_consec_days+1, align="right", fill=0),
                
                gt_consec = x_rm_count > max_consec_days,
                x_incl = case_when(
                    gt_consec ~ T,
                    TRUE ~ x_incl
                )
            )
            
    }
    return(
        event_df_incl %>% 
               ungroup() %>%  
               pull(x_incl)
           )

}





rain_impact_harm <- merge_rainfall_cccm_impact(site_rainfall =cccm_site_chirp_stats,
                                               site_flooding = cccm_flood_impact_data)
rain_impact_harm_simp <- rain_impact_harm %>% 
    select(site_id, date, precip_roll10, fevent)
rain_single_site <- rain_impact_harm_simp %>% 
    filter(site_id =="YE1118_0053")

rain_impact_harm_simp %>% 
    mutate(row_id= row_number() ) %>%
    group_by(site_id) %>% 
    arrange(date) %>% 
    mutate(
        include = consecutive_inclusion(x = precip_roll10,
                                        y= fevent, 
                                        thresh = 20, 
                                        max_consec_days = 5)
    ) %>% ungroup() %>% 
    # filter(precip_roll10>=20)
    # filter(fevent &precip_roll10>=20)
    filter(row_id %in% c(59502:59522)) %>% 
    print(n=nrow(.))
    filter(precip_roll10>=20)
    
library(furrr)
future::plan(multiprocess)
how_we_do <- threshold_classification_table2(df = rain_impact_harm_simp, by=NULL,exclude="all_consec_above")

threshold_classification_table2 <- function(df,
                                            by= NULL,
                                            exclude="all_consec_above"
){
  
    
    rainfall_event_long <- df %>% 
        pivot_longer(starts_with("precip_")) %>% 
        left_join(site_lookup,by="site_id")
    
    rainfall_event_long_split <-  split(rainfall_event_long,rainfall_event_long$name)
    
    classification_tbl <- rainfall_event_long_split %>% 
        
        map2(names(rainfall_event_long_split),
             \(window_df,nm){
                 iterate_to <- round(max(window_df$value,na.rm=T)+5,0)
                 cat(crayon::green(nm),"\n")
                 
                     c(1:iterate_to) %>% 
                         map_dfr(
                             \(thresh){
                                 if(exclude=="all_consec_above"){
                                     window_df <-  window_df %>% 
                                         group_by(site_id) %>% 
                                         arrange(site_id, date) %>% 
                                         mutate(
                                             # create logical col indicating how to include
                                             # values after TP
                                             include= consecutive_inclusion(x=value,
                                                                            y=fevent,
                                                                            thresh = thresh)
                                         ) %>% 
                                         ungroup()
                                     
                                     excluded <- window_df %>% 
                                         filter(!include) %>% 
                                         nrow()
                                     
                                     cat(excluded," records excluded\n")
                                     # keep only T
                                     window_df <- window_df %>% 
                                         filter(include)
                                     
                                 }
                                 events_classified <- window_df %>% 
                                     mutate(
                                         type = case_when(
                                             value>=thresh & fevent ~ "TP",
                                             value>= thresh & !fevent ~"FP",
                                             value<thresh & fevent ~"FN",
                                             value< thresh & !fevent ~"TN"
                                         ),
                                         type= fct_expand(type, c("TP","FP","FN","TN"))
                                     ) 
                                 if(!is.null(by)){
                                     events_classified <-  events_classified %>%
                                         group_by(type,{{by}},.drop = F)
                                 }
                                 if(is.null(by)){
                                     events_classified <-  events_classified %>%
                                         group_by(type,.drop = F)
                                 }
                                 
                                 events_classified_summarised <- events_classified %>% 
                                     summarise(
                                         n=n(),
                                         .groups="drop"
                                     ) 
                                 events_classified_summarised %>% 
                                     pivot_wider(names_from = type, values_from = n) %>% 
                                     mutate(
                                         accuracy = (TP+TN)/(TP+FP+TN+FN),
                                         precision = TP/(TP+FP),
                                         recall = TP/(TP+FN),
                                         TPR = recall,
                                         POD= recall,
                                         FAR = 1- precision,
                                         FPR=FP/(FP+TN),
                                         thresh=thresh,
                                         records_excluded = ifelse(is.null(exclude),0,excluded)
                                     )
                             }
                         )
  
             }
        )
    return(classification_tbl)
}


#tar_load(cccm_site_chirp_stats)
#tar_load(cccm_flood_impact_data)
rain_impact_harm <- merge_rainfall_cccm_impact(site_rainfall =cccm_site_chirp_stats,
                                               site_flooding = cccm_flood_impact_data)
rain_impact_harm_simp <- rain_impact_harm %>% 
    select(site_id, date, precip_roll10, fevent)
rain_single_site <- rain_impact_harm_simp %>% 
    filter(site_id =="YE1118_0053") %>% 
    arrange(date)

rain_impact_harm_simp %>% 
    filter(site_id=="YE1114_2749") %>% 
    plot_site_events_classified(x = precip_roll10,event = fevent,thresh = 25)
    


plot_site_events_classified(df=rain_single_site, x=precip_roll10,event = fevent,thresh=25)

# debugonce(calc_TPFPFN)
# debugonce(plot_site_events_classified)
plot_site_events_classified(df=rain_impact_harm_simp %>% 
                                filter(site_id =="YE1521_0338") %>% 
                                arrange(date),
                            plot_title="boom",
                            x=precip_roll10,
                            event = fevent,
                            thresh=25,day_window = 60)

p_names <- rain_impact_harm_simp$site_id %>% 
    unique()

p_sites_thresh25_roll10<- rain_impact_harm_simp$site_id %>% 
    unique() %>% 
    map(
        ~{print(.x)
            plot_site_events_classified(df=rain_impact_harm_simp %>% 
                                        filter(site_id==.x),
                                        plot_title = .x,
                                     x=precip_roll10,
                                     event = fevent,
                                     thresh=25,day_window=60)
                
        }
    )




library(animation)
n_frames <- length(p_sites_thresh25_roll10)
duration <- 2  # seconds

# Define the function that will generate each frame of the animation
ani_fun <- function() {
    for (i in seq_along(p_sites_thresh25_roll10)) {
        print(p_sites_thresh25_roll10[[i]])
        Sys.sleep(duration/n_frames)
    }
}

# Create the animation
saveGIF(
    expr = ani_fun(),
    movie.name = "animation.gif",
    nmax = n_frames,
    # interval = duration/n_frames,
    ani.width = 750,
    ani.height = 500
)




plot_site_events(df=rain_single_site, value="precip_roll10",event = "fevent")
rain_impact_harm_simp %>% count(site_id)"YE1114_2749"
plot_site_events(df=rain_impact_harm_simp %>% 
                     filter(site_id=="YE1114_2749"),
                 x=precip_roll10,
                 event = fevent,thresh=25)



rain_single_site %>% 
    mutate(
        idx_test=FP_ret_combined,
        # idx_test=FP_search_lt,
        idx= row_number()
    ) %>% 
    # pivot_longer()
    filter(date>="2022-07-04",date<="2022-09-10") %>% 
    ggplot(aes(x= date, y=precip_roll10)) +
    scale_x_date(breaks = "days",date_labels = "%b %d")+
    scale_y_continuous(breaks=seq(0,90, 5))+
    geom_line()+
    geom_point(data=. %>% filter(idx_test))+
    geom_label(data=. %>% filter(idx_test),aes(label=idx))+
    geom_vline(data=rain_single_site %>% 
                   filter(fevent),aes(xintercept=date), color="red")+
    geom_hline(yintercept = 15,linetype="dashed", color= "blue")+
    geom_hline(yintercept = 25, linetype ="dashed", color="blue")+
    geom_hline(yintercept = 50, linetype ="dashed", color="blue")+
    geom_text(aes(x=ymd("2022-08-20"), y=17,label ="threshold a: 15 mm"))+
    geom_text(aes(x=ymd("2022-08-20"), y=27,label ="threshold b: 25 mm"))+
    geom_text(aes(x=ymd("2022-08-07"), y=68,label ="Reported event"),angle = 90)+
    # theme_hdx()+
    labs(y= "10 day rain accumulation (mm)")+
    theme(
        axis.text.x = element_text(angle=90)
    )


function(df=rain_single_site,
         date="date",
         value="precip_roll10",
         event='fevent',
         thresh=10){
    
    # df2 <- df %>% 
    #     mutate(
    #         x_gte = !!sym(value)>=thresh,
    #         ckgroup = as.character(data.table::rleid(x_gte)),
    #         ckgroup =ifelse(x_gte==F,"less",ckgroup),
    #         x_gte_roll2_r = rollsum(x_gte,k=2, align = "right",na.pad = T),
    #         x_gte_event = x_gte & !!sym(event),
    #         x_gte_fill = ifelse(x_gte,row_number(),NA),
    #         event_fill = ifelse(x_gte_event,paste0("event_",!!sym(event)+row_number()-1),NA)
    #     ) %>% 
    #     fill(event_fill,.direction = "downup") 
    # df2 %>% print(n=nrow(.))
    # df2 %>% 
    #     filter(x_gte_roll2_r>0)
    # df2 %>% 
    #     group_by(event_fill, ckgroup) %>% 
    #     summarise(
    #         count= n()
    #     ) %>% filter(ckgroup!="less") %>% 
    #     ggplot(aes(x=count))+
    #     geom_histogram(bins=100)+
    #     facet_wrap(~event_fill)
    # 
    
    gte_thresh <- df2 %>% 
        filter(!!sym(value)>=thresh)
    
    gte_thresh %>% 
        group_by(event_fill) %>% 
        mutate(
            prev_b4 = replace_na(as.integer(date-lag(date)),0),
            consec_days = prev_b4 %in% c(0,1)
        ) %>% 
        filter(!consec_days)
    x_gte_event <-  x_gte & y # TPs
    event_df<- tibble(
        x,
        y,
        x_gte = x_gte,
        x_gte_event= x_gte_event,
        event = ifelse(x_gte_event,paste0("event_",y+row_number()-1),NA)
    )
    event_df <- event_df %>% 
        tidyr::fill(event) %>% 
    
    gte_thresh %>% 
        mutate(
            since_prev = replace_na(as.integer((date- lag(date))),0)
        )
    
    event_df <- df %>% 
        filter(!!sym(event))
    event_dates <- event_df %>% 
        pull(date)
    
    gte_thresh %>% 
        filter(date>= event_dates-7, 
               date<=event_dates+3)
    

    
    
    value <- df$precip_roll10
    event <- df$fevent
    value[value>=thresh]
    value[event==T]
}



rain_impact_harm <- merge_rainfall_cccm_impact(site_rainfall =cccm_site_chirp_stats,
                                               site_flooding = cccm_flood_impact_data)
rain_impact_harm_simp <- rain_impact_harm %>% 
    select(site_id, date, precip_roll10, fevent)

rain_single_site <- rain_impact_harm_simp %>% 
    filter(site_id =="YE1118_0053")
    

rain_impact_harm_simp <- rain_impact_harm %>% 
    # select(site_id, date, precip_roll10,precip_roll3,precip_roll30, fevent) %>% 
    select(site_id, date, precip_roll10,fevent) %>% 
    group_by(site_id) %>% 
    arrange(date) %>% 
    mutate(
        precip_roll10MA5_r = rollmean(x=precip_roll10,k = 5,fill = NA,align = "right"),
        precip_roll10MA5_l = rollmean(x=precip_roll10,k = 5,fill = NA,align = "left")
    ) %>% 
    ungroup()

rain_single_site <- rain_impact_harm_simp %>% 
    filter(site_id =="YE1114_0016") 

rain_single_site %>% 
    mutate(
        idx_test=bla
    ) %>% 
    # pivot_longer()
    filter(date>="2022-07-04",date<="2022-09-10") %>% 
    ggplot(aes(x= date, y=precip_roll10)) +
    scale_x_date(breaks = "days",date_labels = "%b %d")+
    scale_y_continuous(breaks=seq(0,90, 5))+
    geom_line()+
    geom_point(data=. %>% filter(idx_test))+
    geom_vline(data=rain_single_site %>% 
                   filter(fevent),aes(xintercept=date), color="red")+
    geom_hline(yintercept = 15,linetype="dashed", color= "blue")+
    geom_hline(yintercept = 25, linetype ="dashed", color="blue")+
    geom_text(aes(x=ymd("2022-08-20"), y=17,label ="threshold a: 15 mm"))+
    geom_text(aes(x=ymd("2022-08-20"), y=27,label ="threshold b: 25 mm"))+
    geom_text(aes(x=ymd("2022-08-07"), y=68,label ="Reported event"),angle = 90)+
    # theme_hdx()+
    labs(y= "10 day rain accumulation (mm)")+
    theme(
        axis.text.x = element_text(angle=90)
    )

