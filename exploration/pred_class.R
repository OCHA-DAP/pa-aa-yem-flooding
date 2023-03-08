
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
        ) 
    return(rainfall_time_constrained)
    
    
}

rain_impact_harm <- merge_rainfall_cccm_impact(site_rainfall =cccm_site_chirp_stats,
                                               site_flooding = cccm_flood_impact_data)
rain_impact_harm_simp <- rain_impact_harm %>% 
    select(site_id, date, precip_roll10, fevent)
wth <- rain_impact_harm_simp %>% 
    mutate(row_id= row_number() ) %>% 
    filter(row_id %in% c(970:990))

wth %>% 
    mutate(
        asdf= consecutive_inclusion(x = precip_roll10,y=fevent, thresh=20)
    )
debugonce(consecutive_inclusion)

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
test_v <-  c(1,4,5,1,2,5,3,1)
rollsum(x = test_v,k = 3,align = "right",fill = NA)

rain_impact_harm_simp <- rain_impact_harm %>% 
    # select(site_id, date, precip_roll10,precip_roll3,precip_roll30, fevent) %>% 
    select(site_id, date, precip_roll10,fevent) %>% 
    arrange(date) %>% 
    mutate(
        precip_roll10MA5_r = rollmean(x=precip_roll10,k = 5,fill = NA,align = "right"),
        precip_roll10MA5_l = rollmean(x=precip_roll10,k = 5,fill = NA,align = "left")
    )


bla <- rain_impact_harm_simp %>% 
    filter(site_id =="YE1114_0016")

mean(bla[2:32,]$precip_roll10,na.rm=T)
mean(bla[32:62,]$precip_roll10,na.rm=T)
bla[30:33,]
rain_single_site <- rain_impact_harm_simp %>% 
    filter(site_id =="YE1114_0016") %>% View()
    # print(n=20)
    pivot_longer(starts_with("precip_"))
mean(c(1.63,1.63,1.63,1.63,0))
rain_single_site %>% 
    filter(date>="2022-07-04",date<="2022-09-10") %>% 
    ggplot(aes(x= date, y=value, color=name)) +
    scale_x_date(breaks = "days",date_labels = "%b %d")+
    scale_y_continuous(breaks=seq(0,90, 5))+
    geom_line()+
    geom_vline(data=rain_single_site %>% 
                   filter(fevent,name=="precip_roll10"),aes(xintercept=date), color="red")+
    geom_hline(yintercept = 15,linetype="dashed", color= "blue")+
    geom_hline(yintercept = 25, linetype ="dashed", color="blue")+
    geom_text(aes(x=ymd("2022-08-20"), y=17,label ="threshold a: 15 mm"))+
    geom_text(aes(x=ymd("2022-08-20"), y=27,label ="threshold b: 25 mm"),stat="identity")+
    geom_text(aes(x=ymd("2022-08-07"), y=68,label ="Reported event"),angle = 90, stat="identity")+
    theme_hdx()+
    labs(y= "10 day rain accumulation (mm)")+
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=90)
    )

function(df=rain_single_site,
         date,
         value,
         event,
         thresh=10){
    
    gte_thresh <- df %>% 
        filter(!!sym(value)>=thresh)
    
    event_df <- df %>% 
        filter(!!sym(event))
    
    gte_thresh %>% 
        left_join(event_df, by="date") %>% 
        mutate(row_idx = row_number()) %>% 
        filter(row_idx %in% c(:150))
    
    df %>% 
        filter(date>="2022-07-04",date<="2022-09-10") %>% 
        ggplot(aes(x= date, y=precip_roll10)) +
        scale_x_date(breaks = "days",date_labels = "%b %d")+
        scale_y_continuous(breaks=seq(0,90, 5))+
        geom_line()+
        geom_vline(data=gte_thresh %>% 
                       filter(fevent),aes(xintercept=date), color="red")+
        geom_hline(yintercept = 15,linetype="dashed", color= "blue")+
        geom_hline(yintercept = 25, linetype ="dashed", color="blue")+
        geom_text(label ="threshold a: 15 mm",x=ymd("2022-08-20"), y=17)+
        geom_text(label ="threshold b: 25 mm",x=ymd("2022-08-20"), y=27)+
        geom_text(label ="Reported event",x=ymd("2022-08-07"), y=68,angle = 90)+
        theme_hdx()+
        labs(y= "10 day rain accumulation (mm)")+
        theme(
            axis.text.x = element_text(angle=90)
        )
    
    
    value <- df$precip_roll10
    event <- df$fevent
    value[value>=thresh]
    value[event==T]
}

