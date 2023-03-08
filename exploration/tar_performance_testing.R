# performance testing - WILL add to R/ directory when complete


# NOT FINISHED #################
#' include_lgl - NEED better name
#' @description this is where I want to make a function to exclude consecutive values over threshold from performance
#'  testing
#' @param x numeric vector
#' @param thresh \code{numeric} threshold value
#' @return
#' @export
#'
#' @examples \dontrun{
#' 
#'  # this example is just a reminder to self so I remember how this working
#'  # when I return
#'  
#'  rainfall_event_long_split$precip_roll10 %>% 
#      group_by(site_id) %>% 
#      arrange(site_id, date) %>% 
#      mutate(
#          value =include_lgl(x = value,thresh = 20)
#      ) %>% 
#      filter(!is.na(value))
#' }
# consecutive_inclusion <- function(x, thresh=20){
#     x_gte <- x >= thresh
#     x_gte_roll=rollsum(x = x_gte,k = 2,align = 'right',fill = NA)
#     x_incl = ifelse(x_gte & x_gte_roll>1,NA,x)
#     return(x_incl)
# }






# NOT FINISHED #################
#' threshold_classification_table
#' @description obtain accuracy statistics for plotting in ROC-type curves
#' @param site_rainfall chirps data at sites (obj: `cccm_site_chirp_stats`)
#' @param site_flooding flood report data (obj:`cccm_flood_impact_data`)
#' @param by if we want to disaggregate performance testing
#' @return list of data.frames for each rain accumulation window. Each 
#'  table containing the following statistics for all threshold values:
#'  1. `type` column (TP,TN,FP,FN)
#'  2. `accuracy`: `(TP+TN)/(TP+TN+FP+FN)`
#'  3. `precicsion`: `TP/(TP+FN)`
#'  4. `recall`: `TP/(TP+FN)`
#'  5. `True Positive Rate (TPR)`: different naming for `recall`
#'  6. `False Postitive Rate (FPR)`: `FP/(FP+TN)` 
#' @examples \dontrun{
#'  # NOT FINISHED
#'  library(tidyverse)
 # tar_load(cccm_site_chirp_stats)
 # tar_load(cccm_flood_impact_data)
 # classification_tbl <- threshold_classification_table(site_rainfall = cccm_site_chirp_stats,
 #                                                      site_flooding =cccm_flood_impact_data)
 # 
 # classification_tbl_gov <- threshold_classification_table(site_rainfall = cccm_site_chirp_stats,
 #                                                      site_flooding =cccm_flood_impact_data,
 #                                                      by ="governorate_name" )
#' }
#' 
#' 

 

threshold_classification_table <- function(site_rainfall=cccm_site_chirp_stats ,
                                           site_flooding=cccm_flood_impact_data,
                                           by= NULL,
                                           exclude="all_consec"
                                           ){
    if(is.null(by)){
        grpby <- "type"    
    }
    if(!is.null(by)){
        grpby = c("type",by)
    }
    
    site_lookup <- site_flooding %>% 
        distinct(governorate_name,site_id, site_name)
    
    site_report_date_range <-  site_flooding$date_of_episode %>% range(na.rm=T)
    site_report_date_range[2]-site_report_date_range[1]
    
    rainfall_time_constrained <- site_rainfall %>%
        mutate(date= ymd(date)) %>% 
        filter(date>=site_report_date_range[1],
               date<=site_report_date_range[2]) %>% 
        mutate(
            fevent= paste0(site_id,date)%in% paste0(cccm_flood_impact_data$site_id,
                                                    cccm_flood_impact_data$date_of_episode)
        ) 
    
    rainfall_event_long <- rainfall_time_constrained %>% 
        pivot_longer(starts_with("precip_")) %>% 
        left_join(site_lookup,by="site_id")
    rainfall_event_long_split <-  split(rainfall_event_long,rainfall_event_long$name)
    
    classification_tbl <- rainfall_event_long_split %>% 
        map2(names(rainfall_event_long_split),
             \(window_df,nm){
                 cat(crayon::green(nm),"\n")
                 c(1:300) %>% 
                     map_dfr(
                         \(thresh){
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
                             events_classified_grp <- events_classified %>%
                                 grp_data_by(grpby)
                             
                             events_classified_grp$df %>% 
                                 summarise(
                                     n=n(),.groups="drop"
                                 ) %>% 
                                 complete(governorate_name,type, fill = list(n = 0)) %>% 
                                 pivot_wider(names_from = type, values_from = n) %>% 
                                 mutate(
                                     accuracy = (TP+TN)/(TP+FP+TN+FN),
                                     precision = TP/(TP+FP),
                                     recall = TP/(TP+FN),
                                     TPR = recall,
                                     POD= recall,
                                     FAR = 1- precision,
                                     FPR=FP/(FP+TN),
                                     thresh=thresh
                                 )
                         }
                     )
             }
        )
    return(classification_tbl 
           )


}


test <- threshold_classification_table2(site_rainfall = cccm_site_chirp_stats,
                               site_flooding = cccm_flood_impact_data)
test_excl <- threshold_classification_table2(site_rainfall = cccm_site_chirp_stats,
                               site_flooding = cccm_flood_impact_data,exclude = "all_consec_above")
# try this again, but with 
threshold_classification_table2 <- function(site_rainfall=cccm_site_chirp_stats ,
                                           site_flooding=cccm_flood_impact_data,
                                           by= NULL,
                                           exclude="all_consec_above"
                                           ){
    site_lookup <- site_flooding %>% 
        distinct(governorate_name,site_id, site_name)
    
    # site_report_date_range <-  site_flooding$date_of_episode %>% range(na.rm=T)
    # site_report_date_range[2]-site_report_date_range[1]
    
    rainfall_time_constrained <- site_rainfall %>%
        mutate(date= ymd(date)) %>% 
        filter(date>=site_report_date_range[1],
               date<=site_report_date_range[2]) %>% 
        mutate(
            fevent= paste0(site_id,date)%in% paste0(cccm_flood_impact_data$site_id,
                                                    cccm_flood_impact_data$date_of_episode)
        ) 
    
    rainfall_event_long <- rainfall_time_constrained %>% 
        pivot_longer(starts_with("precip_")) %>% 
        left_join(site_lookup,by="site_id")
    
    rainfall_event_long_split <-  split(rainfall_event_long,rainfall_event_long$name)
    
    classification_tbl <- rainfall_event_long_split %>% 
        map2(names(rainfall_event_long_split),
             \(window_df,nm){
                 cat(crayon::green(nm),"\n")
                 c(1:300) %>% 
                     map_dfr(
                         \(thresh){
                             if(exclude=="all_consec_above"){
                                 window_df <-  window_df %>% 
                                     group_by(site_id) %>% 
                                     arrange(site_id, date) %>% 
                                     mutate(
                                         include= consecutive_inclusion(x=value,
                                                                        y=fevent,
                                                                        thresh = thresh)
                                     ) %>% 
                                     ungroup()
                                 excluded <- window_df %>% 
                                     filter(is.na(include)) %>% 
                                     nrow()
                                 cat(excluded," records excluded\n")
                                 
                                 # need to change NA to F
                                 window_df <- window_df %>% 
                                     filter(!is.na(include))
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
                                     thresh=thresh
                                 )
                         }
                     )
             }
        )
    return(classification_tbl)


}


# 
# tryign to check it out
classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    map(
       ~ .x %>% 
            group_by(governorate_name) %>% 
           summarise(
               sum_tp=sum(TP,na.rm = T),
               sum_fp = sum(FP,na.rm=T)
           )
    )

classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    keep_at(at= ~str_detect(.x,"roll10$")) %>% 
    imap_dfr(
        ~.x %>%
            mutate(precip_window = .y)
    ) %>%
    ggplot(aes(x=FPR,y=TPR,color=precip_window))+
    geom_line()+
    geom_point()+
    scale_x_continuous(labels= scales::percent,breaks=seq(0,1,.1))+
    scale_y_continuous(labels= scales::percent,breaks = seq(0,1,.1))+
    facet_wrap(~governorate_name)+
    theme_hdx()

# ROC for abyan specifically
classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    keep_at(at= ~str_detect(.x,"roll10$")) %>% 
    bind_rows() %>% 
    filter(governorate_name =="Abyan") %>%
    arrange(FPR) %>% 
    ggplot(aes(x= FPR, y = TPR))+
    geom_line()+
    geom_point()+
    scale_x_continuous(labels=scales::percent)+
    scale_y_continuous(labels = scales::percent)

# thresh vs 
classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    keep_at(at= ~str_detect(.x,"roll10$")) %>% 
    bind_rows() %>% 
    filter(governorate_name =="Abyan")
    ggplot(aes(x= thresh, y = recall))+
    geom_line()+
    scale_y_continuous(labels = scales::percent)+
    facet_wrap(~governorate_name)

#threshold accuracy
classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    imap_dfr(
        ~.x %>%
            mutate(precip_window = .y)
    ) %>%
    ggplot(aes(x= thresh, y = accuracy, color = precip_window))+
    geom_line()+
    scale_y_continuous(labels = scales::percent)+
    labs(x= "threshold",y="accuracy: TP+FP/(TP+FP+FN+FP)")+
    facet_wrap(~governorate_name)+
    theme_hdx()

classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    imap_dfr(
        ~.x %>%
            mutate(precip_window = .y)
    ) %>%
    ggplot(aes(x= POD, y = FAR, color = precip_window))+
    geom_line()+
    scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::percent)+
    labs(x= "POD",y="FAR")+
    facet_wrap(~governorate_name)+
    theme_hdx()

classification_tbl_gov %>% 
    discard_at(at = ~str_detect(.x,"_c$")) %>% 
    imap_dfr(
        ~.x %>%
            mutate(precip_window = .y)
    ) %>%
    filter(precip_window =="precip_roll10",
           governorate_name=="Marib"
           ) %>%
    # print(n=nrow(.)) %>% View()
    pivot_longer(cols = c("TP","FP","FN")) %>%
    # pull(precision) %>% range(na.rm=T)
    ggplot(aes(x= thresh, y = value, color=name))+
    geom_line()+
    scale_color_discrete(name="performance class")+
    geom_line(aes(x= thresh, y=precision*1000,linetype='precision'), color="black")+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0,100,20),
                                  seq(100, 200,50),
                                  seq(200,1000,200),
                                  seq(2000,6000, 2000)),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*.0010,
                                            breaks = c(seq(0,.1,.02),
                                                seq(.1,.8, by =0.1))
                                            ))+
                       labs(x= "10 day threshold (mm)",y="Number")+
    theme_hdx()+
    facet_wrap(~governorate_name)
test$precip_roll10 %>% 
    # print(n=nrow(.)) %>% View()
    pivot_longer(cols = c("TP","FP","FN")) %>%
    # pull(precision) %>% range(na.rm=T)
    ggplot(aes(x= thresh, y = value, color=name))+
    geom_line()+
    scale_color_discrete(name="performance class")+
    geom_line(aes(x= thresh, y=precision*1000,linetype='precision'), color="black")+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0,100,20),
                                  seq(100, 200,50),
                                  seq(200,1000,200),
                                  seq(2000,6000, 2000)),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*.00010,
                                            breaks = c(seq(0,.1,.02),
                                                seq(.1,.8, by =0.1))
                                            ))+
                       labs(x= "10 day threshold (mm)",y="Number")
test$precip_roll10
test_excl$precip_roll10 %>% 
    # print(n=nrow(.)) %>% View()
    pivot_longer(cols = c("TP","FP","FN")) %>%
    # pull(precision) %>% range(na.rm=T)
    ggplot(aes(x= thresh, y = value, color=name))+
    geom_line()+
    scale_color_discrete(name="performance class")+
    geom_line(aes(x= thresh, y=precision*1000,linetype='precision'), color="black")+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0,100,20),
                                  seq(100, 200,50),
                                  seq(200,1000,200),
                                  seq(2000,6000, 2000)),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*.00010,
                                            breaks = c(seq(0,.1,.02),
                                                seq(.1,.8, by =0.1))
                                            ))+
                       labs(x= "10 day threshold (mm)",y="Number")

test_excl$precip_roll3$precision %>% range(na.rm=T)
test_excl$precip_roll3 %>% 
    # print(n=nrow(.)) %>% View()
    pivot_longer(cols = c("TP","FP","FN")) %>%
    # pull(precision) %>% range(na.rm=T)
    ggplot(aes(x= thresh, y = value, color=name))+
    geom_line()+
    scale_color_discrete(name="performance class")+
    geom_line(aes(x= thresh, y=precision*1000,linetype='precision'), color="black")+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0,100,20),
                                  seq(100, 200,50),
                                  seq(200,1000,200),
                                  seq(2000,6000, 2000)),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*.0010,
                                            breaks = c(seq(0,.1,.02),
                                                seq(.1,.8, by =0.1))
                                            ))+
                       labs(x= "10 day threshold (mm)",y="Number")
test$precip_roll10 %>% 
    pivot_longer(cols = c("TP","FP","FN")) %>%
    # pull(precision) %>% range(na.rm=T)
    ggplot(aes(x= thresh, y = value, color=name))+
    geom_line()+
    scale_color_discrete(name="performance class")+
    geom_line(aes(x= thresh, y=precision*1000,linetype='precision'), color="black")+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0,100,20),
                                  seq(100, 200,50),
                                  seq(200,1000,200),
                                  seq(2000,6000, 2000)),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*.0010,
                                            breaks = c(seq(0,.1,.02),
                                                seq(.1,.8, by =0.1))
                                            ))+
                       labs(x= "10 day threshold (mm)",y="Number")


how_we_do$precip_roll10$precision %>% range(na.rm=T)
data.frame(excl = how_we_do$precip_roll10$precision[1:240] ,
           all=test$precip_roll10$precision[1:240])
test$precip_roll10 %>% 
    # print(n=nrow(.)) %>% View()
    pivot_longer(cols = c("TP","FP","FN")) %>%
    pivot_longer(cols=c("precision","recall"),names_to="metric",values_to="metric_val") %>% 
    # pull(precision) %>% range(na.rm=T)
    ggplot(aes(x= thresh, y = value, color=name))+
    geom_line()+
    scale_color_discrete(name="performance class")+
    geom_line(aes(x= thresh, y=metric_val*1000,color=metric))+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = c(seq(0,100,20),
                                  seq(100, 200,50),
                                  seq(200,1000,200),
                                  seq(2000,6000, 2000)),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*.0010,
                                            breaks = c(seq(0,.1,.02),
                                                seq(.1,.8, by =0.1))
                                            ))+
                       labs(x= "10 day threshold (mm)",y="Number")


# 
# classification_tbl$precip_roll10 %>% 
#     ggplot(aes(x= thresh, y = accuracy))+
#     geom_line()+
#     scale_y_continuous(labels = scales::percent)
# 
# classification_tbl$precip_roll10 %>% 
#     ggplot(aes(x= thresh, y = recall))+
#     geom_line()+
#     scale_y_continuous(labels = scales::percent)
# 
# 
# 
# 
# 
all_high_risk_sites <- cccm_floodscore_df %>% 
    filter(site_flood_hazard_score == "High risk")

cccm_site_chirp_stats %>% 
    filter(site_id %in% all_high_risk_sites$site_id) %>% 
    distinct(site_id)

# plot all events
rain_and_events <- cccm_site_chirp_stats %>% 
    select(-ends_with("_c")) %>% 
    mutate(
        date= ymd(date)
    ) %>% 
    filter(year(ymd(date))%in% c(2021,2022)) %>% 
    mutate(
        fevent = paste0(site_id, date) %in% paste0(
            cccm_flood_impact_data$site_id,
            cccm_flood_impact_data$date_of_episode
        )
    )
# rain_and_events$site_id %>% 
#     unique() %>% 
#     map(
#         \(event_id){
#             rain_site_temp <- rain_and_events %>% 
#                 filter(site_id==event_id)
#             rain_site_temp_event_date <-  rain_site_temp %>% 
#                 filter(fevent) %>% 
#                 pull(date)
#         }
#            
#             rain_site_temp_event_date %>% 
#                 map(
#                     ~{
#                         
#                     }
#                 )
#             
#             
#             
#         }
#     )
# 
# 

rain_and_events %>% filter(fevent) %>% 
    pivot_wider(names_from = name, values_from = value)

    
