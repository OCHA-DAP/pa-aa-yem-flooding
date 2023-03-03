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
include_lgl <- function(x, thresh=20){
    x_gte <- x >= thresh
    x_gte_roll=rollsum(x = x_gte,k = 2,align = 'right',fill = NA)
    x_incl = ifelse(x_gte & x_gte_roll>1,NA,x)
    return(x_incl)
}


# NOT FINISHED #################
#' threshold_classification_table
#' @description obtain accuracy statistics for plotting in ROC-type curves
#' @param site_rainfall chirps data at sites (obj: `cccm_site_chirp_stats`)
#' @param site_flooding flood report data (obj:`cccm_flood_impact_data`)
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
#'  tar_load(cccm_site_chirp_stats)
#'  tar_load(cccm_flood_impact_data)
#'  threshold_classification_table(site_rainfall = cccm_site_chirp_stats,site_flooding =cccm_flood_impact_data)
#' }
threshold_classification_table <- function(site_rainfall=cccm_site_chirp_stats , site_flooding=cccm_flood_impact_data){
    site_report_date_range <-  site_flooding$date_of_episode %>% range(na.rm=T)
    rainfall_time_constrained <- site_rainfall %>%
        mutate(date= ymd(date)) %>% 
        filter(date>=site_report_date_range[1],
               date<=site_report_date_range[2]) %>% 
        mutate(
            fevent= paste0(site_id,date)%in% paste0(cccm_flood_impact_data$site_id,
                                                    cccm_flood_impact_data$date_of_episode)
        )
    
    rainfall_event_long <- rainfall_time_constrained %>% 
        pivot_longer(starts_with("precip_"))
    rainfall_event_long_split <-  split(rainfall_event_long,rainfall_event_long$name)
    
    classification_tbl2 <- rainfall_event_long_split %>% 
        map2(names(rainfall_event_long_split),
             \(window_df,nm){
                 cat(crayon::green(nm),"\n")
                 c(1:300) %>% 
                     map_dfr(
                         \(thresh){
                             window_df %>% 
                                 mutate(
                                     type = case_when(
                                         value>=thresh & fevent ~ "TP",
                                         value>= thresh & !fevent ~"FP",
                                         value<thresh & fevent ~"FN",
                                         value< thresh & !fevent ~"TN"
                                     ),
                                     type= fct_expand(type, c("TP","FP","FN","TN"))
                                 ) %>% 
                                 group_by(type) %>% 
                                 summarise(
                                     n=n(),.groups="drop"
                                 ) %>% 
                                 complete(type, fill = list(n = 0)) %>% 
                                 pivot_wider(names_from = type, values_from = n) %>% 
                                 mutate(
                                     accuracy = (TP+TN)/(TP+FP+TN+FN),
                                     precision = TP/(TP+FP),
                                     recall = TP/(TP+FN),
                                     TPR = recall,
                                     FPR=FP/(FP+TN),
                                     thresh=thresh
                                 )
                             
                             
                             
                         }
                     )
             }
        )


}


# 
# 
# classification_tbl2 %>%
#     imap_dfr(
#         ~.x %>% 
#             mutate(precip_window = .y)
#     ) %>% 
#     ggplot(aes(x=FPR,y=TPR,color=precip_window))+
#     geom_line()+
#     scale_x_continuous(labels= scales::percent,breaks=seq(0,1,.1))+
#     scale_y_continuous(labels= scales::percent,breaks = seq(0,1,.1))+
#     theme_hdx()
# 
# classification_tbl$precip_roll10 %>% 
#     mutate(
#         FPR=FP/(FP+TN)
#     ) %>% 
#     ggplot(aes(x= thresh, y = recall))+
#     geom_line()+
#     scale_y_continuous(labels = scales::percent)
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
