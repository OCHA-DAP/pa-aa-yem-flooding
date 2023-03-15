# 
# test_func <-  function(df, by = c("thresh","class")){
#     df %>% 
#         group_by(across(all_of(by)))
#     
# }
# 
# df <- thresh_class_freq_10d
# calculate_performance_metrics(df = thresh_class_freq_10d,
#                               cccm_wb = cccm_wb,
#                               by = c("governorate_name","thresh","class"))




calculate_performance_metrics <-function(df,
                                         cccm_wb,
                                         by=c("governorate_name","thresh","class")){
    df_summarised <- df %>% 
        # should remove dependency on this 
        left_join(cccm_wb$`ML- Flooding Available data` %>% 
                      select(site_id,contains('governorate'))) %>% 
        mutate(
            across(all_of(by),~factor(.x))
        ) %>% 
        group_by(across(all_of(by)),.drop=F) %>% 
        summarise(
            n=sum(n,na.rm=T),.groups = "drop"
        ) 
    
    if("governorate_name" %in% by){
        df_summarised_wide <- 
            df_summarised %>% 
            pivot_wider(id_cols = c("governorate_name","thresh"),
                        names_from = class, values_from = n) 
        
    }
    else{
        df_summarised_wide <- 
            df_summarised %>% 
            pivot_wider(id_cols = thresh,names_from = class, values_from = n) 
        }
    
    df_summarised_wide <- 
        df_summarised_wide %>% 
        mutate(
            precision = TP/(TP+FP),
            recall = TP/(TP+FN),
            f1_score = 2* ((precision*recall)/(precision+recall))
        )
    
    return(df_summarised_wide)
    
    
}


#' Title
#'
#' @param df 
#' @param governorate 
#' @param pseudo_log 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
# tar_load(tbl_performance_5d_gov)
# plot_performance_metrics(df = tbl_performance_10d_overall %>% filter(as.numeric(thresh)>10),
#                          pseudo_log = F)
# # # 
# plot_performance_metrics(df = tbl_performance_10d_gov,
#                          governorate = "Marib",
#                          pseudo_log = F)
# 
# plot_performance_metrics(df = tbl_performance_5d_gov,
#                          governorate = "Hajjah",
#                          pseudo_log = F,x_axis_title = "asdfa")+
#     labs(x="5 day threshold (mm)")
# 
# plot_performance_metrics(df = tbl_performance_10d_gov,
#                          governorate = "Marib",
#                          pseudo_log = T)
# plot_performance_metrics(df = tbl_performance_30d_gov,
#                          governorate = "Marib",
#                          pseudo_log = T,
#                          x_axis_title = "30 day threshold (mm)")
# 
# plot_performance_metrics(df = tbl_performance_30d_overall,
#                          
#                          pseudo_log = T,
#                          x_axis_title = "30 day threshold (mm)")

plot_performance_metrics <- function(df,
                                     governorate=NULL,
                                     pseudo_log=F,
                                     x_axis_title){
 

    if(!is.null(governorate)){
        df <- df %>% 
            filter(governorate_name == governorate)
        plot_title <- paste0("Yemen: ", governorate)
    }
    if(is.null(governorate)){
        plot_title <-  "Yemen"
    }
    fnfptp_long <- df %>% 
        select(any_of(c("governorate_name","thresh", "FN","FP","TP"))) %>% 
        pivot_longer(cols = c("FN","FP","TP"),
                     names_to = "class",
                     values_to="class_value") 
    
    metrics_long <- df %>% 
        select(any_of(c("governorate_name","thresh", "precision", "recall","f1_score"))) %>% 
        pivot_longer(cols=c("precision","recall","f1_score"),names_to = "metric",values_to="metric_value") 
    
    if(pseudo_log){
        y_breaks = c(seq(0,40,by=10), seq(50,500,by=50))
                     }
    if(!pseudo_log){
        y_breaks= seq(0,300,10)
    }
    
    p1 <- fnfptp_long %>%
        ggplot(aes(x= as.numeric(thresh), y= class_value, color=class,group=class))+
        geom_line()+
        geom_line(data= metrics_long,
                  aes(x= as.numeric(thresh),
                      y=metric_value*200,
                      group=metric,
                      color=metric
                  ))+
        scale_color_manual(values = c('FN'="red",
                                      "f1_score"="purple",
                                      "FP"="orange",
                                      "recall"="cyan",
                                      "precision"="black",
                                      "TP"="green"))
        if(pseudo_log){
            p2 <- p1+scale_y_continuous(labels = scales::comma,
                                        breaks = y_breaks,
                                        trans = scales::pseudo_log_trans(),
                                        sec.axis = sec_axis( trans=~.*(1/200),
                                                             breaks = seq(0,1,.1))
            )
        }
        if(!pseudo_log){
            p2 <- p1+scale_y_continuous(labels = scales::comma,
                                        breaks = y_breaks,
                                        sec.axis = sec_axis( trans=~.*(1/200),
                                                             breaks = seq(0,1,.1))
            )
        }
    
       p2+
        scale_x_continuous(breaks = seq(0,300,10))+
        labs(x= x_axis_title,y="Number", title=plot_title) +
        theme_hdx()+    
        theme(
            axis.text.x = element_text(angle=90)
        )
       
}
    