tar_load(thresh_class_freq_10d)
thresh_class_freq_10d %>% 
    filter(class=="FN",n>1)

thresh_class_df <- thresh_class_freq_10d %>% 
    mutate(
        across(c("thresh","class"),~factor(.x))
        ) %>% 
    group_by(thresh,class,.drop=F) %>% 
    summarise(
        n=sum(n,na.rm=T),.groups = "drop"
    ) %>% 
    pivot_wider(id_cols = thresh,names_from = class, values_from = n) %>% 
    mutate(
        precision = TP/(TP+FP),
        recall = TP/(TP+FN),
        f1_score = 2* ((precision*recall)/(precision+recall))
    ) 
thresh_class_df %>% arrange(thresh,desc(FP))   %>% 
    print(n=20)

fnfptp_long <- thresh_class_df %>% 
    select(thresh, FN,FP,TP) %>% 
    pivot_longer(cols = c("FN","FP","TP"),
                 names_to = "class",
                 values_to="class_value") 

metrics_long <- thresh_class_df %>% 
    select(thresh, precision, recall,f1_score) %>% 
    pivot_longer(cols=c("precision","recall","f1_score"),names_to = "metric",values_to="metric_value") 
    
fnfptp_long %>%
    # filter(class!="FP") %>% 
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
                                  "FP"="yellow",
                                  "recall"="brown",
                                  "precision"="black",
                                  "TP"="green"))+
    # scale_x_continuous(labels = scales::percent)+
    scale_y_continuous(labels = scales::comma,
                       breaks = seq(0,300, by =10),
                       trans = scales::pseudo_log_trans(),
                       sec.axis = sec_axis( trans=~.*(1/200),
                                            breaks = seq(0,1,.1))
                       )+
    scale_x_continuous(breaks = seq(0,300,10))+
    labs(x= "10 day threshold (mm)",y="Number") +
    theme_hdx()+    
    theme(
        axis.text.x = element_text(angle=90)
    )
    # 
  ggplot(metrics_long) +
      geom_line(
              aes(x= thresh,
                  y=metric_value,
                  color=metric,
                  group=metric
              ))

  fnfptp_long %>% 
      filter(class== "FN") %>% 
      arrange(class,thresh,desc(class_value)) %>% 
      print(n=nrow(.))
  
  
  
  governorate_level_thresh_df <- thresh_class_freq_10d %>% 
      mutate(
          across(c("thresh","class"),~factor(.x))
      ) %>% 
      left_join(cccm_wb$`ML- Flooding Available data` %>% 
                    select(site_id,contains('governorate'))) %>% 
      group_by(governorate_name,thresh,class,.drop=F) %>% 
      summarise(
          n=sum(n,na.rm=T),.groups = "drop"
      ) %>% 
      pivot_wider(id_cols = c("governorate_name","thresh"),
                  names_from = class, values_from = n) %>% 
      mutate(
          precision = TP/(TP+FP),
          recall = TP/(TP+FN),
      ) 
  
  gov_fnfptp_long <- governorate_level_thresh_df %>% 
      select(governorate_name,thresh, FN,FP,TP) %>% 
      pivot_longer(cols = c("FN","FP","TP"),names_to = "class", values_to="class_value") 
  
  gov_metrics_long <- governorate_level_thresh_df %>% 
      select(governorate_name,thresh, precision, recall) %>% 
      pivot_longer(cols=c("precision","recall"),names_to = "metric",values_to="metric_value")
  
  
  gov_fnfptp_long %>% 
      filter(governorate_name=="Marib") %>% 
      ggplot(aes(x= as.numeric(thresh), y= class_value, color=class,group=class))+
      geom_line()+
      geom_line(data= gov_metrics_long %>% 
                    filter(governorate_name=="Marib") ,
                aes(x= as.numeric(thresh),
                    y=metric_value*200,
                    group=metric,
                    color=metric
                ))+
      scale_color_manual(values = c('red',"orange","brown","black","green"))+
      # scale_x_continuous(labels = scales::percent)+
      scale_y_continuous(labels = scales::comma,
                         breaks = seq(0,300, by =10),
                         # trans = scales::pseudo_log_trans(),
                         sec.axis = sec_axis( trans=~.*(1/200),
                                              breaks = seq(0,1,.1))
      )+
      scale_x_continuous(breaks = seq(0,300,10))+
      labs(x= "10 day threshold (mm)",y="Number", title = "Performance Tests",subtitle = "Marib Yemen") +

      # facet_wrap(~governorate_name)+
      theme_hdx()+    
      theme(
          axis.text.x = element_text(angle=90)
      )
  
  
  boom <- test_threshold_performance_all_sites(df = rainfall_impact_tbl,
                                       x = precip_roll10,
                                       event = fevent,
                                       thresholds = seq(0,200,1))
  
  
  
  boom %>% 
      group_by(thresh,class) %>% 
      filter(class=="FN") %>% 
      summarise(sum(n))
  
  boom %>% 
      filter(class=="FN") %>%
      group_by(site_id) %>% 
      arrange(site_id, thresh) %>% 
      filter(n==n)
      
  
  
  group_by(site_id,thresh,class) %>% 
      summarise()
      filter(
          (thresh==113 & class=="FN" & n==0)|
          (thresh==114 & class=="FN" & n>0)
      ) 
  rainfall_impact_tbl(gov_metrics_long)
  test_threshold_performance_all_sites
  
  
  
  
  thresh_class_freq_10d %>% 
      filter(class=="FN") %>% 
      filter(thresh %in% 40:50) %>% 
      arrange(site_id,thresh) %>% 
      filter(site_id=="YE1504_2020") %>% 
      
      print(n=500)
  
  
  
 rainfall_impact_tbl %>% 
     filter(fevent) %>% 
     group_by(site_id,fevent) %>% 
     count() %>% 
     arrange(desc(n))
 
 rain_single_site <- rainfall_impact_tbl %>% 
     filter(site_id=="YE3003_1604")
 
 
 ck1 <- calc_TPFPFN(df = rain_single_site,
              x = precip_roll10,
              event = fevent,
              thresh = 1,
              look_back = 7,
              look_ahead = 3)
 ck0 <- calc_TPFPFN(df = rain_single_site,
              x = precip_roll10,
              event = fevent,
              thresh = 0,
              look_back = 7,
              look_ahead = 3)
 
 sum(ck1$FPs)
 sum(ck0$FPs)

sites_with_alot_of_FPs_marib <-  thresh_class_freq_10d %>% 
     left_join(
         cccm_wb$`ML- Flooding Available data` %>% select(site_id,site_name,governorate_name)
     ) %>% 
     filter(governorate_name=="Marib") %>% 
     filter(class=="FP") %>% 
     filter(thresh>20) %>% 
     arrange(desc(n)) %>% 
     filter(n %in% c(5, 6)) %>% 
     distinct(site_id) %>% 
    pull(site_id)
 
 
thresh_class_freq_10d %>% 
    left_join(
        cccm_wb$`ML- Flooding Available data` %>% select(site_id,site_name,governorate_name)
    ) %>% 
    filter(site_id %in% sites_with_alot_of_FPs_marib) %>% 
    ggplot(aes(x= thresh, y= n, group=class))+
    geom_line()+
    geom_vline(aes(xintercept= 60))+
    facet_wrap(~site_id)


rainfall_impact_tbl %>% 
    filter(site_id %in%sites_with_alot_of_FPs_marib) %>% 
    ggplot(aes(x=date, y= precip_roll10))+
    geom_line()+
    geom_hline(aes(yintercept=60))+
    facet_wrap(~site_id)



plot_site_events_classified(df = rainfall_impact_tbl %>% 
                                filter(site_id=="YE2613_1399"),
                            x = precip_roll10,
                            event = fevent,
                            thresh = 62,
                            day_window = 100,plot_title = "YE2613_1399")+
    geom_line(data= rainfall_impact_tbl %>% 
                  filter(site_id=="YE2613_1399") %>% 
                  filter(date >="2022-04-01",date<="2022-09-30"),
              aes(x= date, y= precip_roll30),color="red"
    )

rainfall_impact_tbl %>% select(precip_roll10)


