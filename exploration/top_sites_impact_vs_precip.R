library(tidyverse)
library(zoo)
library(gghdx)
library(sysfonts)
targets::tar_load(cccm_site_chirps)
font_add_google("Source Sans Pro")
showtext::showtext_auto()

cccm_site_chirps_roll <- cccm_site_chirps %>% 
    group_by(site_id) %>%
    arrange(site_id,date) %>% 
    mutate(
        precip_roll3 = rollsum(x= precip_daily,fill=NA, k= 3,align="right"),
        precip_roll5=  rollsum(x= precip_daily,fill=NA, k= 5,align="right"),
        precip_roll10=  rollsum(x= precip_daily,fill=NA, k= 10,align="right"),
        precip_roll30=  rollsum(x= precip_daily,fill=NA, k= 30,align="right")
    )    %>% 
    ungroup()


tar_load(cccm_flood_impact_data)
high_impact_events <- cccm_flood_impact_data %>% 
    filter(
        site_id %in% high_priority_sites$by_affected_shelters$site_id
    ) %>% 
    mutate(
        date= ymd(date_of_episode)
    ) 


cccm_site_chirps_roll %>% 
    filter(
        site_id %in% high_priority_sites$by_affected_shelters$site_id,
        year(date) %in% c(2021,2022)
        ) %>% 
    pivot_longer(cols = contains("roll"), names_to ="period",values_to = "precip") %>% 
    left_join(
        high_impact_events %>% 
            distinct(site_id, site_name),
        by="site_id"
    ) %>% 
    mutate(
        date= ymd(date)
    ) %>% 
    ggplot(aes(x= date,y=precip,color=period))+
    geom_line()+
    scale_x_date(date_breaks = "month",date_labels = "%b-%y")+
    scale_y_continuous(breaks = seq(0,400,by=25))+
    geom_vline(data= high_impact_events,aes(xintercept=date, group=site_id),color="black",linetype="dashed")+
    geom_text_repel(data=high_impact_events,aes(x=date,label=num_shelters_affected,y=100),color="black",angle =90)+
    facet_wrap(~site_name)+
    theme_hdx()+
    labs(y="Precipitation: 10 day rolling sum (mm)", 
         title="Precipitation vs Flood Events",
         subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)")+
    theme(
        axis.text.y=element_text(size=7),
        axis.text.x = element_text(angle=90)
        # legend.position="none"
    )

cccm_site_chirps_roll %>% tail()
cccm_site_chirps_roll %>% 
    mutate(date= ymd(date)) %>% 
    filter(
        site_id %in% high_priority_sites$by_affected_shelters$site_id,
        # year(date) %in% c(2021,2022)
        paste0(date,site_id) %in% c(paste0(high_impact_events$date,high_impact_events$site_id))
    ) %>%
    left_join(
        high_impact_events %>% 
            distinct(site_id, site_name) ,
        by=c("site_id")) %>% 
    pivot_longer(cols = contains("roll"), 
                 names_to ="period",
                 values_to = "precip") %>% 
    mutate(
        date= ymd(date)
    ) %>% 
    ggplot(aes(x= site_name, y=precip))+
    scale_y_continuous(breaks = seq(0,400,by=25))+
    coord_flip()+
    geom_point()+
    # geom_point()+
    # geom_boxplot()+
    
    
    # geom_text_repel(data=high_impact_events,aes(x=date,label=num_shelters_affected,y=100),color="black",angle =90)+
    facet_wrap(~period)+
    theme_hdx()+
    labs(y="Precipitation: 10 day rolling sum (mm)", 
         title="Precipitation vs Flood Events",
         subtitle = "Yemen: IDP Sites with most shelters affected by flooding (2021-2022)")+
    theme(
        axis.text.y=element_text(size=7),
        axis.text.x = element_text(angle=90)
        # legend.position="none"
    )

    
calc_pct_threshold <- function(df,precip){
    c(1:300) %>% 
        purrr::map_dfr(
            ~{
                total_events <- nrow(df)
                num_events_under_thresh <- df %>% 
                    filter(!!sym(precip)<=.x) %>% 
                    nrow()
                data.frame(
                    num_events=num_events_under_thresh,
                    pct_events= num_events_under_thresh/total_events,
                    thresh=.x
                           )
                
            }
            )
    
}

top10_shelter_impact_events<- cccm_site_chirps_roll %>% 
    mutate(date= ymd(date)) %>% 
    filter(
        site_id %in% high_priority_sites$by_affected_shelters$site_id,
        # year(date) %in% c(2021,2022)
        paste0(date,site_id) %in% c(paste0(high_impact_events$date,high_impact_events$site_id))
    ) 
roll3_roc<- calc_pct_threshold(df = top10_shelter_impact_events,
                               precip = "precip_roll3") %>% 
    mutate(param="roll3")
roll5_roc<- calc_pct_threshold(df = top10_shelter_impact_events,
                               precip = precip_roll5) %>% 
    mutate(param="roll5")

roll10_roc<- calc_pct_threshold(df = top10_shelter_impact_events,
                                precip = precip_roll10) %>% 
    mutate(param="roll10")
roll30_roc<- calc_pct_threshold(df = top10_shelter_impact_events,
                                precip = precip_roll30) %>% 
    mutate(param="roll30")

bind_rows(list(roll30_roc,roll5_roc,roll10_roc,roll3_roc)) %>% 
    filter(pct_events<=1) %>% 
    ggplot(aes(x=thresh,y=pct_events, color=param,group=param))+
    geom_line()+
    theme_hdx()+
    scale_y_continuous(labels = scales::percent)+
    labs(title= "ROC Curves for rainfall vs % reported flood events by parameter ",
         subtitle = "Yemen: Top 10 site locations by number of shelters affeected 2021-2022",
         x= "precip (mm)"
    )+
    theme(axis.title.y = element_blank())



# all cccm flood events ---------------------------------------------------

all_cccm_flood_events <- cccm_flood_impact_data %>% 
    mutate(
        date= ymd(date_of_episode)
    ) %>% 
    left_join(cccm_site_chirps_roll %>% 
                  mutate(date= ymd(date)), by= c('site_id',"date"))


all_cccm_rocs <- c("precip_roll3","precip_roll5","precip_roll10","precip_roll30") %>% 
    map_dfr(
        ~ calc_pct_threshold(df=all_cccm_flood_events,precip = .x) %>% 
            mutate(
                param=.x
            )
    )


p_rocs <- all_cccm_rocs %>% 
    filter(pct_events<=1) %>%
    group_by(param) %>% 
    mutate(
        # lab50 = ifelse(round(pct_events, 2)==0.5,thresh,"")
        lab50 = ifelse(abs(pct_events - 0.5) == min(abs(pct_events - 0.5)) & row_number() == which.min(abs(pct_events - 0.5)), thresh, ""),
        lab75 = ifelse(abs(pct_events - 0.75) == min(abs(pct_events - 0.75)) & row_number() == which.min(abs(pct_events - 0.75)), thresh, ""),
        lab90 = ifelse(abs(pct_events - 0.90) == min(abs(pct_events - 0.9)) & row_number() == which.min(abs(pct_events - 0.90)), thresh, "")
    ) %>% 
    ungroup() %>% 
    ggplot(aes(x=thresh,y=pct_events, color=param,group=param))+
    geom_line()+
    theme_hdx()+
    scale_x_continuous(breaks = seq(0,300,by=25))+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab50), hjust=1.3, vjust=-1)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab75), hjust=1.3, vjust=-1)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab90), hjust=1.3, vjust=-1)+
    labs(title= "ROC Curves for rainfall vs % reported flood events by precipitation ",
         subtitle = "Yemen: CCCM Flood Report DB (2021-2022)",
         x= "precip (mm)"
    )+
    geom_hline(yintercept = 0.5,linetype="dashed")+
    geom_hline(yintercept = 0.75,linetype="dashed")+
    geom_hline(yintercept = 0.9,linetype="dashed")+
    theme(axis.title.y = element_blank())


cccm_rep_with_geomorph <- cccm_report_sites_with_geomorph %>% 
    st_drop_geometry() %>% 
    mutate(
        geomorph_srtm_cat=case_when(
            str_detect(srtm_landforms,"Lower")~"Lower Slope",
            str_detect(srtm_landforms,"Upper")~"Upper Slope",
            str_detect(srtm_landforms,"Val")~"Valley"
            
        )
    ) 

all_reps_with_rs <- all_cccm_flood_events %>% 
    left_join(cccm_rep_with_geomorph)

all_reps_with_rs_split <- split(all_reps_with_rs,all_reps_with_rs$geomorph_srtm_cat)
rocs_by_geomorph <- all_reps_with_rs_split%>% 
    map2_dfr( .y = names(all_reps_with_rs_split),\(ds,nm)
        c("precip_roll3","precip_roll5","precip_roll10","precip_roll30") %>% 
            imap_dfr(
                ~ calc_pct_threshold(df=ds,precip = .x) %>% 
                    mutate(
                        param=.x,
                        geomorph=nm
                        
                    )
            )
        
    ) 
p_rocs_geomorph <- rocs_by_geomorph %>% 
    filter(pct_events<=1) %>%
    group_by(param,geomorph) %>% 
    mutate(
        # lab50 = ifelse(round(pct_events, 2)==0.5,thresh,"")
        lab50 = ifelse(abs(pct_events - 0.5) == min(abs(pct_events - 0.5)) & row_number() == which.min(abs(pct_events - 0.5)), thresh, ""),
        lab75 = ifelse(abs(pct_events - 0.75) == min(abs(pct_events - 0.75)) & row_number() == which.min(abs(pct_events - 0.75)), thresh, ""),
        lab90 = ifelse(abs(pct_events - 0.90) == min(abs(pct_events - 0.9)) & row_number() == which.min(abs(pct_events - 0.90)), thresh, "")
    ) %>% 
    ungroup() %>% 
    ggplot(aes(x=thresh,y=pct_events, color=param,group=param))+
    geom_line()+
    theme_hdx()+
    scale_x_continuous(breaks = seq(0,300,by=25))+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab50), hjust=1.3, vjust=-1)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab75), hjust=1.3, vjust=-1)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab90), hjust=1.3, vjust=-1)+
    labs(title= "ROC Curves for rainfall vs % reported flood events by precipitation ",
         subtitle = "Yemen: CCCM Flood Report DB (2021-2022)",
         x= "precip (mm)"
    )+
    geom_hline(yintercept = 0.5,linetype="dashed")+
    geom_hline(yintercept = 0.75,linetype="dashed")+
    facet_wrap(~geomorph)+
    theme(axis.title.y = element_blank())

library(ggpubr)
ggarrange(plotlist = list(p_rocs,p_rocs_geomorph),ncol = 1,nrow = 2)

cccm_report_sites_with_fl %>% 
    ggplot(aes(x= value))+
    geom_histogram(bins=100)+
    scale_x_continuous(breaks = seq(0,10000,by=500))+
    theme(
        axis.text.x = element_text(angle=90)
    )


cccm_report_with_water_dist <- cccm_report_sites_with_fl %>% 
    st_drop_geometry() %>% 
    mutate(
        closest_water_dist=case_when(
            value<1750 ~ "close (<1.75 km)",
            value<6500 ~ "medium distance (1.75-6.5 km)",
            TRUE ~ "far (>6.5 km)"
        ),
        closest_water_dist = factor(closest_water_dist,
                                    levels=c("close (<1.75 km)",
                                             "medium distance (1.75-6.5 km)",
                                             "far (>6.5 km)"))
            
            
    ) 

all_reps_with_rs <- all_cccm_flood_events %>% 
    left_join(cccm_report_with_water_dist %>% select(site_id,closest_water_dist))



all_reps_with_rs_split <- split(all_reps_with_rs,all_reps_with_rs$closest_water_dist)
rocs_by_water_dist <- all_reps_with_rs_split%>% 
    map2_dfr( .y = names(all_reps_with_rs_split),\(ds,nm)
              c("precip_roll3","precip_roll5","precip_roll10","precip_roll30") %>% 
                  imap_dfr(
                      ~ calc_pct_threshold(df=ds,precip = .x) %>% 
                          mutate(
                              param=.x,
                              water_dist_class=nm
                              
                          )
                  )
              
    ) 
p_rocs_water <- rocs_by_water_dist %>% 
    filter(pct_events<=1) %>%
    mutate(
        water_dist_class = factor(water_dist_class,
                                    levels=c("close (<1.75 km)",
                                             "medium distance (1.75-6.5 km)",
                                             "far (>6.5 km)"))
    ) %>% 
    group_by(param,water_dist_class) %>% 
    mutate(
        # lab50 = ifelse(round(pct_events, 2)==0.5,thresh,"")
        lab50 = ifelse(abs(pct_events - 0.5) == min(abs(pct_events - 0.5)) & row_number() == which.min(abs(pct_events - 0.5)), thresh, ""),
        lab75 = ifelse(abs(pct_events - 0.75) == min(abs(pct_events - 0.75)) & row_number() == which.min(abs(pct_events - 0.75)), thresh, ""),
        lab90 = ifelse(abs(pct_events - 0.90) == min(abs(pct_events - 0.9)) & row_number() == which.min(abs(pct_events - 0.90)), thresh, "")
    ) %>% 
    ungroup() %>% 
    ggplot(aes(x=thresh,y=pct_events, color=param,group=param))+
    geom_line()+
    theme_hdx()+
    scale_x_continuous(breaks = seq(0,300,by=25))+
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab50), hjust=1.3, vjust=-1)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab75), hjust=1.3, vjust=-1)+
    geom_text(aes(x=thresh, y= pct_events,group=param, label=lab90), hjust=1.3, vjust=-1)+
    labs(title= "ROC Curves for rainfall vs % reported flood events by precipitation ",
         subtitle = "Yemen: CCCM Flood Report DB (2021-2022)",
         x= "precip (mm)"
    )+
    geom_hline(yintercept = 0.5,linetype="dashed")+
    geom_hline(yintercept = 0.75,linetype="dashed")+
    facet_wrap(~water_dist_class)+
    theme(axis.title.y = element_blank())

p_rocs_water
ggarrange(plotlist = list(p_rocs,p_rocs_water),ncol = 1,nrow = 2)
