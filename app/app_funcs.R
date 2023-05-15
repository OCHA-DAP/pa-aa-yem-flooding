mutate_trigger_status <- function(df,threshold,max_lead){
    action_window <- 1:max_lead
    readiness_window <- (max_lead+1):10
    
    
    df %>% 
        mutate(
            activation = case_when(
                value >= threshold & leadtime %in% action_window ~ "Action",
                value >= threshold & leadtime %in% readiness_window ~ "Readiness",
                value < threshold ~ "No Activation"#,
                # TRUE ~ "No Activation"
            ) %>%
                fct_expand("Action", "Readiness", "No Activation"),
            txt_label = glue("{activation} predicted {format(date_forecast_predict,'%m-%d')} ({leadtime})")
        )
    
    
}


grade_trigger_alerts <- function(alert_forecast, 
                                 observed_historical
                                 
                                 # max_lead=7
){
    
    
    activations_pred_df <- alert_forecast %>%
        # filter(value >= thresh_temp) %>%
        # group_by(governorate_name,year = year(date_forecast_predict), season_short, activation) %>%
        # filter(date_forecast_made == min(date_forecast_made), leadtime == max(leadtime),activation=="Action") %>%
        # arrange(season_short, activation) %>%
        ungroup() %>%
        filter(season_short!="dy") %>% 
        mutate(splitter=paste0(governorate_name,"_",year(date_forecast_made),"_",season_short)) %>% 
        split(.$splitter)
    
    historical_gte_thresh_split <- observed_historical %>% 
        filter(season_short!="dy") %>% 
        mutate(splitter=paste0(governorate_name,"_",year(date),"_",season_short)) %>% 
        split(.$splitter)
    
    activations_pred_df %>% 
        imap_dfr(
             \(forecast_gov,nm){
                 cat(nm,"\n")
                 historical_gov <- historical_gte_thresh_split[[nm]]
                 # cat("printing historical observed\n")
                 # historical_gov %>% count(splitter)
                 
                 # print(historical_gov)
                 
                 ret <- forecast_gov %>% 
                     mutate(
                         F_dates = map(date_forecast_made, \(dfm) seq((dfm-74),(dfm-14), by="day")),
                         D_dates = map(date_forecast_made, \(dfm) seq((dfm-74),(dfm+28), by="day")),
                         C_dates = map(date_forecast_made, \(dfm) seq((dfm-13),(dfm-7), by="day")),
                         B_minus_dates = map(date_forecast_made, \(dfm) seq((dfm-6),(dfm-4), by="day")),
                         B_dates = map(date_forecast_made, \(dfm) seq((dfm-3),(dfm-1), by="day")),
                         A_minus_dates = map(date_forecast_made, \(dfm) seq((dfm),(dfm+2), by="day")),
                         A_dates = map2(date_forecast_made,date_forecast_predict, \(dfm,dfp) seq((dfm+3),(dfp+14), by="day")),
                         A_plus_dates = map(date_forecast_predict, \(dfp) seq((dfp-2),(dfp+2), by="day")),
                         
                         A= historical_gov %>% filter(date %in% as_date(unlist(A_dates))) %>% nrow(),
                         Ap= historical_gov %>% filter(date %in% as_date(unlist(A_plus_dates))) %>% nrow(),
                         Am= historical_gov %>% filter(date %in% as_date(unlist(A_minus_dates))) %>% nrow(),
                         B= historical_gov %>% filter(date %in% as_date(unlist(B_dates))) %>% nrow(),
                         Bm= historical_gov %>% filter(date %in% as_date(unlist(B_minus_dates))) %>% nrow(),
                         C= historical_gov %>% filter(date %in% as_date(unlist(C_dates))) %>% nrow(),
                         D= historical_gov %>% filter(date %in% as_date(unlist(D_dates))) %>% nrow(),
                         F= historical_gov %>% filter(date %in% as_date(unlist(F_dates))) %>% nrow(),
                         grade = case_when(
                             (historical_gov %>% filter(date %in% as_date(unlist(F_dates))) %>% nrow())>0~"F",
                             
                             # historical_gov %>% nrow()==0 ~"D",
                             (historical_gov %>% filter(date %in% as_date(unlist(C_dates))) %>% nrow())>0~"C",
                             (historical_gov %>% filter(date %in% as_date(unlist(B_minus_dates))) %>% nrow())>0~"B-",
                             (historical_gov %>% filter(date %in% as_date(unlist(B_dates))) %>% nrow())>0~"B",
                             (historical_gov %>% filter(date %in% as_date(unlist(A_plus_dates))) %>% nrow())>0~"A+",
                             (historical_gov %>% filter(date %in% as_date(unlist(A_minus_dates))) %>% nrow())>0~"A-",
                             (historical_gov %>% filter(date %in% as_date(unlist(A_dates))) %>% nrow())>0~"A",
                             (historical_gov %>% filter(date %in% as_date(unlist(D_dates))) %>% nrow())==0~"D",
                             TRUE~ "ungraded"
                         ),
                         date2= as.character(as_date(date_forecast_made))
                             
                                                 
                     ) %>% 
                     select(governorate_name,
                            date_forecast_made,
                            leadtime,
                            date_forecast_predict,
                            value, 
                            activation,
                            grade,
                            splitter,
                            A,Ap,Am,B,Bm,C,D,F
                            )
                         print(ret)
                         return(ret)
                     
                     
                         # alert_date <- forecast_gover %>% 
                         #     pull(date_forecat_made)
                         # forecast_predict_date <- forecast_gov %>%
                         #     pull(date_forecast_predict)
                         # 
                         # # print(forecast_gov)
                         # pre_alert_14 <- alert_date -14
                         # 
                         # F_dates<- seq(pre_alert_14-60,pre_alert_14,"day")
                         # cat("F date seq:", F_dates,"\n")
                         # F_n <- historical_gov %>% 
                         #     filter(date %in% F_dates) %>% 
                         #     nrow()
                         # F_criteria <- F_n>0
                         # 
                         # D_dates <-  seq(pre_alert_14,pre_alert_14+24, by ="day")
                         # cat("D date seq:", D_dates,"\n")
                         # D_n <- historical_gov %>% 
                         #     filter(date %in% D_dates) %>% 
                         #     nrow()
                         # D_criteria <- D_n==0
                         # 
                         # # 7-14 days late
                         # C_dates <- seq(alert_date-14, alert_date-7,by="day")
                         # cat("C date seq:", C_dates,"\n")
                         # C_n <- historical_gov %>% 
                         #     filter(date %in% C_dates) %>% 
                         #     nrow()
                         # C_criteria <- C_n >0
                         # 
                         # # 4-7 days late
                         # B_minus_dates <- seq(alert_date-7, alert_date-4,by="day")
                         # cat("B minus date seq:", B_minus_dates,"\n")
                         # B_minus_n <- historical_gov %>% 
                         #     filter(date %in% B_minus_dates) %>% 
                         #     nrow()
                         # B_minus_criteria <- B_minus_n > 0
                         # 
                         # # 1-3 days late
                         # B_dates <- seq(alert_date-3, alert_date-1,by="day")
                         # B_n <- historical_gov %>% 
                         #     filter(date %in% B_dates) %>% 
                         #     nrow()
                         # B_criteria <- B_n>0
                         # 
                         # # 1-2 day leadtime
                         # A_minus_dates <- seq(alert_date, alert_date+2,by="day")
                         # A_minus_n <- historical_gov %>% 
                         #     filter(date %in% A_minus_dates) %>% 
                         #     nrow()
                         # A_minus_criteria <- A_minus_n > 0
                         # 
                         # # 1-2 day leadtime
                         # A_plus_dates <- seq(forecast_predict_date-2, forecast_predict_date+2,by="day")
                         # A_plus_n <- historical_gov %>% 
                         #     filter(date %in% A_plus_dates) %>% 
                         #     nrow()
                         # A_plus_criteria <- A_plus_n>0
                         # 
                         # # 1-2 day leadtime
                         # A_dates <- seq(alert_date+2, forecast_predict_date+14,by="day")
                         # A_n <- historical_gov %>% 
                         #     filter(date %in% A_dates) %>% 
                         #     nrow()
                         # A_criteria <- A_n>0
                         # 
                         # 
                         # if(F_criteria){
                         #     ret <- "F"
                         # }
                         # # if threshold not crossed 14 days before or after (after 10 day leadtime)
                         # if(D_criteria & ! F_criteria){
                         #     ret <- "D"
                         # }
                         # if(C_criteria & !D_criteria & !F_criteria){
                         #     ret <- "C"
                         # }
                         # if(B_minus_criteria & !C_criteria){
                         #     ret <- "B-"
                         # }
                         # if(B_criteria & !B_minus_criteria){
                         #     ret <- "B+"
                         # }
                         # if(A_minus_criteria){
                         #     ret <- "A-"
                         # }
                         # if(A_plus_criteria){
                         #     ret <- "A+"
                         # }
                         # if(A_criteria){
                         #     ret <- "A"
                         # }
                         # else{
                         #     ret <- historical_gov %>% 
                         #         filter(date %in% seq(alert_date-14,alert_date+14,by="day")) 
                         # }
                         # return(ret)
                         
                     })
}
                 
                 
             
        
    



# # max_vals_per_window_phase
# 
# grade_trigger_alerts <- function(alert_forecast=max_vals_per_window_phase, observed_historical=historical_obs, threshold){
#     
#     
#     chirps_gefs_tfilt %>% 
#         mutate_trigger_status(threshold=19.8,max_lead = 7)
#     predicted_dates <- alert_forecast %>% 
#         filter(trigger_win)
#     trigger_dates <- alert_forecast$date_forecast_made
#     
#     predicted_dates %>% 
#         map(\(dt){
#             pre_date <- dt-2
#             post_date <-  dt+2
#             
#              %>% 
#                 filter(dt>=(dt-30))
#             
#             
#             
# 
#         }
#             
#         )
#     
#     
#     
#     
#     # Calculate:
#     # 1. Predicted threshold crossing wrong by how many days?
#     
#     # 2. Alert: number of days lead time.
#     
#     
#     # Grade.
#     ################
#     
#     # Predicted forecast correct within +/- 2 days -- A+ : Forecast + trigger perform "perfectly" resources are mobilized to respond
#     
#     # if Action alert issued with at least 3 days lead time and threshold is crossed anytime in next 14 days -- A: resources are mobilized and ready to go - timing not exact
#     
#     # if Action alert issued 1-2 days leadtime -- B+ : resources mobilized a little some are on time some are late
#     
#     # if Action alert issues up to 3 days late (after threshold crossed) -- B : Alert helps partners get ready, but most services arrive just a little late
#     
#     # if Action alert issued up to 4-7 days late (after threshold crossed) -- C : Alert helps partners get ready, but most services arrive just a little late
#     
#     # if Action alert issued and threshold not crossed -- D
#     
#     # if Action alert not issued and threshold is crossed in season - F
#     
#     
# }
gta <- function(alert_forecast,
                observed_historical,
                thresh= 19.8, max_lead=7){
    alerts <- alert_forecast %>%
        filter(value >= thresh, leadtime %in% c(1:max_lead)) %>%
        group_by(governorate_name, year = year(date_forecast_predict), season_short) %>%
        filter(date_forecast_made == min(date_forecast_made)) %>% 
        group_by(date_forecast_made,.add=T) %>% 
        filter(leadtime==max(leadtime)) %>% 
        filter(season_short!="dy") %>% 
        # group_by(governorate_name,year = year(date_forecast_predict), season_short) %>%
        # filter(date_forecast_made == min(date_forecast_made),
               # leadtime == max(leadtime),
               # season_short!="dy") %>%
        arrange(governorate_name,year,season_short) %>%
        ungroup()
    
    # observed_gte_thresh <- observed_historical %>% 
    #     filter(value>=thresh)
    
    alerts %>% 
        pmap_dfr(function(...){
            current <- tibble(...)
            alert_date <- current$date_forecast_made
            predict_date <-  current$date_forecast_predict
            scan_D <- seq((alert_date-56),(alert_date+28),"day") # if no true thresh cross -> FP : grade D
            scan_F <- seq((alert_date-56),(alert_date-14), by="day") # if threshold crosses between 2 & 8 weeks before trigger its quite late: F
            scan_C <- seq((alert_date-13),(alert_date-7), by="day") # if threshold crosses between 1 & 2 weeks like a bit late : C
            scan_Bm <- seq((alert_date-6),(alert_date-4), by="day") # if threshold crosses between 4-6 days  before alert - B-
            scan_B <- seq((alert_date-3),(alert_date-1), by="day")# if threshold crosses between 1-2 days before laert -- not to bad: B
            scan_Bp <- seq((predict_date+14),(predict_date+(7*8)), by="day")
            scan_Am <- seq((alert_date),(alert_date+2), by="day") # if threshold crosses between alert date and 2 days after getting good - A-
            scan_A <- seq((alert_date+3),(predict_date+14), by="day")
            
            scan_Ap <- seq((predict_date-2),(predict_date+2), by="day") # nailed it.
            
            observed_pre_filt <- observed_historical %>% 
                filter(
                    governorate_name==current$governorate_name,
                    season_short==current$season_short
                )
            
            observed_D_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_D) %>% 
                pull(value)
            # cat(observed_D_vals)
            
            observed_F_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_F) %>% 
                pull(value)
            # cat(which(observed_F_vals>threshold))
            
            observed_C_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_C) %>% 
                pull(value)
            observed_Bm_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_Bm) %>% 
                pull(value)
            observed_B_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_B) %>% 
                pull(value)
            
            observed_Bp_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_Bp) %>% 
                pull(value)
            
            observed_Am_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_Am) %>% 
                pull(value)
            observed_A_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_A) %>% 
                pull(value)
            
            observed_Ap_vals <-  observed_pre_filt %>% 
                filter(date %in% scan_Ap) %>% 
                pull(value)
            
            
          
            if(any(observed_F_vals>thresh)){
                ret <- "F"
            }
            else if(any(observed_C_vals>thresh)){
                ret <- "C"
            }
            else if(any(observed_Bm_vals>thresh)){
                ret <- "B-"
            }
            else if(any(observed_B_vals>thresh)){
                ret <- "B"
            }
            else if(any(observed_Am_vals>thresh)){
                ret <- "A-"
            }
            else if(any(observed_Ap_vals>thresh)){
                ret <- "A+"
            }
            else if(any(observed_A_vals>thresh)){
                ret <- "A"
            }
            else if(any(observed_Bp_vals>thresh)){
                ret <- "B+"
            }
            else if(all(observed_D_vals<thresh)){
                ret <- "D"
            }
           
            else{
                ret <- "ungraded"
            }
            current$grade <- ret
            return(current)
        })
}



trigger_grading_desc <- function(){
    tibble::tribble(
        ~Grade,                                                                                   ~Description,                                              ~`Operational Consequence`,
        "A+",                    "Trigger predicted event within +/- 2 days without missing previous events",                                         "Resources mobilized & ready",
        "A",                          "Trigger alert activated with >= 3 day lead time up to 2 weeks early",                                         "Resources mobilized & ready",
        "A-",                                                "Trigger alert activated < 3 days before event",               "Resources mobilized: some ready, some a few days late",
        "B+",                    "Trigger activated 2-8 weeks early",                                                       "Resources arrive very early - potential waste",
        "B",                                              "Trigger alert activated between 1 & 3 days late", "Resources mobilized after event, but still able to mitigate effects",
        "B-",                                              "Trigger alert activated betwee 4  & 6 days late",                        "Resources mobilized after event, less impact",
        "C",                                             "Trigger alert activated between 1 & 2 weeks late",                    "Resoureces mobilized after event, minimal impact",
        "D", "Trigger alert activated, but no event occurrs between 2 months prior and 1 month after alert",              "Resources mobilized, but no event (waste of resources)",
        "F",                                                    "Trigger alert activated over 2 weeks late",                                                "No meaningful impact"
    )
    
    
}
# 
# 
# grade_trigger_alerts2 <- function(alert_forecast=forecast_tfilt, 
#                                  observed_historical=historical_obs,
#                                  threshold=19.8
#                                  # max_lead=7
# ){
#     
#     
#     activations_pred_df <- alert_forecast %>%
#         # filter(value >= thresh_temp) %>%
#         # group_by(governorate_name,year = year(date_forecast_predict), season_short, activation) %>%
#         # filter(date_forecast_made == min(date_forecast_made), leadtime == max(leadtime),activation=="Action") %>%
#         # arrange(season_short, activation) %>%
#         ungroup() %>%
#         split(.$governorate_name)
#     
#     historical_gte_thresh_split <- observed_historical %>% 
#         filter(value>=threshold) %>% 
#         split(.$governorate_name)
#     
#     activations_pred_df %>% 
#         map2(historical_gte_thresh_split,
#              \(forecast_gov,historical_gov){
#                  
#                  forecast_gov %>% 
#                      
#                          alert_date <- forecast_gover %>% 
#                              pull(date_forecat_made)
#                          forecast_predict_date <- forecast_gov %>%
#                              pull(date_forecast_predict)
#                          
#                          # print(forecast_gov)
#                          pre_alert_14 <- alert_date -14
#                          
#                          F_dates<- seq(pre_alert_14-60,pre_alert_14,"day")
#                          cat("F date seq:", F_dates,"\n")
#                          F_n <- historical_gov %>% 
#                              filter(date %in% F_dates) %>% 
#                              nrow()
#                          F_criteria <- F_n>0
#                          
#                          D_dates <-  seq(pre_alert_14,pre_alert_14+24, by ="day")
#                          cat("D date seq:", D_dates,"\n")
#                          D_n <- historical_gov %>% 
#                              filter(date %in% D_dates) %>% 
#                              nrow()
#                          D_criteria <- D_n==0
#                          
#                          # 7-14 days late
#                          C_dates <- seq(alert_date-14, alert_date-7,by="day")
#                          cat("C date seq:", C_dates,"\n")
#                          C_n <- historical_gov %>% 
#                              filter(date %in% C_dates) %>% 
#                              nrow()
#                          C_criteria <- C_n >0
#                          
#                          # 4-7 days late
#                          B_minus_dates <- seq(alert_date-7, alert_date-4,by="day")
#                          cat("B minus date seq:", B_minus_dates,"\n")
#                          B_minus_n <- historical_gov %>% 
#                              filter(date %in% B_minus_dates) %>% 
#                              nrow()
#                          B_minus_criteria <- B_minus_n > 0
#                          
#                          # 1-3 days late
#                          B_dates <- seq(alert_date-3, alert_date-1,by="day")
#                          B_n <- historical_gov %>% 
#                              filter(date %in% B_dates) %>% 
#                              nrow()
#                          B_criteria <- B_n>0
#                          
#                          # 1-2 day leadtime
#                          A_minus_dates <- seq(alert_date, alert_date+2,by="day")
#                          A_minus_n <- historical_gov %>% 
#                              filter(date %in% A_minus_dates) %>% 
#                              nrow()
#                          A_minus_criteria <- A_minus_n > 0
#                          
#                          # 1-2 day leadtime
#                          A_plus_dates <- seq(forecast_predict_date-2, forecast_predict_date+2,by="day")
#                          A_plus_n <- historical_gov %>% 
#                              filter(date %in% A_plus_dates) %>% 
#                              nrow()
#                          A_plus_criteria <- A_plus_n>0
#                          
#                          # 1-2 day leadtime
#                          A_dates <- seq(alert_date+2, forecast_predict_date+14,by="day")
#                          A_n <- historical_gov %>% 
#                              filter(date %in% A_dates) %>% 
#                              nrow()
#                          A_criteria <- A_n>0
#                          
#                          
#                          if(F_criteria){
#                              ret <- "F"
#                          }
#                          # if threshold not crossed 14 days before or after (after 10 day leadtime)
#                          if(D_criteria & ! F_criteria){
#                              ret <- "D"
#                          }
#                          if(C_criteria & !D_criteria & !F_criteria){
#                              ret <- "C"
#                          }
#                          if(B_minus_criteria & !C_criteria){
#                              ret <- "B-"
#                          }
#                          if(B_criteria & !B_minus_criteria){
#                              ret <- "B+"
#                          }
#                          if(A_minus_criteria){
#                              ret <- "A-"
#                          }
#                          if(A_plus_criteria){
#                              ret <- "A+"
#                          }
#                          if(A_criteria){
#                              ret <- "A"
#                          }
#                          else{
#                              ret <- historical_gov %>% 
#                                  filter(date %in% seq(alert_date-14,alert_date+14,by="day")) 
#                          }
#                          return(ret)
#                          
#                      })
# }
#                  
#                  
#              
#         
#     
# 
# 
# 
# # # max_vals_per_window_phase
# # 
# # grade_trigger_alerts <- function(alert_forecast=max_vals_per_window_phase, observed_historical=historical_obs, threshold){
# #     
# #     
# #     chirps_gefs_tfilt %>% 
# #         mutate_trigger_status(threshold=19.8,max_lead = 7)
# #     predicted_dates <- alert_forecast %>% 
# #         filter(trigger_win)
# #     trigger_dates <- alert_forecast$date_forecast_made
# #     
# #     predicted_dates %>% 
# #         map(\(dt){
# #             pre_date <- dt-2
# #             post_date <-  dt+2
# #             
# #              %>% 
# #                 filter(dt>=(dt-30))
# #             
# #             
# #             
# # 
# #         }
# #             
# #         )
# #     
# #     
# #     
# #     
# #     # Calculate:
# #     # 1. Predicted threshold crossing wrong by how many days?
# #     
# #     # 2. Alert: number of days lead time.
# #     
# #     
# #     # Grade.
# #     ################
# #     
# #     # Predicted forecast correct within +/- 2 days -- A+ : Forecast + trigger perform "perfectly" resources are mobilized to respond
# #     
# #     # if Action alert issued with at least 3 days lead time and threshold is crossed anytime in next 14 days -- A: resources are mobilized and ready to go - timing not exact
# #     
# #     # if Action alert issued 1-2 days leadtime -- B+ : resources mobilized a little some are on time some are late
# #     
# #     # if Action alert issues up to 3 days late (after threshold crossed) -- B : Alert helps partners get ready, but most services arrive just a little late
# #     
# #     # if Action alert issued up to 4-7 days late (after threshold crossed) -- C : Alert helps partners get ready, but most services arrive just a little late
# #     
# #     # if Action alert issued and threshold not crossed -- D
# #     
# #     # if Action alert not issued and threshold is crossed in season - F
# #     
# #     
# # }