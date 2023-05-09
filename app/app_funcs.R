mutate_trigger_status <- function(df,threshold,max_lead){
    action_window <- 1:max_lead
    readiness_window <- (max_lead+1):10
    
    
    df %>% 
        mutate(
            activation = case_when(
                value >= threshold & leadtime %in% action_window ~ "Action",
                value >= threshold & leadtime %in% readiness_window ~ "Readiness",
                value < threshold ~ "No Activation",
            ) %>%
                fct_expand("Action", "Readiness", "No Activation"),
            txt_label = glue("{activation} predicted {format(date_forecast_predict,'%m-%d')} ({leadtime})")
        )
    
    
}
