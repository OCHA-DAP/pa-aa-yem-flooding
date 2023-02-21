
library(rgee)
library(tidyverse)
ee_Initialize()
chirps_link <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_link)
x <- chirps$filterDate("2021-01-01","2022-01-01")

rolling_10_max <- ee_roll_stat(x = x,window = 10, stat="sum")

ee_roll_stat <- function(x, window,stat){
    ee_reducer <- tidyrgee:::stat_to_reducer_full(stat)
    first_img_date <- ee$Date(x$sort("system:time_start",TRUE)$first()$get("system:time_start"))
    last_img_date <- ee$Date(x$sort("system:time_start",FALSE)$first()$get("system:time_start"))
    
    first_date_roll <- first_img_date$advance(window-1,"days")
    
    dates_to_map <- x$filterDate(first_date_roll,last_img_date)$
        aggregate_array("system:time_start")
    
    x_roll <- rgee::ee$ImageCollection$fromImages(
        dates_to_map$map(ee_utils_pyfunc(function(dt){
            start_date <- first_date_roll$advance(ee$Number(window-1)$multiply(-1), "day")
            x_temp <- x$filterDate(start_date, dt)
            x_reduced <- ee_reducer(x_temp)
            x_reduced$set('system:time_start',dt)
        }
        )
        )
    )
    return(x_roll)
}


stat_to_reducer <- function(fun){ switch(
    fun,
    "mean" = rgee::ee$Reducer$mean(),
    "max" = rgee::ee$Reducer$mean(),
    "min" = rgee::ee$Reducer$min(),
    "median"= rgee::ee$Reducer$median(),
    "sum"= rgee::ee$Reducer$sum(),
    "sd" = rgee::ee$Reducer$stdDev(),
    "first" = rgee::ee$Reducer$first(),
    NULL
)
}

tidyrgee:::ee_month_composite.tidyee(
    
)
