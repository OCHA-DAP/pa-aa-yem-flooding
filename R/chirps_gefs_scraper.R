library(rvest)
library(janitor)
library(lubridate)
library(glue)




#' latest_gefs_metadata
#'
#' @param url \code{character} base url of CHIRP-GEFS 16 day forecast server (default = "https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/",)
#' @param leadtime leadtimes to download (default=1:10)
#'
#' @return tibble containing metadata of latest CHIRPS-GEFS files available and what there equivalent name should be on the gdrive

latest_gefs_metadata<- function(
        url="https://data.chc.ucsb.edu/products/EWX/data/forecasts/CHIRPS-GEFS_precip_v12/daily_16day/",
        leadtime= 1:10
){
    
    year_latest_url <- latest_gefs_url(url)
    month_latest_url <- latest_gefs_url(year_latest_url)
    day_latest_url <- latest_gefs_url(month_latest_url)
    
    dl_tbl<- gefs_url_table(day_latest_url) %>% 
        slice(leadtime) 
    
    
    # convert to file naming convention in gdrive
    date_chr_trimmed <- str_remove_all(dl_tbl$name,"data\\.|\\.tif")
    formatted_date <- str_replace_all(date_chr_trimmed, "(\\d{4})\\.(\\d{2})(\\d{2})", "\\1-\\2-\\3")
    fnames <- paste0(paste0("yem_aoi_chirps_gefs_",formatted_date)[1],".",1:length(formatted_date),".tif")
    
    return(
        dl_tbl %>% 
            mutate(
                fname_bare= paste0(formatted_date[1],".",1:length(formatted_date)),
                drive_fname=fnames,
                forecast_made= formatted_date[1]
            )
    )
}



#' gefs_url_table
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples

gefs_url_table <- function(url){
    html_table_ob <-  html_table(read_html(url))[[1]] 
    html_table_ob %>% 
        clean_names() %>% 
        filter(last_modified!="") %>% 
        mutate(
            last_modified = ymd_hm(last_modified),
            url= glue("{url}{name}")
        )
}
#' latest_gefs_url
#'
#' @param url \code{character}
#'
#' @return \code{character} latest url available on current page
#'
#' @examples

latest_gefs_url <- function(url){
    url_table <-  gefs_url_table(url)
    url_table %>% 
        filter(last_modified==max(last_modified)) %>% 
        pull(url)
}