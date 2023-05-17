#' Title
#'
#' @param gdrive_id 
#'
#' @return
#' @export
#'
#' @examples
chirps_gefs_gdrive_id <- function(gdrive_id=gdrive_id){
    
    base_dir <- drive_ls(
        path=as_id(gdrive_id)
    )
    inputs_id<- base_dir_df %>% 
        filter(name=="inputs") %>% 
        pull(id)
    
    drive_ls(
        path=as_id(inputs_id)
    ) %>% 
        filter(name=="chirps_gefs") %>% 
        pull(id)
    
}

#' Title
#'
#' @param gdrive_id 
#'
#' @return
#' @export
#'
#' @examples
#' 
# chirps_gefs_gdrive_dir(gdrive_id = gdrive_id)

chirps_gefs_gdrive_dir <- function(gdrive_id){
    chirps_gefs_id <- chirps_gefs_gdrive_id(gdrive_id = gdrive_id)
    drive_ls(
        as_id(chirps_gefs_id)
    )
}




cascade_ymd_directories <- function(run_date=Sys.Date(),gdrive_id= gdrive_id){
    
    run_year <- year(run_date) %>% as.character()
    run_month<- formatC(month(run_date),width = 2,flag = "0")
    run_day<-  mday(run_date) %>% as.character()
    base_dir_id <- chirps_gefs_gdrive_id(gdrive_id)
    base_dir <- chirps_gefs_gdrive_dir(gdrive_id)
    
    
    # check that year of run has folder
    base_dir_filt <- base_dir %>% 
        filter(as.character(name)%in%run_year)
    
    # if no year of run then we need to make month folder and year folder
    if(nrow(base_dir_filt)==0){
        drive_mkdir(name = run_year,path = as_id(base_dir_id))
        
        # then we get the updated directory
        base_dir <- chirps_gefs_gdrive_dir(gdrive_id)
        
        # filtered to year
        base_dir_filt <- base_dir %>% 
            filter(name%in%run_year)
    }
    
    if(nrow(base_dir_filt)>0){
        year_dir_id <- base_dir_filt %>% 
            pull(id)      
        # open year dir
        year_dir <- drive_ls(
            as_id(year_dir_id)
        )
        # filter to month
        year_dir_filt <- year_dir %>% 
            filter(as.character(name) %in% run_month)
        
        # if no month make it inside
        if(nrow(year_dir_filt)==0){
            drive_mkdir(name = month_run,path = as_id(base_dir_filt$id))
            # then refresh that dir with made folder
            year_dir_filt <- drive_ls(
                as_id(year_dir_id)
            ) %>% 
                filter(name %in% run_month)
        }
        # check month directory
        if(nrow(year_dir_filt)>0){
            month_dir_id <- year_dir_filt %>% 
                pull(id)
            
            month_dir <- drive_ls(
                as_id(month_dir_id)
            )
            month_dir_filt <- month_dir %>% 
                filter(as.character(name) %in% run_day)
            
            # if day is not in make day folder
            if(nrow(month_dir_filt)==0){
                drive_mkdir(name = run_day,path = as_id(month_dir_id))
                # then refresh that dir with made folder
                month_dir_filt <-  drive_ls(
                    as_id(month_dir_id)
                )%>% 
                    filter(name %in% run_day)
            }
        
        }
    }
    return(month_dir_filt)

    
}

