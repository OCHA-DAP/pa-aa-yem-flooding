

#' load_libs 
#' @description 
#' convenience function to load libraries used in this project. Since tar_make() clears environ


load_libs <- function(){
    req_CRAN_pkgs <- c(
        "targets",
        "tidyverse",
        "sf",
        "terra",
        "exactextractr",
        "lubridate",
        "readxl",
        "janitor",
        "zoo",
        "rgee",
        "ggrepel",
        "glue",
        "ggiraph",
        "tidyrgee", 
        "extRemes",
        "showtext"
    )
    packages_used = c(req_CRAN_pkgs,"gghdx")
    lapply(packages_used, require, character.only = TRUE)   
}

