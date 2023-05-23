install.packages("remotes") # moving this to the top to see if it will cause the GH action to fail faster
remotes::install_github("OCHA-DAP/gghdx")

install.packages(c("dplyr",
                   "ggplot2",
                   "readr",
                   "sf",
                   "terra", # raster manipulation
                   "exactextractr", # zonal stats
                   "tidyr", # pivot_wider
                   "zoo", # rolling stats
                   "googledrive",
                   "purrr",
                   "lubridate",
                   "here", # set paths in rmds without thinking.
                   "blastula", # rmd emails
                   "glue",  # mainly for pasting text in email.
                   # "remotes", # needed to install gghdx
                   "showtext" # needed to set fonts in gghdx
                   ))




# gdrive_2_0_url <- "https://cran.r-project.org/src/contrib/Archive/googledrive/googledrive_2.0.0.tar.gz"
# install.packages(gdrive_2_0_url, repos=NULL, type="source")
