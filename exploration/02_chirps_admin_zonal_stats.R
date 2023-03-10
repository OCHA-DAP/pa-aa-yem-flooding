# this script extracts zonal statistics fo CHIRPS and saves statistics
# as a csv in a similar format to the FloodScan Zonal stats.
# Rolling stats are calculated with 3,5, 10, 30 day "right" aligned windows
# Zonal stats are just calculated for the 10 day rolling and daily chirps at the moment
# may add the 3,5, and 30 day zonal stat extraction later if we decide this
# type of zonal extraction is useful

# WARNING: this script takes a long time to run (~30+ min)

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(lubridate)
library(readxl)
library(janitor)

# toggle on and off to write final csv
write_zonal_stats <- c(T, F)[2]

# Load Data ---------

## Load CODs -------
# file to path to COD
cod_db_fp <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "public", "raw", "yem", "cod_ab", "yem_cod.gpkg"
)

adm <- st_layers(cod_db_fp)$name %>%
  map(
    ~ st_read(cod_db_fp, layer = .x) %>%
      clean_names()
  ) %>%
  set_names(st_layers(cod_db_fp)$name)

adm0 <- adm$yem_admbnda_adm0_govyem_cso_20191002
adm1 <- adm$yem_admbnda_adm1_govyem_cso_20191002
adm2 <- adm$yem_admbnda_adm2_govyem_cso_20191002


## Load CCCM data
cccm_new_fp <- file.path(
  Sys.getenv("AA_DATA_DIR"),
  "private", "exploration",
  "yem",
  "Yemen -  CCCM Cluster -December ML - Flooding available data.xlsx"
)
cccm_new_sheet_names <- excel_sheets(cccm_new_fp)


ss_skip_vals <- c(1, rep(0, 3))
cccm_new <- cccm_new_sheet_names %>%
  map2(
    .y = ss_skip_vals,
    ~ read_excel(cccm_new_fp, sheet = .x, skip = .y) %>%
      clean_names()
  ) %>%
  set_names(cccm_new_sheet_names)

### wrangle CCCM data ----
cccm_master_list <- cccm_new$`ML- Flooding Available data`
cccm_floodlist <- cccm_new$`CCCM FLOOD REPORT IN IDP SITES`


# for merging master and flood list for relevant columns
lat_lon_lookup <- cccm_master_list %>%
  select(site_id,
    longitude = available_coordinates_longitude,
    latitude = available_coordinates_latitude,
    site_population
  )

# get distinct sites with coords
cccm_floodlist_w_coords <- cccm_floodlist %>%
  distinct(cccm_idp_sites_master_list_site_id) %>%
  left_join(lat_lon_lookup,
    by = c("cccm_idp_sites_master_list_site_id" = "site_id")
  ) %>%
  filter(!is.na(latitude)) %>%
  rename(site_id = "cccm_idp_sites_master_list_site_id")

# make distinct sites with coords spatial
cccm_floodlist_sp <- st_as_sf(cccm_floodlist_w_coords, coords = c("longitude", "latitude"), crs = 4326)

## Load chirps raster data

# get file paths
full_fps <- list.files(file.path(Sys.getenv("AA_DATA_DIR"), "public", "processed", "yem", "chirps"), full.names = T)
rast_names <- list.files(file.path(Sys.getenv("AA_DATA_DIR"), "public", "processed", "yem", "chirps"))

# string manipulation to name layers by dates
rast_dates <- str_extract(string = rast_names, pattern = "(?<=yem_chirps_daily_).*?(?=_r0)") %>%
  str_replace_all("_", "-")


# loading in works -- performance here is fine
chirps_daily_full <- terra:::rast(raster::stack(full_fps))
terra::set.names(x = chirps_daily_full, rast_dates)

# Raster data maniupulation ---------
# masking values and next step of rolling stat are slow on this big raster...
# would be interesting to see about `sds` methods, if no bueno batching is an option


## Deal with NAs ----
# this is slooow 5-10 minutes
system.time(
  chirps_daily_full[chirps_daily_full == -9999] <- NA
)

## Pixel level rolling stats ----
# slow also (8 min on my computer)... guess im asking alot
# just going to use rolling 10 for first output
# 10 day
system.time(
  chirps_roll10 <- terra::roll(chirps_daily_full, n = 10, fun = sum, type = "to")
)

# 5 day
system.time(
  chirps_roll5 <- terra::roll(chirps_daily_full, n = 5, fun = sum, type = "to")
)

# 3 day
system.time(
  chirps_roll3 <- terra::roll(chirps_daily_full, n = 3, fun = sum, type = "to")
)

# 30 day
system.time(
  chirps_roll30 <- terra::roll(chirps_daily_full, n = 30, fun = sum, type = "to")
)




# Extract Zonal Stats ---

## Daily ----

adm0_desc_stats <- exact_extract(chirps_daily_full,
  adm0,
  append_cols = "adm0_pcode",
  fun = c("mean", "stdev", "median", "min", "max", "sum"),
  force_df = T,
  full_colnames = T
)

adm1_desc_stats <- exact_extract(chirps_daily_full,
  adm1,
  append_cols = c("adm0_pcode", "adm1_pcode"),
  fun = c("mean", "stdev", "median", "min", "max", "sum"),
  force_df = T,
  full_colnames = T
)
adm2_desc_stats <- exact_extract(chirps_daily_full,
  adm2,
  append_cols = c("adm0_pcode", "adm1_pcode", "adm2_pcode"),
  fun = c("mean", "stdev", "median", "min", "max", "sum"),
  force_df = T,
  full_colnames = T
)

## Zonal ----

adm0_roll_stats <- exact_extract(chirps_roll10,
  adm0,
  append_cols = c("adm0_pcode"),
  fun = c("mean", "median", "max"),
  force_df = T,
  full_colnames = T
)

adm1_roll_stats <- exact_extract(chirps_roll10,
  adm1,
  append_cols = c("adm0_pcode", "adm1_pcode"),
  fun = c("mean", "median", "max"),
  force_df = T,
  full_colnames = T
)
adm2_roll_stats <- exact_extract(chirps_roll10,
  adm2,
  append_cols = c("adm0_pcode", "adm1_pcode", "adm2_pcode"),
  fun = c("mean", "median", "max"),
  force_df = T,
  full_colnames = T
)

# Merge Zonal Stats ----

### daily ----

# separate is a bit slow, huh!?
adm_level_desc_stats <- list(adm0_desc_stats, adm1_desc_stats, adm2_desc_stats) %>%
  map(
    ~ .x %>%
      pivot_longer(-matches("^adm\\d")) %>%
      separate(name, into = c("stat", "date"), sep = "\\.") %>%
      pivot_wider(names_from = "stat", values_from = "value")
  ) %>%
  set_names(c("adm0", "adm1", "adm2"))

## Rolling ----

adm_level_roll_stats <- list(adm0_roll_stats, adm1_roll_stats, adm2_roll_stats) %>%
  map(
    ~ .x %>%
      pivot_longer(-matches("^adm\\d")) %>%
      separate(name, into = c("stat", "date"), sep = "\\.") %>%
      pivot_wider(names_from = "stat", values_from = "value") %>%
      rename_with(.cols = c("mean", "median", "max"), ~ paste0("rollsum10_", .x))
  ) %>%
  set_names(c("adm0", "adm1", "adm2"))


# Merge Daily with Rolling Zonal ----
adm0_zonal <- adm_level_roll_stats$adm0 %>%
  left_join(adm_level_desc_stats$adm0)

adm1_zonal <- adm_level_roll_stats$adm1 %>%
  left_join(adm_level_desc_stats$adm1)

adm2_zonal <- adm_level_desc_stats$adm2 %>%
  left_join(adm_level_roll_stats$adm2)


# Write CSV ----

if (write_zonal_stats) {
  adm0_zonal %>%
    write_csv(
      file.path(Sys.getenv("AA_DATA_DIR"), "public", "processed", "yem", "chirps_zonal", "chirps_daily_stats_ADM0_PCODE.csv")
    )
  adm1_zonal %>%
    write_csv(
      file.path(Sys.getenv("AA_DATA_DIR"), "public", "processed", "yem", "chirps_zonal", "chirps_daily_stats_ADM1_PCODE.csv")
    )

  adm2_zonal %>%
    write_csv(
      file.path(Sys.getenv("AA_DATA_DIR"), "public", "processed", "yem", "chirps_zonal", "chirps_daily_stats_ADM2_PCODE.csv")
    )
}
