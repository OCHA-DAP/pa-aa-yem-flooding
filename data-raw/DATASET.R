# using this space to document output data sets used for other purposes


# 1. admin level % hh/pop in high risk flood sites for mapping in qgis
library(targets)
library(sf)
tar_load(high_risk_flood_stats_by_cod)


st_write(high_risk_flood_stats_by_cod$adm1,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm1_rev",append = T)
st_write(high_risk_flood_stats_by_cod$adm2,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm2_rev",append = T)






gefs_csv_fp <- file.path(Sys.getenv("AA_DATA_DIR"),
                         "public",
                         "processed",
                         "yem",
                         "chirps_gefs","chirps-gefs_roll3_high_risk_hulls.csv")

tar_load(gefs_zonal_rolled)
gefs_zonal_rolled %>% 
    rename(precip_regime =name) %>% 
    filter(precip_regime=="roll3") %>% 
    write_csv(gefs_csv_fp)
