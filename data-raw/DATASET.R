# using this space to document output data sets used for other purposes


# 1. admin level % hh/pop in high risk flood sites for mapping in qgis
library(targets)
library(sf)
tar_load(high_risk_flood_stats_by_cod)
<<<<<<< Updated upstream
st_write(high_risk_flood_stats_by_cod$adm1,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm2",append = T)
st_write(high_risk_flood_stats_by_cod$adm2,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm0",append = T)
=======
st_write(high_risk_flood_stats_by_cod$adm1,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm1_rev",append = T)
st_write(high_risk_flood_stats_by_cod$adm2,dsn = "../../qgis_projects/aa_yem_flood.gpkg","yem_adm2_rev",append = T)
>>>>>>> Stashed changes
