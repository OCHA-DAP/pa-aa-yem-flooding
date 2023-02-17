import logging

import pandas as pd
import xarray as xr
from ochanticipy import CodAB

from src import constants
from src.datasource_extensions import FloodScan

logger = logging.getLogger(__name__)


def load_codab(admin_level=0):
    codab = CodAB(constants.country_config)
    # codab.download()
    return codab.load(admin_level=admin_level)


# question: should this preprocessing go here or in datasource_extensions?
def load_floodscan_raw() -> xr.Dataset:
    floodscan = FloodScan(country_config=constants.country_config)
    return floodscan.load_raw(gdf=load_codab(admin_level=0))


def process_floodscan_admin(admin_level: int = 2):
    gdf = load_codab(admin_level=admin_level)
    floodscan = FloodScan(country_config=constants.country_config)
    logger.info(f"Processing admin level {admin_level}")
    floodscan.process(gdf=gdf, feature_col=f"ADM{admin_level}_PCODE")


def load_floodscan_stats(admin_level: int = 2) -> pd.DataFrame:
    floodscan = FloodScan(country_config=constants.country_config)
    return floodscan.load(feature_col=f"ADM{admin_level}_PCODE")
