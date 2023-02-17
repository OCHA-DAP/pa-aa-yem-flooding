import datetime
import logging

import pandas as pd
import xarray as xr
from ochanticipy import ChirpsDaily, CodAB, GeoBoundingBox

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


def download_and_process_chirps(clobber=False):
    gdf = load_codab(admin_level=0)
    geo_bounding_box = GeoBoundingBox.from_shape(gdf)
    start_date = datetime.date(year=1998, month=1, day=12)
    end_date = datetime.date(year=2022, month=12, day=31)
    chirps = ChirpsDaily(
        country_config=constants.country_config,
        geo_bounding_box=geo_bounding_box,
        start_date=start_date,
        end_date=end_date,
    )
    chirps.download(clobber=clobber)
    chirps.process(clobber=clobber)
