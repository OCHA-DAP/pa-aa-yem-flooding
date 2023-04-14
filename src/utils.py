import datetime
import logging

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import xarray as xr
from ochanticipy import ChirpsDaily, CodAB, GeoBoundingBox
from scipy.interpolate import interp1d
from scipy.stats import genextreme as gev

from src import constants
from src.datasource_extensions import ChirpsGefs, FloodScan

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


def download_chirps_gefs(year=None, clobber=False):
    adm0 = load_codab(admin_level=0)
    if year is not None:
        start_date = datetime.date(year=year, month=1, day=1)
        end_date = datetime.date(year=year, month=12, day=31)
    else:
        start_date = datetime.date(year=2020, month=1, day=1)
        end_date = datetime.date(year=2022, month=12, day=31)
    chirps_gefs = ChirpsGefs(
        country_config=constants.country_config,
        adm0=adm0,
        start_date=start_date,
        end_date=end_date,
        leadtime_max=10,
    )
    chirps_gefs.download(clobber=clobber)


def get_return_period_function_analytical(
    df_rp: pd.DataFrame,
    rp_var: str,
    show_plots: bool = False,
    plot_title: str = "",
    extend_factor: int = 1,
):
    """
    :param df_rp: DataFrame where the index is the year, and the rp_var
    column contains the maximum value per year
    :param rp_var: The column with the quantity to be evaluated
    :param show_plots: Show the histogram with GEV distribution overlaid
    :param plot_title: The title of the plot
    :param extend_factor: Extend the interpolation range in case you want to
    calculate a relatively high return period
    :return: Interpolated function that gives the quantity for a
    given return period
    """
    df_rp = df_rp.sort_values(by=rp_var, ascending=False)
    rp_var_values = df_rp[rp_var]
    shape, loc, scale = gev.fit(
        rp_var_values,
        loc=rp_var_values.median(),
        scale=rp_var_values.median() / 2,
    )
    x = np.linspace(
        rp_var_values.min(),
        rp_var_values.max() * extend_factor,
        100 * extend_factor,
    )
    if show_plots:
        fig, ax = plt.subplots()
        ax.hist(rp_var_values, density=True, bins=20)
        ax.plot(x, gev.pdf(x, shape, loc, scale))
        ax.set_title(plot_title)
        plt.show()
    y = gev.cdf(x, shape, loc, scale)
    y = 1 / (1 - y)
    return interp1d(y, x)
