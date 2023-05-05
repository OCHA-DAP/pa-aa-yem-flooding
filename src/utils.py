import datetime
import logging
from typing import List

import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import xarray as xr
from ochanticipy import ChirpsDaily, CodAB, GeoBoundingBox
from scipy.interpolate import interp1d
from scipy.stats import genextreme as gev

from src import constants
from src.datasource_extensions import ChirpsGefs, Era5, FloodScan

logger = logging.getLogger(__name__)


def load_codab(admin_level=0):
    codab = CodAB(constants.country_config)
    # codab.download()
    return codab.load(admin_level=admin_level)


# question: should this preprocessing go here or in datasource_extensions?
def load_floodscan_raw() -> xr.Dataset:
    floodscan = FloodScan(country_config=constants.country_config)
    return floodscan.load_raw(gdf=load_codab(admin_level=0))


def load_high_risk_hulls() -> gpd.GeoDataFrame:
    # TODO: In R targets make the shapefile output be written somewhere
    #  automatically, then update the filepath
    return gpd.read_file(
        constants.oap_data_dir / "private/processed/yem/targets_20230407/csv/"
        "high_risk_hulls.shp"
    )


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


def download_era5(clobber=False):
    era5 = Era5(
        country_config=constants.country_config,
    )
    era5.download(clobber=clobber)


def process_era5(clobber=False):
    era5 = Era5(
        country_config=constants.country_config,
    )
    era5.process(high_risk_hulls=load_high_risk_hulls(), clobber=clobber)


def download_hres(clobber=False):
    hres = Era5(
        country_config=constants.country_config,
    )
    hres.download(clobber=clobber)


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


def process_chirps_gefs(clobber=False):
    adm0 = load_codab(admin_level=0)
    chirps_gefs = ChirpsGefs(
        country_config=constants.country_config,
        adm0=adm0,
        leadtime_max=10,
    )
    chirps_gefs.process(high_risk_hulls=load_high_risk_hulls())


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


def get_dates_list_from_data_series(
    ds: pd.Series, threshold: float, min_duration: int = 1
) -> List[np.datetime64]:
    """
    Given a data array of a smoothly varying quantity over time,
    get the dates of an event occurring where the quantity crosses
    some threshold for a specified duration. If the duration is more than
    one timestep, then the event date is defined as the timestep when
    the duration is reached.
    :param ds: Data array with the main quantity
    :param threshold: Threshold >= which an event is defined
    :param min_duration: Number of timesteps above the quantity to be
    considered an event
    :return: List of event dates
    """
    groups = get_groups_above_threshold(
        observations=ds.values,
        threshold=threshold,
        min_duration=min_duration,
    )
    return [
        ds.index[group[0] + min_duration - 1].to_numpy() for group in groups
    ]


def get_groups_above_threshold(
    observations: np.ndarray,
    threshold: float,
    min_duration: int = 1,
    additional_condition: np.ndarray = None,
) -> List:
    """
    Get indices where consecutive values are equal to or above a
    threshold :param observations: The array of values to search for
    groups (length N) :param threshold: The threshold above which the
    values must be :param min_duration: The minimum group size (default
    1) :param additional_condition: (optional) Any additional condition
    the values must satisfy (array-like of bools, length N) :return:
    list of arrays with indices
    """
    condition = observations >= threshold
    if additional_condition is not None:
        condition = condition & additional_condition
    groups = np.where(np.diff(condition, prepend=False, append=False))[
        0
    ].reshape(-1, 2)
    return [group for group in groups if group[1] - group[0] >= min_duration]


def get_detection_stats(
    true_event_dates: np.ndarray,
    forecasted_event_dates: np.ndarray,
    days_before_buffer: int,
    days_after_buffer: int,
) -> dict:
    """
    Give a list of true and forecasted event dates, calculate how many
    true / false positives and false negatives occurred
    :param true_event_dates: A list of dates when the true events occurred
    :param forecasted_event_dates: A list of dates when the events were
    forecasted to occur
    :param days_before_buffer: How many days before the forecasted date the
    true event can occur. Usually set to the lead time or a small number
    (even 0)
    :param days_after_buffer: How many days after the forecasted date the
    true event can occur. Can usually be a generous number
    like 30, since forecasting too early isn't usually an issue
    :return: dictionary with parameters
    """
    df_detected = pd.DataFrame(
        0, index=np.array(true_event_dates), columns=["detected"]
    )
    FP = 0
    # Loop through the forecasted event
    for forecasted_event in forecasted_event_dates:
        # Calculate the offset from the true dates
        days_offset = (true_event_dates - forecasted_event) / np.timedelta64(
            1, "D"
        )
        # Calculate which true events were detected by this forecast event
        detected = (days_offset >= -1 * days_before_buffer) & (
            days_offset <= days_after_buffer
        )
        df_detected.loc[detected, "detected"] += 1
        # If there were no detections at all, it's a FP
        if not sum(detected):
            FP += 1
    return {
        # TP is the number of true events that were detected
        "TP": sum(df_detected["detected"] > 0),
        # FN is the number of true events that were not detected
        "FN": sum(df_detected["detected"] == 0),
        "FP": FP,
    }


def get_more_detection_stats(df: pd.DataFrame) -> pd.DataFrame:
    """
    Compute precision, recall, F1, POD and FAR
    :param df: Dataframe with columns TP, FP and FN
    :return: Dataframe with additional stats columns
    """
    # Convert everything to float to avoid zero division errors
    for q in ["TP", "FP", "FN"]:
        df[q] = df[q].astype("float")
    df["precision"] = df["TP"] / (df["TP"] + df["FP"])
    df["recall"] = df["TP"] / (df["TP"] + df["FN"])
    df["F1"] = 2 / (1 / df["precision"] + 1 / df["recall"])
    df["POD"] = df["recall"]
    df["FAR"] = 1 - df["precision"]
    for q in ["TP", "FP", "FN"]:
        df[q] = df[q].astype("int")
    return df
