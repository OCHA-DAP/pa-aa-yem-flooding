import logging
from datetime import date, timedelta
from pathlib import Path

import geopandas as gpd
import pandas as pd
import rasterio
import xarray as xr
from dateutil import rrule
from ochanticipy import CountryConfig
from ochanticipy.datasources.datasource import DataSource
from rasterio.errors import RasterioIOError

logger = logging.getLogger(__name__)


# TODO: Fix this -- either make a separate ABC or make it work
#  better from the toolbox
class _DataSourceExtension(DataSource):
    def __init__(self, country_config: CountryConfig, is_global_raw=False):
        if hasattr(self, "_IS_GLOBAL_RAW"):
            is_global_raw = self._IS_GLOBAL_RAW
        super().__init__(
            country_config,
            datasource_base_dir=self._DATASOURCE_BASENAME,
            is_public=self._IS_PUBLIC,
            is_global_raw=is_global_raw,
        )
        if hasattr(self, "_RAW_FILENAME"):
            self.raw_filepath = self._raw_base_dir / self._RAW_FILENAME
        if hasattr(self, "_PROCESSED_FILENAME"):
            self.processed_filepath = (
                self._processed_base_dir / self._PROCESSED_FILENAME
            )
        # TODO: need a better method to define the exploration_dir
        if hasattr(self, "_EXPLORATION_FILENAME"):
            self._exploration_dir = (
                Path(*Path(self._processed_base_dir).parts[:-3])
                / "exploration"
                / Path(*Path(self._processed_base_dir).parts[-2:])
            )
            self.exploration_filepath = (
                self._exploration_dir / self._EXPLORATION_FILENAME
            )

    def download(self):
        pass

    def process(self):
        pass

    def load(self):
        pass


class FloodScan(_DataSourceExtension):
    _DATASOURCE_BASENAME = "floodscan"
    _RAW_FILENAME = (
        "floodscan_flooded_fraction_africa_19980112-20221231_p00/"
        "aer_sfed_area_300s_19980112_20221231_v05r01.nc"
    )
    _IS_PUBLIC = False
    _IS_GLOBAL_RAW = True

    def load_raw(self, gdf: gpd.GeoDataFrame) -> xr.Dataset:
        with xr.open_dataset(self.raw_filepath) as ds:
            # TODO: what are these things for?
            ds.SFED_AREA.attrs.pop("grid_mapping")
            ds.NDT_SFED_AREA.attrs.pop("grid_mapping")
            ds.LWMASK_AREA.attrs.pop("grid_mapping")
            ds.rio.write_crs("EPSG:4326", inplace=True)
            # Needed for clipping, should be fixed in AnticiPy
            ds = ds.rio.set_spatial_dims(x_dim="lon", y_dim="lat")
            # Clip to GDF area
            ds = ds.oap.clip_box(*gdf.total_bounds)
            return ds

    def process(self, gdf: gpd.GeoDataFrame, feature_col: str):
        da = self.load_raw(gdf)["SFED_AREA"]
        da_stats = da.oap.compute_raster_stats(
            gdf=gdf, feature_col=feature_col
        )
        output_filepath = (
            self._processed_base_dir / f"floodscan_stats_{feature_col}.csv"
        )
        output_filepath.parent.mkdir(parents=True, exist_ok=True)
        logger.info(f"Saving file to {output_filepath}")
        da_stats.to_csv(output_filepath, index=False)

    def load(self, feature_col) -> pd.DataFrame:
        return pd.read_csv(
            self._processed_base_dir / f"floodscan_stats_{feature_col}.csv"
        )


class ChirpsGefs(_DataSourceExtension):
    _DATASOURCE_BASENAME = "chirps_gefs"
    _IS_PUBLIC = True
    _IS_GLOBAL_RAW = False
    _BASE_URL = (
        "https://data.chc.ucsb.edu/products/EWX/data/forecasts/"
        "CHIRPS-GEFS_precip_v12/daily_16day/{run_date}/"
        "data.{forecast_date}.tif"
    )
    _RUN_DATE_FORMAT = "%Y/%m/%d"
    _FORECAST_DATE_FORMAT = "%Y.%m%d"

    def __init__(
        self,
        country_config: CountryConfig,
        adm0: gpd.GeoDataFrame,
        end_date: date,
        start_date: date = date(2000, 1, 1),
        leadtime_max: int = 15,
    ):
        # Add anything here
        self._adm0 = adm0
        self._start_date = start_date
        self._end_date = end_date
        self._date_range = rrule.rrule(
            freq=rrule.DAILY,
            dtstart=self._start_date,
            until=self._end_date,
        )
        self._leadtime_max = leadtime_max
        super().__init__(country_config)

    def download(self, clobber=False):
        """Download the chirps-gefs forecast for the given year and
        day of the year
        """
        raw_dir = self._raw_base_dir / "daily"
        raw_dir.mkdir(parents=True, exist_ok=True)
        for run_date in self._date_range:
            for leadtime in range(self._leadtime_max + 1):
                filename = (
                    f"chirpsgefs_{self._country_config.iso3}_"
                    f"{run_date.strftime('%Y-%m-%d')}_"
                    f"lt{str(leadtime).zfill(2)}d.tif"
                )
                download_filepath = raw_dir / filename
                if not clobber and download_filepath.exists():
                    logger.info(
                        f"{download_filepath} already exists and "
                        f"clobber is False, skipping"
                    )
                    continue
                # TODO: check if exists
                url = self._BASE_URL.format(
                    run_date=run_date.strftime(self._RUN_DATE_FORMAT),
                    forecast_date=(
                        run_date + timedelta(days=leadtime)
                    ).strftime(self._FORECAST_DATE_FORMAT),
                )
                try:
                    with rasterio.open(url) as src:
                        # From here
                        # https://rasterio.readthedocs.io/en/latest/topics/masking-by-shapefile.html
                        out_image, out_transform = rasterio.mask.mask(
                            src, self._adm0.geometry, crop=True
                        )
                        out_meta = src.meta
                except RasterioIOError:
                    logger.warning(
                        f"Url {url} for {run_date} doesn't exist, skipping"
                    )
                    continue
                out_meta.update(
                    {
                        "driver": "GTiff",
                        "height": out_image.shape[1],
                        "width": out_image.shape[2],
                        "transform": out_transform,
                    }
                )
                with rasterio.open(download_filepath, "w", **out_meta) as dest:
                    dest.write(out_image)
                logger.info(
                    f"Downloaded and cropped {url} to {download_filepath}"
                )
