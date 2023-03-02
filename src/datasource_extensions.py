import logging
from pathlib import Path

import geopandas as gpd
import pandas as pd
import xarray as xr
from ochanticipy import CountryConfig
from ochanticipy.datasources.datasource import DataSource

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


class FloodScanStats(_DataSourceExtension):
    def __init__(
        self,
        country_config: CountryConfig,
        adm_level: int,
        is_global_raw=False,
    ):
        self._PROCESSED_FILENAME = f"tcd_floodscan_stats_adm{adm_level}.csv"
        self._DATASOURCE_BASENAME = "floodscan"
        self._IS_PUBLIC = False
        super().__init__(country_config, is_global_raw)

    def load(self) -> pd.DataFrame:
        return pd.read_csv(self.processed_filepath, parse_dates=["time"])
