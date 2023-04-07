import os
from datetime import date
from pathlib import Path

import numpy as np
import xarray as xr
from dateutil import rrule
from scipy.interpolate import griddata

clobber = False

start_date = date(2007, 1, 1)
# TODO: check date 2020-01-01
# start_date = date(2020, 1, 2)
end_date = date(2022, 12, 31)

date_range = rrule.rrule(
    freq=rrule.DAILY,
    dtstart=start_date,
    until=end_date,
)

# Read in template dataset
template_filename = (
    Path(os.environ["OAP_DATA_DIR"]) / "private/raw/yem/ecmwf/"
    "yem_fc_tp_2007-01-01.grib2"
)
ds_template = xr.load_dataset(template_filename)

for forecast_date in date_range:
    print(forecast_date)
    forecast_date_string = forecast_date.strftime("%Y-%m-%d")
    input_filename = (
        Path(os.environ["OAP_DATA_DIR"])
        / f"private/raw/yem/ecmwf/strange_coords/"
        f"yem_fc_tp_{forecast_date_string}.grib2"
    )
    output_filename = (
        Path(os.environ["OAP_DATA_DIR"])
        / f"private/processed/yem/ecmwf/strange_coords/"
        f"yem_fc_tp_{forecast_date_string}.nc"
    )
    output_filename.parents[0].mkdir(parents=True, exist_ok=True)
    if output_filename.exists():
        if clobber:
            output_filename.unlink()
        else:
            print(f"{forecast_date_string} exists, skipping")
            continue

    da = xr.load_dataset(input_filename, engine="cfgrib")["tp"]
    steps = (da.step.values / np.timedelta64(1, "D")).astype(int)

    x, y = np.arange(42.0, 55.1, 0.1), np.arange(12.1, 19.1, 0.1)
    y = y[::-1]
    xi = tuple(np.meshgrid(x, y))
    new_values = np.empty((len(steps), len(y), len(x)))

    for istep in steps:
        da_step = da.isel(step=istep)
        points = (da_step.longitude.values, da_step.latitude.values)
        values = da_step.values
        new_values[istep] = griddata(points, values, xi)

    ds = ds_template.copy(deep=True)
    ds.coords["time"] = forecast_date
    ds.coords["valid_time"] = ds.time + ds.step
    ds["tp"].values = new_values

    ds.to_netcdf(output_filename)
