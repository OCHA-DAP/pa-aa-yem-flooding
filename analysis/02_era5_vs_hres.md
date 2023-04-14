# ERA5 and HRES

This notebook compares the total precipitation metric of ERA5 from CDS
with HRES hindcasts from MARS. It was created to share with ECMWF
to present our questions about the data.

Note that for it to run, the two datasets "yem_era5_tp_2022-08.grib2"
and "yem_fc_tp_2022-08-04.grib2" need to be present in the same directory.
This is so that it can be easily shared with these datasets and run by
others if needed.

```python
%load_ext jupyter_black
```

```python
from datetime import datetime

import xarray as xr
```

```python
filename_era5 = "yem_era5_tp_2022-08.grib2"
filename_hres = "yem_fc_tp_2022-08-04.grib2"
# Selecting this day as there was a fair amount of rain in ERA5
compare_date = datetime(2022, 8, 4, 0, 0, 0)
```

## ERA5

First read in the ERA5 data, which was downloaded from
[CDS](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview)
(land hourly data)

```python
# Read in HRES data and select desired date
da_era5 = (
    xr.load_dataset(filename_era5, engine="cfgrib")["tp"]
    .sum(dim="step")
    .sel(time=compare_date)
)
```

A plot of the data shows significant rainfall along the west coast.
My best guess is that the precipitation is presented in mm?

```python
da_era5.plot(vmin=0, vmax=0.3)
```

## HRES

Now let's turn to the HRES forecast, from the previous date but with lead time one,
so hopefully will be a close comparison. Data were downloaded from MARS
using a query such as
[this one](https://apps.ecmwf.int/mars-catalogue/?axis_step=240&axis_param=228.128&stream=oper&levtype=sfc&time=00:00:00&expver=1&month=jan&year=2022&date=2022-01-01&type=fc&class=od).

```python
# Read in the data and select the date
da_hres = xr.load_dataset(filename_hres, engine="cfgrib")["tp"].isel(step=1)
```

Plotting the HRES data, it looks to be about a factor of 10 below ERA5.
So I'm guessing it's in cm? Even though the units say m, which I don't
think is correct.

```python
da_hres.plot(vmin=0, vmax=0.03)
```
