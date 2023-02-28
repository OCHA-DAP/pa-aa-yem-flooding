# Floodscan

Looking at floodscan for Yemen generall, and
comparing to CCCM

```python
%load_ext jupyter_black
```

```python
from datetime import timedelta

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
from scipy.interpolate import interp1d

from src import utils, constants
```

## Examine stats

Just loopk at admin 0 for now to get an idea

```python
fs = utils.load_floodscan_stats(admin_level=0).set_index("time")
fs.index = pd.to_datetime(fs.index)
```

```python
fs["mean"].plot()
```

```python
# Looking for seasonality, use a 30 day smoothing
# It's there but extremely small
pv = pd.pivot_table(
    fs.rolling(window=30).mean()[["mean"]],
    index=fs.index.dayofyear,
    columns=fs.index.year,
).mean(axis=1)
fig, ax = plt.subplots()
pv.plot(alpha=0.5, ax=ax)
ax.set_xlabel("day of year")
ax.set_ylabel("flood fraction")
```

## CCCM comparison

```python
# CCCM Data processed by Zack in R scripts
cccm_filename = (
    constants.oap_data_dir
    / "private/processed/yem/cccm_site_flood_reports_coords_2122.csv"
)
df_cccm = pd.read_csv(cccm_filename)
df_cccm
```

```python
df_cccm.columns
```

```python
# Create a time variable
df_cccm = df_cccm[
    [
        "date_of_episode",
        "lat",
        "lon",
        "site_id",
        "site_name",
        "num_shelters_affected",
    ]
]
df_cccm["time"] = [
    i.replace(tzinfo=None)
    for i in pd.to_datetime(df_cccm["date_of_episode"], utc=False)
]
df_cccm
```

```python
# Sort by number of shelters affected
total_shelters_dict = (
    df_cccm.groupby("site_id")[["num_shelters_affected"]]
    .sum()
    .sort_values(by="num_shelters_affected")
    .to_dict()
)["num_shelters_affected"]
df_cccm["num_shelters_affected_site"] = df_cccm["site_id"].map(
    total_shelters_dict
)
df_cccm = df_cccm.sort_values(by=["site_id", "date_of_episode"]).sort_values(
    by=["num_shelters_affected_site", "site_id"], ascending=False
)
df_cccm
```

```python
# Raw floodscan data
fs_raw = utils.load_floodscan_raw()["SFED_AREA"]
```

```python
# Plot the shelters over floodscan data max values

cmap = mpl.cm.get_cmap("OrRd").copy()
cmap.set_under(color="black")
fig, ax = plt.subplots()
fs_raw.max(dim="time").plot(vmin=0.01, ax=ax, cmap=cmap)

ax.scatter(df_cccm.lon, df_cccm.lat, marker="x")
```

```python
def get_return_period_function_empirical(df_rp: pd.DataFrame, rp_var: str):
    """
    :param df_rp: DataFrame where the index is the year, and the rp_var
    column contains the maximum value per year
    :param rp_var: The column
    with the quantity to be evaluated
    :return: Interpolated function
    that gives the quantity for a give return period
    """
    df_rp = df_rp.sort_values(by=rp_var, ascending=False)
    n = len(df_rp)
    df_rp["rank"] = np.arange(n) + 1
    df_rp["exceedance_probability"] = df_rp["rank"] / (n + 1)
    df_rp["rp"] = 1 / df_rp["exceedance_probability"]
    return interp1d(df_rp["rp"], df_rp[rp_var])
```

```python
lon_slice = slice(
    row.lon - pixel_window_size / 2, row.lon + pixel_window_size / 2
)
lat_slice = slice(
    row.lat + pixel_window_size / 2, row.lat - pixel_window_size / 2
)
fs_raw.sel(lat=lat_slice, lon=lon_slice).mean(dim=["lat", "lon"]).max()
```

```python
time_window_size = 60  # days
pixel_window_size = 0.5  # deg
return_period = 1.5
rp_min = 0.01
hits = []

plot = False


for i, row in df_cccm.iterrows():
    # Get the area associated with the event
    ff = fs_raw.sel(
        lon=slice(
            row.lon - pixel_window_size / 2, row.lon + pixel_window_size / 2
        ),
        lat=slice(
            row.lat + pixel_window_size / 2,
            row.lat - pixel_window_size / 2,
        ),
    )
    # print(len(ff.lat))
    ff = ff.mean(dim=["lat", "lon"])
    # Check area is always 0 - if so, skip
    if ff.max() == 0:
        print(f"Skipping {row.site_name} because floodscan is 0")
        continue
    # Get return period
    input_to_rp = ff.to_dataframe()[["SFED_AREA"]].resample("1y").agg("max")
    rpf = get_return_period_function_empirical(input_to_rp, "SFED_AREA")
    rp = rpf(return_period)
    if rp < 0.01:
        print(f"Skipping {row.site_name} because RP is {rp}")
        continue
    # Get the time window
    d1, d2 = row.time - timedelta(
        days=time_window_size / 2
    ), row.time + timedelta(days=time_window_size / 2)
    ff_window = ff.sel(time=slice(d1, d2))
    # Plot
    if plot:
        fig, ax = plt.subplots(figsize=(5, 2))
        ff_window.plot(ax=ax)
        # ff.plot(ax=ax)
        ax.axhline(rp, c="r")
        ax.set_title(f"{row.site_name}")
        ax.axvline(row.time, c="k")
        ax.set_ylim(-0.01, 0.10)
    # Add to data frame
    df_cccm.loc[i, f"hit_{return_period}"] = (ff_window > rp).any().values
```

```python
sum(df_cccm["hit_1.5"].dropna()) / len(df_cccm["hit_1.5"].dropna()) * 100
```

```python
len(df_cccm["hit_1.5"].dropna())
```
