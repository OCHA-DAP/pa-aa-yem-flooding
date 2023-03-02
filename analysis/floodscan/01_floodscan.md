# Floodscan

Looking at floodscan for Yemen generall, and
comparing to data from CCCM

```python
%load_ext jupyter_black
```

```python
from datetime import timedelta

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
from scipy.interpolate import interp1d

from src import utils, constants
```

## Examine floodscan

Just loopk at admin 0 for now to get a basic idea

```python
# Load the stats for admin 0
fs = utils.load_floodscan_stats(admin_level=0).set_index("time")
fs.index = pd.to_datetime(fs.index)
```

```python
# Plot mean flood fracion for admin 0 across all time
fs["mean"].plot()
```

Now looking to see if there is any annual seasonality to the flood fraction
by taking the mean around a 30 day window for each day of the year.

There are two peaks, but they ar equite small in magnitude (<<0.1% flood fraction)

```python
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

Now compare the floodscan data to flooding events
compiled by the CCCM cluster

```python
# Read in the CCCM Data processed by Zack using R scripts
cccm_filename = (
    constants.oap_data_dir
    / "private/processed/yem/cccm_site_flood_reports_coords_2122.csv"
)
df_cccm = pd.read_csv(cccm_filename)
df_cccm
```

```python
# Get an idea of the column names available
df_cccm.columns
```

```python
# Clean up the CCCM data:
# Only take the columns that we need,
# and clean up the time variable to make it
# easy to compare to Floodscan
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
# To match the figures that Zack made, we want to
# sort the data by the shelters that were most affected
# It's a bit compliated because we need to sum multiple
# events per shelter
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
# Read in the raw floodscan data
fs_raw = utils.load_floodscan_raw()["SFED_AREA"]
```

Now we want to get an idea visually of how the floodscan data
compares to the CCCM flood event locations. To do this, we
plot the maximum value at each floodscan pixel (just to get an idea
of which pixels never had any flood fraction), and overlay the
shelter locations with an x.

From this plot, we can see that floodscan covers many shelter
locations, but not all, so we need to be checking.

```python
cmap = mpl.cm.get_cmap("OrRd").copy()
cmap.set_under(color="black")
fig, ax = plt.subplots()
fs_raw.max(dim="time").plot(vmin=0.01, ax=ax, cmap=cmap)

ax.scatter(df_cccm.lon, df_cccm.lat, marker="x")
```

Next wee want to check if floodscan is crossing a particular threshold
around the time of a CCCM data event. For this we need the return period.
At the moment we're just calculating it empirically, i.e. interpolating
between past event values. If we narrow down the list of locations we can
do a more careful GEV fit.

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
# Set up for loop
time_window_size = 60  # days
pixel_window_size = 0.5  # deg
return_period_target = 1.5
rp_min = 0.01
hits = []
plot = False
```

```python
# Loop through all the CCCM events
for i, row in df_cccm.iterrows():
    # Get the floodscan area associated with the event
    # by taking the nearset pixels
    fs_event_area = fs_raw.sel(
        lon=slice(
            row.lon - pixel_window_size / 2, row.lon + pixel_window_size / 2
        ),
        lat=slice(
            row.lat + pixel_window_size / 2,
            row.lat - pixel_window_size / 2,
        ),
    ).mean(dim=["lat", "lon"])
    # Check whether flood fraction is always 0 - if so, skip
    if fs_event_area.max() == 0:
        print(f"Skipping {row.site_name} because floodscan is 0")
        continue
    # Get the floodscan return period
    input_to_rp = (
        fs_event_area.to_dataframe()[["SFED_AREA"]].resample("1y").agg("max")
    )
    return_period_function = get_return_period_function_empirical(
        input_to_rp, "SFED_AREA"
    )
    return_period_value = return_period_function(return_period_target)
    if return_period_value < 0.01:
        print(f"Skipping {row.site_name} because RP is {return_period_value}")
        continue
    # Restrict floodscan data to time window
    d1, d2 = row.time - timedelta(
        days=time_window_size / 2
    ), row.time + timedelta(days=time_window_size / 2)
    fs_event_area_time = fs_event_area.sel(time=slice(d1, d2))
    # Plot
    if plot:
        fig, ax = plt.subplots(figsize=(5, 2))
        fs_event_area_time.plot(ax=ax)
        # ff.plot(ax=ax)
        ax.axhline(rp, c="r")
        ax.set_title(f"{row.site_name}")
        ax.axvline(row.time, c="k")
        ax.set_ylim(-0.01, 0.10)
    # Add a boolean to the dataframe whether or not
    # the floodscan data crossed the return period threshold
    df_cccm.loc[i, f"hit_{return_period}"] = (
        (fs_event_area_time > return_period_value).any().values
    )
```

```python
# Print percentage of CCCM events where floodscan
# crossed the return period
sum(df_cccm["hit_1.5"].dropna()) / len(df_cccm["hit_1.5"].dropna()) * 100
```

```python
# Print the total number of CCCM events with floodscan data
len(df_cccm["hit_1.5"].dropna())
```
