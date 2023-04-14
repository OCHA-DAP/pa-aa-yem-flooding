# ECMWF forecast analysis

Analyzing HRES hindcast downloaded from MARS
(and also using ERA5 historical).

Using two csvs that were crated by hand from the R targets:
`ecmwf_mars_high_risk_hulls` (hres.csv) and `era5_w_rolling` (era5.csv).

```python
%load_ext jupyter_black
```

```python
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from src import constants, utils
```

```python
from importlib import reload

reload(utils)
```

```python
# Governorate names
GOVS = ["Hajjah", "Marib"]
RP = 2
```

```python
# Read in the data
data_dir = (
    constants.oap_data_dir / "private/processed/yem/targets_20230407/csv/"
)
df_era5 = pd.read_csv(data_dir / "era5.csv")
df_hres = pd.read_csv(data_dir / "hres.csv")
```

```python
# Clean the dataframes a bit
df_era5_dict = {
    gov: df_era5.loc[df_era5["governorate_name"] == gov][
        ["date", "era_roll3"]
    ].set_index("date")
    for gov in GOVS
}
df_hres_dict = {
    gov: df_hres.loc[df_hres["governorate_name"] == gov][["date", "mean"]]
    for gov in GOVS
}
```

## ERA5 return period

```python
# First let's look quickly at the ERA5 data, for Hajjah
df_era5_dict[GOVS[0]].plot(rot=90, title=GOVS[0])
```

```python
df_era5_dict[GOVS[1]].plot(rot=90, title=GOVS[1])
```

A bit concerned about these plots. The data don't seem
as smooth as with CHIRPS, and the peak rainfall in the last
3 years does not appear to be present.

The units appear to be mm.

Next, on to RP calculation.

```python
show_plots = True

rp_dict = {}
for gov in GOVS:
    df = df_era5_dict[gov].copy()
    df.index = pd.to_datetime(df.index).to_period()
    df = df.resample(rule="A", kind="period").max().sort_values(by="era_roll3")
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df, rp_var="era_roll3", show_plots=show_plots
    )
    rp_dict[gov] = float(rp_func(RP))

rp_dict
```

For Hajjah the RP value is similar (40 vs I think ~30 or 35 mm
for CHIRPS) but Marib is much lower.

## Processing HRES

The HRES data need a bit more procesing, it turns out that the numbers are cumulative,
so here I'm starting from the non-rolled values.

### Example for a single governorate

```python
df = df_hres_dict[GOVS[0]].copy()
# First step is to split up the date column
df[["date", "step"]] = (
    df["date"].str.strip("yem_fc_tp").str.split("_tp_step=", expand=True)
)
# Convert step to days
df["step"] = (pd.to_numeric(df["step"]) / 24).astype(int)
# Convert columns to leadtime,
# for each lead time, subtract previous, since it's cumulative,
# then multiply by 1000 (I think Zack must have changed the units?)
df = (
    df.pivot(columns="step", values="mean", index="date")
    .diff(axis=1)
    .multiply(1000)
)
df
```

```python
# Make sure the stats make sense
df.describe()
```

The increasing means are a bit concerning, but not sure what else to do.
Waiting to hear back from ECMWF.

```python
# Next do the rolling 3-day sum, but across columns
# Doing by hand because I don't know how to apply rolling
df = df.rolling(3, axis=1).sum().drop(columns=[0, 1, 2])
# Shift column names back
df.columns -= 2
# Finally, shift the leadtimes
for n in range(8):
    df[n + 1] = df[n + 1].shift(n + 1)
df
```

## Apply to all HRES data

```python
# Now apply the above to the hres data
df_hres_proc_dict = {}

for gov in GOVS:
    df = df_hres_dict[gov].copy()
    # First step is to split up the date column
    df[["date", "step"]] = (
        df["date"].str.strip("yem_fc_tp").str.split("_tp_step=", expand=True)
    )
    # Convert step to days
    df["step"] = (pd.to_numeric(df["step"]) / 24).astype(int)
    # Convert columns to leadtime,
    # for each lead time, subtract previous, since it's cumulative,
    # then multiply by 1000 (I think Zack must have changed the units?)
    df = (
        df.pivot(columns="step", values="mean", index="date")
        .diff(axis=1)
        .multiply(1000)
    )
    # Next do the rolling 3-day sum, but across columns
    # Doing by hand because I don't know how to apply rolling
    df = df.rolling(3, axis=1).sum().drop(columns=[0, 1, 2])
    # Shift column names back
    df.columns -= 2
    # Finally, shift the leadtimes
    for n in range(8):
        df[n + 1] = df[n + 1].shift(n + 1)
    df_hres_proc_dict[gov] = df
```

```python
df_hres_proc_dict[GOVS[0]][1].plot()
```

## Forecast analysis

Start by merging data frames

```python
df_comb_dict = {}
for gov in GOVS:
    obs = df_era5_dict[gov].copy()
    fc = df_hres_proc_dict[gov].copy()
    obs.index = pd.to_datetime(obs.index)
    fc.index = pd.to_datetime(fc.index)
    df_comb_dict[gov] = pd.merge(obs, fc, left_index=True, right_index=True)
df_comb_dict
```

## General forecast skill and bias

As a function of leadtime, look at the mean absolute error
of the forecast compared to the model.

```python
def calc_bias(obs, fc):
    return (obs - fc) / obs * 100


def calc_mae(obs, fc):
    return np.abs((obs - fc)) / obs * 100


func_dict = {"mae": calc_mae, "bias": calc_bias}

df_results = pd.DataFrame(
    columns=["gov", "leadtime", "values", "quantity", "minval"]
)

leadtimes = np.arange(1, 9)
for gov in GOVS:
    df = df_comb_dict[gov].copy()
    for minval in [1, rp_dict[gov]]:
        df = df[df["era_roll3"] > minval]
        for quantity in ["mae", "bias"]:
            df_results = pd.concat(
                (
                    df_results,
                    pd.DataFrame(
                        dict(
                            values=[
                                func_dict[quantity](
                                    df["era_roll3"], df[leadtime]
                                ).mean()
                                for leadtime in leadtimes
                            ],
                            gov=gov,
                            leadtime=leadtimes,
                            quantity=quantity,
                            minval=minval,
                        )
                    ),
                ),
                ignore_index=True,
            )
```

```python
# Make some plots

for quantity in ["mae", "bias"]:
    fig, ax = plt.subplots()
    df = df_results[df_results["quantity"] == quantity]
    for (gov, minval), group in df.groupby(["gov", "minval"]):
        ax.plot(
            group["leadtime"],
            group["values"],
            label=f"{gov}, tp > {minval:.1f} mm",
        )
    ax.legend()
    ax.set_title(quantity)
    ax.set_xlabel("lead time (days)")
    ax.set_ylabel("percent error")
```

### Forecast events

Use both the observation and forecast 1-in-2 year threshold to
find events and compare performance

```python
# Loop through lead times
min_duration = 1  # day
days_before_buffer = 5
days_after_buffer = 1

df_event_stats = pd.DataFrame()
leadtimes = np.arange(1, 9)

for gov in GOVS:
    df = df_comb_dict[gov].copy()
    rp_value = rp_dict[gov]
    # Get the model and dates
    model = df["era_roll3"]
    model_dates = utils.get_dates_list_from_data_series(
        model, rp_value, min_duration=min_duration
    )
    for leadtime in leadtimes:
        # Get the forecast at a specific lead time and dates
        forecast = df[leadtime]
        forecast_dates = utils.get_dates_list_from_data_series(
            forecast, rp_value, min_duration=min_duration
        )
        # Match the dates to events
        detection_stats = utils.get_detection_stats(
            true_event_dates=model_dates,
            forecasted_event_dates=forecast_dates,
            days_before_buffer=days_before_buffer,
            days_after_buffer=days_after_buffer,
        )
        df_event_stats = pd.concat(
            (
                df_event_stats,
                pd.DataFrame(
                    {
                        **{"gov": gov, "leadtime": leadtime},
                        **detection_stats,
                    },
                    index=[0],
                ),
            ),
            ignore_index=True,
        )
df_event_stats = utils.get_more_detection_stats(df_event_stats)
df_event_stats
```

```python
cdict = {
    "TP": "tab:olive",
    "FP": "tab:orange",
    "FN": "tab:red",
    "POD": "tab:blue",
    "FAR": "tab:cyan",
}

for gov in GOVS:
    fig, ax = plt.subplots()
    ax2 = ax.twinx()
    df = df_event_stats[df_event_stats["gov"] == gov]
    lines1, lines2 = [], []
    for stat in ["TP", "FP", "FN"]:
        lines1 += ax.plot(df["leadtime"], df[stat], c=cdict[stat], label=stat)
    for stat in ["POD", "FAR"]:
        lines2 += ax2.plot(df["leadtime"], df[stat], c=cdict[stat], label=stat)
    lines = lines1 + lines2
    labels = [line.get_label() for line in lines]
    ax.legend(lines, labels)
    ax.set_ylim(-1, 60)
    ax2.set_ylim(-0.03, 1.03)
    ax.set_ylabel("Number")
    ax.set_xlabel("Lead time [days]")
    ax2.set_ylabel("POD / FAR")
    ax.set_title(gov)
```
