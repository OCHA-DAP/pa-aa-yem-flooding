# CHIRPS-GEFS forecast analysis

Analyzing CHIRPS-GEFS

Using `chirps-gefs_daily_high_risk_hulls` and `chirps`.


```python
%load_ext jupyter_black
```


```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os

os.chdir("..")
from src import constants, utils
```


```python
# Read in the data
data_dir = constants.oap_data_dir
chirps_dir = data_dir / "private/processed/yem/targets_20230407/csv/"
chirps_gefs_dir = data_dir / "public/processed/yem/chirps_gefs/"
df_chirps = pd.read_csv(chirps_dir / "chirps.csv")
df_chirps_gefs = pd.read_csv(
    chirps_gefs_dir / "chirps-gefs_daily_high_risk_hulls.csv"
)
```


```python
# Governorate namess
GOVS = ["Hajjah", "Marib"]
RP = 2
```


```python
# setting code to run for entire year or for only the period from May to September
may_to_sept = False  # set to False to analyse the full year of data or True for only specific months
```


```python
df_chirps
```


```python
df_chirps_gefs
```


```python
mon_list = list(range(5, 10))
if may_to_sept:
    df_chirps["date"] = pd.to_datetime(df_chirps["date"])
    df_chirps["month"] = df_chirps["date"].dt.month
    df_chirps["mean"] = df_chirps["mean"].where(
        df_chirps["month"].isin(mon_list), None
    )
    df_chirps_gefs["time"] = pd.to_datetime(df_chirps_gefs["time"])
    df_chirps_gefs["month"] = df_chirps_gefs["time"].dt.month
    df_chirps_gefs["value"] = df_chirps_gefs["value"].where(
        df_chirps_gefs["month"].isin(mon_list), None
    )
df_chirps_gefs
```


```python
# Clean the dataframes a bit
df_chirps_dict = {
    gov: df_chirps.loc[df_chirps["governorate_name"] == gov][
        ["date", "mean"]
    ].set_index("date")
    for gov in GOVS
}
df_chirpsgefs_dict = {
    gov: df_chirps_gefs.loc[df_chirps_gefs["gov"] == gov][
        ["time", "leadtime", "value"]
    ]
    for gov in GOVS
}
```


```python
df_chirpsgefs_dict
```

## CHIRPS Data


```python
df_chirps_dict[GOVS[0]].plot(rot=90, title=GOVS[0])
```


```python
df_chirps_dict[GOVS[1]].plot(rot=90, title=GOVS[1])
```

## CHIRPS Return Period


```python
show_plots = True

rp_chirps_dict = {}
for gov in GOVS:
    df = df_chirps_dict[gov].copy()
    df.index = pd.to_datetime(df.index).to_period()
    df = df.resample(rule="A", kind="period").max().sort_values(by="mean")
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df, rp_var="mean", show_plots=show_plots
    )
    rp_chirps_dict[gov] = float(rp_func(RP))

rp_chirps_dict
```


```python
# Now apply the above to the hres data
df_chirpsgefs_proc_dict = {}

for gov in GOVS:
    df = df_chirpsgefs_dict[gov].copy()
    df = df.pivot(columns="leadtime", values="value", index="time")
    # Next do the rolling 3-day sum, but across columns
    # Doing by hand because I don't know how to apply rolling
    df = df.rolling(3, axis=1).sum().drop(columns=[1, 2])
    # Shift column names back
    df.columns -= 2
    # Finally, shift the leadtimes
    for n in range(8):
        df[n + 1] = df[n + 1].shift(n + 1)
    df_chirpsgefs_proc_dict[gov] = df
```


```python
df_chirpsgefs_proc_dict[gov]
```


```python
df_chirpsgefs_proc_dict[GOVS[0]][1].plot(rot=90, title=GOVS[0])
```

## CHIRPS-GEFS Return Period


```python
### Get CHIRPS-GEFS return periods

rp_dict_chirpsgefs = {}
show_plots = False
for gov in GOVS:
    rp_dict_chirpsgefs_leadtime = {}
    df = df_chirpsgefs_proc_dict[gov].copy()
    # Recalculate CHIRPS RP with new dates
    df_chirps = df_chirps_dict[gov].copy()
    df_chirps = df_chirps[df_chirps.index >= df.index[0]]
    df_chirps.index = pd.to_datetime(df_chirps.index).to_period()
    df_chirps = (
        df_chirps.resample(rule="A", kind="period")
        .max()
        .sort_values(by="mean")
    )
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df_chirps, rp_var="mean", show_plots=show_plots
    )
    rp_dict_chirpsgefs_leadtime["chirps"] = float(rp_func(RP))
    # Now do CHIRPS-GEFS
    df.index = pd.to_datetime(df.index).to_period("D")
    df = df.resample(rule="A", kind="period").max()
    for leadtime in range(1, 9):
        rp_func = utils.get_return_period_function_analytical(
            df_rp=df, rp_var=leadtime, show_plots=show_plots
        )
        rp_dict_chirpsgefs_leadtime[leadtime] = float(rp_func(RP))
    rp_dict_chirpsgefs[gov] = rp_dict_chirpsgefs_leadtime

rp_dict_chirpsgefs
```


```python
df_comb_dict = {}
for gov in GOVS:
    obs = df_chirps_dict[gov].copy()
    fc = df_chirpsgefs_proc_dict[gov].copy()
    obs.index = pd.to_datetime(obs.index)
    fc.index = pd.to_datetime(fc.index)
    df_comb_dict[gov] = pd.merge(obs, fc, left_index=True, right_index=True)
df_comb_dict
```

## Forecast Skill Analysis


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
    for minval in [1, rp_chirps_dict[gov]]:
        df = df[df["mean"] > minval]
        for quantity in ["mae", "bias"]:
            df_results = pd.concat(
                (
                    df_results,
                    pd.DataFrame(
                        dict(
                            values=[
                                func_dict[quantity](
                                    df["mean"], df[leadtime]
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


```python
# Loop through lead times
min_duration = 1  # day
days_before_buffer = 15
days_after_buffer = 15

df_event_stats = pd.DataFrame()
leadtimes = np.arange(1, 9)

for gov in GOVS:
    df = df_comb_dict[gov].copy()
    rp_value = rp_chirps_dict[gov]
    # Get the model and dates
    model = df["mean"]
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
    "precision": "tab:blue",
    "recall": "tab:cyan",
    "F1": "tab:green",
}

for gov in GOVS:
    fig, ax = plt.subplots()
    ax2 = ax.twinx()
    df = df_event_stats[df_event_stats["gov"] == gov]
    lines1, lines2 = [], []
    for stat in ["TP", "FP", "FN"]:
        lines1 += ax.plot(df["leadtime"], df[stat], c=cdict[stat], label=stat)
    for stat in ["precision", "recall", "F1"]:
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

## Checking events in CHIRPS and CHIRPS-GEFS


```python
# Make plots of what happens when crossing thresholds
min_duration = 1  # day
leadtimes = np.arange(1, 9)
xrange = np.timedelta64(15, "D")


for gov in GOVS:
    df = df_comb_dict[gov].copy()
    rp_value = rp_chirps_dict[gov]
    # Get the model and dates
    for desired_events, title in zip(
        (df["mean"], df[5]), ("CHIRPS", "CHIRPS-GEFS: LT=5d")
    ):
        event_dates = utils.get_dates_list_from_data_series(
            desired_events, rp_value, min_duration=min_duration
        )
        for event_date in event_dates:
            fig, ax = plt.subplots()
            ax.set_xlim(
                pd.Timestamp(event_date - xrange),
                pd.Timestamp(event_date + xrange),
            )
            ax.axhline(rp_value, c="k", lw=1)
            ax.axvline(event_date, c="k", lw=0.5)
            df["mean"].plot(ax=ax, c="k", lw=2)
            df[[1, 2, 3, 4, 5, 6, 7, 8]].plot(ax=ax, lw=0.5)
            ax.set_title(f"{gov}, event from {title}")
            plt.show()
```
