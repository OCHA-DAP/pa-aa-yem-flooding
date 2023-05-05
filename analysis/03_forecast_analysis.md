# ECMWF forecast analysis

Analyzing HRES hindcast downloaded from MARS
(and also using ERA5 historical).

The HRES data was created by hand from the R target
`ecmwf_mars_high_risk_hulls` (hres.csv).

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
# Governorate names
GOVS = ["Hajjah", "Marib"]
RP = 2
```

```python
# Read in the data
era5_data_dir = constants.oap_data_dir / "public/processed/yem/ecmwf/"
df_era5 = pd.read_csv(era5_data_dir / "era5_high_risk_hulls.csv")

hres_data_dir = (
    constants.oap_data_dir / "private/processed/yem/targets_20230407/csv/"
)
df_hres = pd.read_csv(hres_data_dir / "hres.csv")
```

```python
# Clean the dataframes a bit
# In both cases convert m to mm (x by 1000)
df_era5_dict = {
    gov: df_era5.loc[df_era5["gov"] == gov][["time", "value"]]
    .rename(columns={"value": "era5"})
    .set_index("time")
    .multiply(1000)
    for gov in GOVS
}

df_hres["mean"] = df_hres["mean"] * 1000
df_hres_dict = {
    gov: df_hres.loc[df_hres["governorate_name"] == gov][["date", "mean"]]
    for gov in GOVS
}
```

## ERA5 return period

```python
# For era5 need to conver tot 3 day rolling
for gov in GOVS:
    df = df_era5_dict[gov]
    df.index = pd.to_datetime(df.index).to_period("D")
    df = df.rolling(3).sum().shift(-1).dropna()
    df_era5_dict[gov] = df
```

```python
# Then let's look quickly at the ERA5 data, for Hajjah
df_era5_dict[GOVS[0]].plot(rot=90, title=GOVS[0])
```

```python
df_era5_dict[GOVS[1]].plot(rot=90, title=GOVS[1])
```

A couple of potential issues: The data don't seem
as smooth as with CHIRPS, and the peak rainfall in the last
3 years does not appear to be present for Marib.

Next, on to RP calculation.

```python
show_plots = True

rp_dict = {}
for gov in GOVS:
    df = df_era5_dict[gov].copy()
    df = df.resample(rule="A", kind="period").max().sort_values(by="era5")
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df, rp_var="era5", show_plots=show_plots
    )
    rp_dict[gov] = float(rp_func(RP))

rp_dict
```

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
df = df.pivot(columns="step", values="mean", index="date").diff(axis=1)
df
```

```python
# Next do the rolling 3-day sum, but across columns
# Doing by hand because I don't know how to apply rolling
df = df.rolling(3, axis=1).sum().drop(columns=[0, 1, 2])
# Shift column names back
df.columns -= 2
# Finally, shift the leadtimes
for n in range(8):
    df[n + 1] = df[n + 1].shift(n)
df
```

### Apply to all HRES data

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
    df = df.pivot(columns="step", values="mean", index="date").diff(axis=1)
    # Next do the rolling 3-day sum, but across columns
    # Doing by hand because I don't know how to apply rolling
    df = df.rolling(3, axis=1).sum().drop(columns=[0, 1, 2])
    # Shift column names back
    df.columns -= 2
    # Finally, shift the leadtimes
    for n in range(8):
        df[n + 1] = df[n + 1].shift(n)
    df.index = pd.to_datetime(df.index).to_period("D")
    df_hres_proc_dict[gov] = df
```

### Get HRES return periods

```python
rp_dict_hres = {}
show_plots = False
for gov in GOVS:
    rp_dict_hres_leadtime = {}
    df = df_hres_proc_dict[gov].copy()
    # Recalculate ERA5 RP with new dates
    df_era5 = df_era5_dict[gov].copy()
    df_era5 = df_era5[df_era5.index >= df.index[0]]
    df_era5 = (
        df_era5.resample(rule="A", kind="period").max().sort_values(by="era5")
    )
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df_era5, rp_var="era5", show_plots=show_plots
    )
    rp_dict_hres_leadtime["era5"] = float(rp_func(RP))
    # Now do HRES
    df = df.resample(rule="A", kind="period").max()  # .sort_values(by="era5")
    for leadtime in range(1, 9):
        rp_func = utils.get_return_period_function_analytical(
            df_rp=df, rp_var=leadtime, show_plots=show_plots
        )
        rp_dict_hres_leadtime[leadtime] = float(rp_func(RP))
    rp_dict_hres[gov] = rp_dict_hres_leadtime

rp_dict_hres
```

## Forecast analysis

Start by merging data frames

```python
df_comb_dict = {}
for gov in GOVS:
    obs = df_era5_dict[gov].copy()
    fc = df_hres_proc_dict[gov].copy()
    df_comb = pd.merge(obs, fc, left_index=True, right_index=True)
    df_comb.index = df_comb.index.to_timestamp()
    df_comb_dict[gov] = df_comb

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

target_months = [5, 6, 7, 8, 9]

leadtimes = np.arange(1, 9)
for gov in GOVS:
    for minval in [1, rp_dict[gov]]:
        for limit_months in [True, False]:
            df = df_comb_dict[gov].copy()
            if limit_months:
                df[~df.index.month.isin(TARGET_MONTHS)] = np.NaN
            df = df[df["era5"] > minval]
            for quantity in ["mae", "bias"]:
                df_results = pd.concat(
                    (
                        df_results,
                        pd.DataFrame(
                            dict(
                                values=[
                                    func_dict[quantity](
                                        df["era5"], df[leadtime]
                                    ).mean()
                                    for leadtime in leadtimes
                                ],
                                gov=gov,
                                leadtime=leadtimes,
                                quantity=quantity,
                                minval=minval,
                                limit_months=limit_months,
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
    for (gov, minval, limit_months), group in df.groupby(
        ["gov", "minval", "limit_months"]
    ):
        label = f"{gov}, tp > {minval:.1f} mm"
        if limit_months:
            label += ", May-Sep"
        ax.plot(group["leadtime"], group["values"], label=label)
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
days_before_buffer = 15
days_after_buffer = 15

df_event_stats = pd.DataFrame()
leadtimes = np.arange(1, 9)

for gov in GOVS:
    df = df_comb_dict[gov].copy()
    rp_value = rp_dict[gov]
    # Get the model and dates
    model = df["era5"]
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
    ax2.set_ylabel("Fraction")
    ax.set_title(gov)
```

```python
# Make plots of what happens when crossing thresholds
min_duration = 1  # day
leadtimes = np.arange(1, 9)
xrange = np.timedelta64(15, "D")


for gov in GOVS:
    df = df_comb_dict[gov].copy()
    rp_value = rp_dict[gov]
    # Get the model and dates
    for desired_events, title in zip(
        (df["era5"], df[5]), ("ERA5", "HRES: LT=5d")
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
            df["era5"].plot(ax=ax, c="k", lw=2)
            df[[1, 2, 3, 4, 5, 6, 7, 8]].plot(ax=ax, lw=0.5)
            ax.set_title(f"{gov}, event from {title}")
            plt.show()
```

```python

```
