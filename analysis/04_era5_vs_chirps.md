# ERA5 vs CHIRPS

Checking how well the ECMWF rainfall model, ERA5,
corresponds with CHIRPS rainfall.

Using csv that were crated by hand from the R targets:
`zonal_stats_high_risk_hulls` (chirps.csv)

```python
%load_ext jupyter_black
```

```python
import pandas as pd

from src import constants, utils
```

```python
# Governorate namess
GOVS = ["Hajjah", "Marib"]
RP = 2
```

```python
# Read in the data

# Read in the data
era5_data_dir = constants.oap_data_dir / "public/processed/yem/ecmwf/"
df_era5 = pd.read_csv(era5_data_dir / "era5_high_risk_hulls.csv")


chirps_data_dir = (
    constants.oap_data_dir / "private/processed/yem/targets_20230407/csv/"
)
df_chirps = pd.read_csv(chirps_data_dir / "chirps.csv")
```

```python
# Clean the dataframes a bit
df_era5_dict = {
    gov: df_era5.loc[df_era5["gov"] == gov][["time", "value"]]
    .rename(columns={"value": "era5"})
    .set_index("time")
    .multiply(1000)
    for gov in GOVS
}

df_chirps_dict = {
    gov: df_chirps.loc[df_chirps["governorate_name"] == gov][
        ["date", "mean"]
    ].set_index("date")
    for gov in GOVS
}
```

## Return periods

### ERA5

```python
# For era5 need to conver tot 3 day rolling
for gov in GOVS:
    df = df_era5_dict[gov]
    df.index = pd.to_datetime(df.index).to_period("D")
    df = df.rolling(3).sum().shift(1).dropna()
    df_era5_dict[gov] = df
```

```python
df_era5_dict
```

```python
show_plots = True

rp_era5_dict = {}
for gov in GOVS:
    df = df_era5_dict[gov].copy()
    df = df.resample(rule="A", kind="period").max().sort_values(by="era5")
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df, rp_var="era5", show_plots=show_plots
    )
    rp_era5_dict[gov] = float(rp_func(RP))

rp_era5_dict
```

### CHIRPS

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

## Event comparison

```python
# First combine the two dataframes
df_comb_dict = {}
for gov in GOVS:
    obs = df_chirps_dict[gov].copy().rename(columns={"mean": "chirps"})
    fc = df_era5_dict[gov].copy().rename(columns={"era_roll3": "era5"})
    obs.index = pd.to_datetime(obs.index)
    fc.index = fc.index.to_timestamp()
    df_comb_dict[gov] = pd.merge(
        obs, fc, left_index=True, right_index=True
    ).dropna()
df_comb_dict
```

```python
# Loop through lead times
min_duration = 1  # day
days_before_buffer = 10
days_after_buffer = 10

df_event_stats = pd.DataFrame()

for gov in GOVS:
    df = df_comb_dict[gov].copy()
    # Get the model and dates
    rp_value_obs = rp_chirps_dict[gov]
    obs = df["chirps"]
    obs_dates = utils.get_dates_list_from_data_series(
        obs, rp_value_obs, min_duration=min_duration
    )
    # Get the forecast and dates
    rp_value_model = rp_era5_dict[gov]
    model = df["era5"]
    model_dates = utils.get_dates_list_from_data_series(
        model, rp_value_model, min_duration=min_duration
    )
    # Match the dates to events
    detection_stats = utils.get_detection_stats(
        true_event_dates=obs_dates,
        forecasted_event_dates=model_dates,
        days_before_buffer=days_before_buffer,
        days_after_buffer=days_after_buffer,
    )
    df_event_stats = pd.concat(
        (
            df_event_stats,
            pd.DataFrame(
                {
                    **{"gov": gov},
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
