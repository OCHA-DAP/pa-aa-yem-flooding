# ECMWF forecast analysis

Analyzing HRES hindcast downloaded from MARS
(and also using ERA5 historical).

For now, using two csvs that were crated from the R targets:
`ecmwf_mars_high_risk_hulls` (hres.csv) and `era5_w_rolling` (era5.csv).

```python
%load_ext jupyter_black
```

```python
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
    ].dropna()
    for gov in GOVS
}
df_hres_dict = {
    gov: df_hres.loc[df_hres["governorate_name"] == gov][
        ["date", "mean"]
    ].dropna()
    for gov in GOVS
}
```

## ERA5 return period

```python
# First let's look quickly at the ERA5 data, for Hajjah
df_era5_dict[GOVS[0]].plot(x="date", y="era_roll3", rot=90)
```

```python
df_era5_dict[GOVS[1]].plot(x="date", y="era_roll3", rot=90)
```

A bit concerned about these plots. The data don't seem
as smooth as with CHIRPS, and the peak rainfall in the last
3 years does not appear to be present.

But anyway, on to RP calculation.

```python
show_plots = True

rp_dict = {}
for gov in GOVS:
    df = df_era5_dict[gov].copy()
    df["date"] = pd.to_datetime(df["date"]).dt.to_period()
    df = (
        df.set_index("date")
        .resample(rule="A", kind="period")
        .max()
        .sort_values(by="era_roll3")
    )
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df, rp_var="era_roll3", show_plots=show_plots
    )
    rp_dict[gov] = float(rp_func(RP))

rp_dict
```

For Hajjah the RP value is similar (40 vs I think ~30 or 35 cm
for CHIRPS) but Marib is much lower.

## Processing HRES

The HRES data need a bit more procesing, it turns out that the numbers are cumulative,
so here I'm starting from the non-rolled values.

```python
# An example for a single goverornorate

df = df_hres_dict[GOVS[1]].copy()
# First step is to split up the date column
df[["date", "step"]] = (
    df["date"].str.strip("yem_fc_tp").str.split("_tp_step=", expand=True)
)
# Convert step to days
df["step"] = (pd.to_numeric(df["step"]) / 24).astype(int)
# Convert columns to leadtime
df = df.pivot(columns="step", values="mean", index="date")
# For each lead time, subtract all previous lead times
# (only subtracing the single last leadtime is not enough
# to make the means agree)
for n in range(1, 11):
    for m in range(0, n):
        df[n] = df[n] - df[m]
df
```

```python
# Make sure the stats make sense
df.describe()
```

The means look roughly similar, still increasing with leadtime though.
I'm just wondering whether it makes sense to subtract all past leadtimes,
I would think only the previous one would be needed, otherwise it doesn't
make that much sense. Something we maybe need to ask ECMWF, or there
is a very strong positive bias.

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

```python
# Now apply the above to the hres data
df_hres_proc_dict = {}

for gov in govs:
    df = df_hres_dict[gov].copy()
    # First step is to split up the date column
    df[["date", "step"]] = (
        df["date"].str.strip("yem_fc_tp").str.split("_tp_step=", expand=True)
    )
    # Convert step to days
    df["step"] = (pd.to_numeric(df["step"]) / 24).astype(int)
    # Convert columns to leadtime
    df = df.pivot(columns="step", values="mean", index="date")
    # For each lead time, subtract all previous lead times
    # (only subtracing the single last leadtime is not enough
    # to make the means agree)
    for n in range(1, 11):
        for m in range(0, n):
            df[n] = df[n] - df[m]
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

## General forecast skill

As a function of leadtime, look at the mean absolute error
of the forecast compared to the model.
