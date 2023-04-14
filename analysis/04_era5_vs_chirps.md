# ERA5 vs CHIRPS

Checking how well the ECMWF rainfall model, ERA5,
corresponds with CHIRPS rainfall

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
data_dir = (
    constants.oap_data_dir / "private/processed/yem/targets_20230407/csv/"
)
df_era5 = pd.read_csv(data_dir / "era5.csv")
```

```python
# Clean the dataframes a bit
df_era5_dict = {
    gov: df_era5.loc[df_era5["governorate_name"] == gov][
        ["date", "era_roll3"]
    ].set_index("date")
    for gov in GOVS
}
```

## ERA5 return period

```python
show_plots = True

rp_era5_dict = {}
for gov in GOVS:
    df = df_era5_dict[gov].copy()
    df.index = pd.to_datetime(df.index).to_period()
    df = df.resample(rule="A", kind="period").max().sort_values(by="era_roll3")
    rp_func = utils.get_return_period_function_analytical(
        df_rp=df, rp_var="era_roll3", show_plots=show_plots
    )
    rp_era5_dict[gov] = float(rp_func(RP))

rp_era5_dict
```
