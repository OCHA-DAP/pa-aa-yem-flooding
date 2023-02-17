# Flooded fraction in the country and its correlation to impact

This is an exploration to understanding if anticipating an increased
 risk in TCD is possible.
We start by getting a historical overview on the floods in the country
 from FloodScan
We compare this to the limited impact data we got.

This is an exploration of a quick understanding of increased risk,
 not a full AA framework
3 notions:

- recommended focus on riverine flooding
- priority areas/provinces: N'Djamena and Mayo Kebbi Est
- expected peak period: september-october (sometimes until november)

This notebook focuses on the whole country and `02_tcd_floodscan_roi.md`
 zooms in on the priority provinces

```python
%load_ext jupyter_black
```

```python
from src import utils
```

```python
fs = utils.load_floodscan_stats(admin_level=0)
```

```python
fs.plot("time", "sum")
```

```python

```
