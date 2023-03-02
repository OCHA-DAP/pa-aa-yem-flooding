# Floodscan

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
fs.time.max()
```

```python

```
