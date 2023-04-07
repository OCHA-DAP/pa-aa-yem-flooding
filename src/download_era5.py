import os
import sys
from datetime import date
from pathlib import Path

import cdsapi
from dateutil import rrule

clobber = False

c = cdsapi.Client()

year = int(sys.argv[1])
start_date = date(year, 1, 1)
end_date = date(year, 12, 31)

date_range = rrule.rrule(
    freq=rrule.MONTHLY,
    dtstart=start_date,
    until=end_date,
)

for forecast_date in date_range:
    forecast_date_string = forecast_date.strftime("%Y-%m")
    req = {
        "variable": "total_precipitation",
        "year": forecast_date.strftime("%Y"),
        "month": forecast_date.strftime("%m"),
        "day": [
            "01",
            "02",
            "03",
            "04",
            "05",
            "06",
            "07",
            "08",
            "09",
            "10",
            "11",
            "12",
            "13",
            "14",
            "15",
            "16",
            "17",
            "18",
            "19",
            "20",
            "21",
            "22",
            "23",
            "24",
            "25",
            "26",
            "27",
            "28",
            "29",
            "30",
            "31",
        ],
        "time": [
            "00:00",
            "01:00",
            "02:00",
            "03:00",
            "04:00",
            "05:00",
            "06:00",
            "07:00",
            "08:00",
            "09:00",
            "10:00",
            "11:00",
            "12:00",
            "13:00",
            "14:00",
            "15:00",
            "16:00",
            "17:00",
            "18:00",
            "19:00",
            "20:00",
            "21:00",
            "22:00",
            "23:00",
        ],
        "format": "grib",
        "area": [19, 42, 12, 55],
    }
    output_filename = (
        Path(os.environ["OAP_DATA_DIR"])
        / f"public/raw/yem/ecmwf/yem_era5_tp_{forecast_date_string}.grib2"
    )
    if output_filename.exists() and not clobber:
        print(f"{forecast_date_string} exists, skipping")
        continue
    c.retrieve("reanalysis-era5-land", req, str(output_filename))
