import os
from datetime import date
from pathlib import Path

import cdsapi
from dateutil import rrule

c = cdsapi.Client()

start_date = date(2007, 1, 1)
end_date = date(2022, 12, 31)

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
        "time": "00:00",
        "format": "grib",
        "area": [19, 42, 12, 55],
    }
    output_filename = (
        Path(os.environ["OAP_DATA_DIR"])
        / f"private/raw/yem/ecmwf/yem_era5_tp_{forecast_date_string}.grib2"
    )
    if output_filename.exists():
        print(f"{forecast_date_string} exists, skipping")
        continue
    c.retrieve("reanalysis-era5-land", req, str(output_filename))
