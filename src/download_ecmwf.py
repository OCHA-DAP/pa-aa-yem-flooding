import os
from datetime import date
from pathlib import Path

from dateutil import rrule
from ecmwfapi import ECMWFService

server = ECMWFService("mars")

start_date = date(2006, 1, 1)
end_date = date(2006, 12, 31)

date_range = rrule.rrule(
    freq=rrule.DAILY,
    dtstart=start_date,
    until=end_date,
)

for forecast_date in date_range:
    forecast_date_string = forecast_date.strftime("%Y-%m-%d")
    req = {
        "class": "od",
        "date": forecast_date_string,
        "expver": 1,
        "levtype": "sfc",
        "area": "19/42/12/55",
        "param": "228.128",
        "step": "0/24/48/72/96/120/144/168/192/216/240",
        "stream": "oper",
        "time": "00:00:00",
        "type": "fc",
        "use": "infrequent",
    }
    output_filename = (
        Path(os.environ["OAP_DATA_DIR"])
        / f"private/raw/yem/ecmwf/yem_fc_tp_{forecast_date_string}.grib2"
    )
    if output_filename.exists():
        print(f"{forecast_date_string} exists, skipping")
        continue
    server.execute(req, output_filename)
