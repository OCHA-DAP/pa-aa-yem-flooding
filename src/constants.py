import os
from pathlib import Path

from ochanticipy import create_custom_country_config

iso3 = "yem"

oap_data_dir = Path(os.environ["OAP_DATA_DIR"])

country_config = create_custom_country_config(
    Path(__file__).parent.resolve() / "yem.yaml"
)
