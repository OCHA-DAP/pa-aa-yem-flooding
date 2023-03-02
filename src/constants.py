from pathlib import Path

from ochanticipy import create_custom_country_config

iso3 = "yem"
country_config = create_custom_country_config(
    Path(__file__).parent.resolve() / "yem.yaml"
)
