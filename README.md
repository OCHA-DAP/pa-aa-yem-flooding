# Yemen Anticipatory Action: flooding

[![Generic badge](https://img.shields.io/badge/STATUS-ENDORSED-%231EBFB3)](https://shields.io/)

## Background information

This repo contains technical work on Anticipatory Action (AA) pilot framework designed to preemptively intervene against flood-related events and impact in Yemen in 2023. A framework was developed collaboratively by the OCHA Yemen Country  team, the IMWG, the Yemen Humanitarian Fund (YHF),  and the Centre for Humanitarian Data. It includes pre-agreed financing, pre-selected interventions, and a trigger for automated activation. 

The AA pilot project focuses on IDP sites categorized as high flood hazard (CCCM, 2023) within two governorates in Yemen: Hajjah & Marib. The triggering mechanism is based on a 3-day cumulative rainfall forecast. When any day within a 7 day forecast crosses the threshold set in either Area of Interest (AOI), the trigger is activated for the respective AOI. 


The trigger framework was not activated in 2023 and the monitoring has since been disabled. Nonetheless the GitHub action can always be re-enabled for those interested, but it is no longer linked to an AA frameowkr

More information can be found on the [Technical Note](https://docs.google.com/document/d/1pf3tlQZ-QSb59fEEPextxUpWxy68spKyIUu4VWL8L4s/edit?tab=t.0#heading=h.ieffsjdjd8lt).


## Overview of analysis

This repo contains both `python` and `R` analyses.

- The  exploratory analysis & trigger design done in R follows the [{targets}](https://github.com/ropensci/targets) framework.  The R analysis can be initiated using `targets::tar_make()`. For instructions on how to run the R - analysis please visit the targets documentation.
- Python was used primarily to obtain and query data sets from APIs. 

## Data description

### Exploratory Data

The following data sets were  assessed in the development of the trigger:

- Rainfall (observed)
    + [CHIRPS](https://www.chc.ucsb.edu/data/chirps)
    + [ERA5](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels?tab=overview)
- Rainfall (forecast)
    + [CHIRPS-GEFS](https://chc.ucsb.edu/data/chirps-gefs)
    + [ECMWF HRES](https://www.ecmwf.int/en/forecasts/datasets/set-i)
- Impact data: 
    + CCCM flood report data (2021-2022) - from Yemen CCCM cluster [dashboard here](https://data.unhcr.org/es/dataviz/210)
    + [EMDAT](https://www.emdat.be/)
    + [DISVENTAR](https://www.desinventar.net/)

## Directory structure

The code in this repository is organized as follows:

```shell

├── analysis      # python analytical work
├── docs          # .Rmd files or other relevant documentation
├── exploration   # Experimental work not intended to be replicated
├── src           # Python code to run any relevant data acquisition/processing pipelines as well as R code for GHA monitoring.
  ├── update_trigger.R # GHA - R code sourced by github actions to run real time monitoring and email dispatch
  ├── email       # GHA - email specific github action utility functions
├── R             # R code to run any relevant data acquisition/processing pipelines
├── _targets      # Diretory containing data and metadata output from running of R `{targets}` pipeline
├── .gitignore
├── README.md
├── _targets.R.   # R script containing targets pipeline
└── requirements.txt

```
## Realtime Trigger Monitoring (GHA)

A GitHub Action was set up to monitor the 2023 season and send daily notifications with the latest forecast information from
CHIRP-GEFs. Data storage for the monitoring system was the GDRIVE with cloud access controlled by a service account json key file which is referenced in the `src/update_trigger.R` file and github actions yaml using the  env var: `YEM_TRIG_MONITORNG_JSON`. 

## Reproducing this analysis

Create a directory where you would like the data to be stored,
and point to it using an environment variable called
`AA_DATA_DIR`.

### R

The analysis conducted in R is all performed via a `{targets}` workflow.
To run R `targets` pipeline you must have the `{targets}` R package installed. More information can be found [here](https://github.com/ropensci/targets)

The CRAN release can be installed with :

```r
install.packages("targets")
```

The `_targets.R` file contains the entire R portion of the analysis and can be run with:

```r
targets::tar_make()
```

Any functions sourced in targets are contained in the `R/` directory.


Currently the targets pipeline uses Google Earth Engine (GEE) via the [rgee package](https://github.com/r-spatial/rgee). You must have an earth engine account and the rgee package set up for this portion of the pipeline to work.

### Python

Create a new virtual environment and install the requirements with:

```shell
pip install -r requirements.txt
```

Finally, install any code in `src` using the command:

```shell
pip install -e .
```

To run the pipeline that downloads and processes the data, execute:

```shell
python src/main.py
```


To see runtime options, execute:

```shell
python src/main.py -h
```



If you would like to instead receive the processed data from our team, please
[contact us](mailto:centrehumdata@un.org).

## Development

All python code is formatted according to black and flake8 guidelines.
The repo is set-up to use pre-commit.
Before you start developing in this repository, you will need to run

```shell
pre-commit install
```

The `markdownlint` hook will require
[Ruby](https://www.ruby-lang.org/en/documentation/installation/)
to be installed on your computer.

You can run all hooks against all your files using

```shell
pre-commit run --all-files
```

- It is also **strongly** recommended to use `jupytext`
to convert all Jupyter notebooks (`.ipynb`) to Markdown files (`.md`)
before committing them into version control. This will make for
cleaner diffs (and thus easier code reviews) and will ensure that cell outputs aren't
committed to the repo (which might be problematic if working with sensitive data).

- For R code styling the [{styler}](https://github.com/r-lib/styler) package is recommended.
