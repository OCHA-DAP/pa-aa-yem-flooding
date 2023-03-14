# Yemen Anticipatory Action: flooding

[![Generic badge](https://img.shields.io/badge/STATUS-UNDER%20DEVELOPMENT-%23007CE0)](https://shields.io/)

## Background information

Provide a basic overview of the context of anticipatory action in this country.
Link to the GDrive Trigger Card document for greater context and details.


## Overview of analysis

What is the basic process of the analysis contained within this repository?

## Data description

### Exploratory Data

- CCCM flood report data (2021-2022) - from Yemen CCCM cluster
- CHIRPS
- CHIRPS-GEFS
- EMDAT
- DISVENTAR

- Where does the data come from? Are there any licensing or usage restrictions?
- How can the data be accessed?
- Why were these datasets selected?
- Are there any limitations with these datasets that one should be aware
    of when running the analysis and interpreting results?

## Directory structure

The code in this repository is organized as follows:

```shell

├── analysis      # Main repository of analytical work for the AA pilot
├── docs          # .Rmd files or other relevant documentation
├── exploration   # Experimental work not intended to be replicated
├── src           # Python code to run any relevant data acquisition/processing pipelines
├── R             # R code to run any relevant data acquisition/processing pipelines
├── _targets      # Diretory containing data and metadata output from running of R `{targets}` pipeline
|
├── .gitignore
├── README.md
├── _targets.R.   # R script containing targets pipeline
└── requirements.txt

```

## Reproducing this analysis

### Python

Create a directory where you would like the data to be stored,
and point to it using an environment variable called
`AA_DATA_DIR`.

Next create a new virtual environment and install the requirements with:

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


If you would like to instead receive the processed data from our team, please
[contact us](mailto:centrehumdata@un.org).

## Development

All code is formatted according to black and flake8 guidelines.
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

It is also **strongly** recommended to use `jupytext`
to convert all Jupyter notebooks (`.ipynb`) to Markdown files (`.md`)
before committing them into version control. This will make for
cleaner diffs (and thus easier code reviews) and will ensure that cell outputs aren't
committed to the repo (which might be problematic if working with sensitive data).
