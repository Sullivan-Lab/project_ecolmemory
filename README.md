# Ecological Memory of Grasslands
A repository for code and analysis of the ecological memory of plant biomass production from experiments E001 and E002 at Cedar Creek Ecosystem Science Reserve.

## Table of Contents
- [Introduction](#Introduction)
- [Workflow](#Workflow)
- [Location of data](#Location-of-data)
- [Spatiotemporal extent and resolution](#Spatiotemporal-extent-and-resolution)
- [Usage](#Usage)
- [File naming conventions](#File-naming-conventions)
- [License](#License)
- [Funding Sources](#Funding-sources)
- [Acknowledgements](#Acknowledgements)
- [Contributors](#Contributors)
- [Contact Information](#Contact-information)

## Introduction

This repository allows readers to run models to reproduce results and figures of the paper "Ecological memory mediates plant biomass response to multiple global change drivers", currently submitted to *Ecology Letters*.

## Workflow

This repository is organized in to four main folders:
- **`code`:** contains folders for wrangling the data to generate data files for analysis (`/L1_dataWrangling_DataForModels`), run the analysis (`/L2_dataAnalysis`), perform calculations based on model estimates (`/L2_dataAnalysis_calculations`), and generate the figures present in the main paper (`/L2_figures`).
- **`data`:** contains a folder for raw data (`/L0`) and one for processed data to be used to run the analysis (`/L1`).
- **`figures`:** contains all the figures included in the paper (main `/final` and supplement), but all of them can be generated from the code in this repository.
- **`parameters`:** 

After cloning this repository, downloaded the data from [DRYAD](link) and paste the files in their respective folders (specifications can be found here). Then, run the code. The data files in the appropriate structure needed to run the models can be (and should be) generated from this repository to then run the models. Be aware that these models are complex and running them might take days. All figures present in the manuscript can be generated *without* running the models by using the files present in [DRYAD](link). 

Precipitation (illustration credits to Diana Kleine) and temperature icons (illustration credits to Tracey Saxby) present in `parameters/CoarseEnv/PNG` were retrieved from [Integration and Application Network](https://ian.umces.edu/media-library/). 

## Location of data 

All CSV files needed to run this code can be found in [DRYAD](link).

## Spatiotemporal extent and resolution 

[*Describe the spatial and temporal extent and resolution of the data sets resulting from the workflow. For example:*]  
- Spatial extent: [*81 NEON sites across North America*]
- Spatial resolution: [*1 NEON site*]
- Temporal extent: [*1950-2010*]
- Temporal resolution: [*Monthly average temperature and precipitation*]

## Usage

R version 4.5.1 and RStudio version 2025.05.0. 

### File Naming Conventions

- **Data Files**: [*Specify a clear naming convention for data files, such as `YYYY-MM-DD_sensorID_raw.csv`.*]
- **Scripts**: [*Use descriptive names for scripts and follow a consistent naming convention, such as `process_data.py` or `cleaning_script.R`. If scripts are designed to be run in sequence, it may be helpful to number them in order (e.g., `1_cleaning_script.R`, `2_process_data.R`)*]

## Scripts

[*Provide descriptions of the scripts in the `/L0` folder, including their purpose, inputs, and outputs. Specify whether scripts are standalone or designed to be run in sequence.*] 

### [*`preprocess_data.py`*]

- **Purpose**: [*Cleans and preprocesses raw Level 0 data.*]
- **Inputs**: [*Raw data files in the `/data` folder (`YYYY-MM-DD_sensorID_raw.ext`).*]
- **Outputs**: [*Processed data files in a new folder (`/processed_data`).*]

### [*`merge_datasets.R`*]

- **Purpose**: [*Merges multiple preprocessed datasets.*]
- **Inputs**: [*Processed data files in the `/processed_data` folder.*]
- **Outputs**: [*Merged dataset saved as `merged_data.csv` in the `/output` folder.*]

## Data

Below is the specific folder location where the file downloaded from [DRYAD](link) should be added to, and a description of the CSV file columns:

### [*`data/L0/file.csv`*]: 

## Funding Sources

Data collection at Cedar Creek Ecosystem Science Reserve was funded, in part, by the University of Minnesota and NSF grant DEB-2425352. DMPP was supported by the USGS-Southwest Climate Adaptation Science Center.

## Acknowledgements

We thank the Sullivan Lab for their valuable feedback. We also acknowledge everyone who contributed to data collection over the years of experiments E001 and E002 at the Cedar Creek Ecosystem Science Reserve. This is Kellogg Biological Station Contribution no.___.

## Contributors

Plant data has been collected since 1982 at [Cedar Creek Ecosystem Science Reserve LTER](https://cedarcreek.umn.edu/research/data), and pre-processed data used to generate the files we store here in `data/L0` was obtained from [DeSiervo et al. 2023](https://doi.org/10.1111/ele.14229).

## Contact Information

Please contact Laís Petri at petrila1<at>msu.edu re: questions about the scripts and data.
