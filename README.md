# Ecological Memory of Grasslands
A repository for code and analysis of the ecological memory of plant biomass production from experiments E001 and E002 at Cedar Creek Ecosystem Science Reserve.

## Table of Contents
- [Introduction](#Introduction)
- [Workflow](#Workflow)
- [Location of data](#Location-of-data)
- [Spatiotemporal extent and resolution](#Spatiotemporal-extent-and-resolution)
- [Usage](#Usage)
- [File naming conventions](#File-naming-conventions)
- [Data](#Data)
- [Scripts](#Scripts)
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
- **`parameters`:** contains a folder per model type (SAM models `/CoarseEnv`, linear model `/CoarseEnv_Slopes`), and a folder for supplemental data formatted more closely to how it is displayed in the supplement (`/SuppTable`)

Instructions:
- Clone this repository.
- Download the data from [DRYAD](link) and place the files in their respective folders (see folder structure specifications [here](#Data).
- Run the code to generate the data files in the structure required to fit the models.
- These structured data files are produced from this repository and are necessary to run the models.
- Note: The models are computationally intensive and may take several days to run.
- To reproduce the figures in the manuscript without rerunning the models, use the processed files provided in [DRYAD](link).

Precipitation (illustration credits to Diana Kleine) and temperature icons (illustration credits to Tracey Saxby) present in `parameters/CoarseEnv/PNG` were retrieved from [Integration and Application Network](https://ian.umces.edu/media-library/). 

## Location of data 

All CSV files needed to run this code can be found in [DRYAD](link).

## Spatiotemporal extent and resolution 

- Spatial extent: Cedar Creek Ecosystem Science Reserve in Minnesota, US
- Spatial resolution: two experiments (E001, E002) within the Reserve
- Temporal extent: 1982-2004 (plant data), 1963-2023 (weather data)
- Temporal resolution: yearly plant biomass data, monthly average temperature, and monthly maximum temperature data

## Usage

R version 4.5.1 and RStudio version 2025.05.0. 

### File Naming Conventions

- **Data Files**:  
  - Files in `/L0` do not follow a naming convention.  
  - Files in `/L1` are named using the format `DataModel_TREATMENT_CoarseEnv.RData` and are generated using the R scripts located in `/code/L1_dataWrangling_DataForModels`.  

- **Scripts**:  
  - Scripts are organized into descriptive folders and named consistently.  
  - File names begin with either `L1_` or `L2_`, followed by `dataWrangling_DataForModels_CoarseEnv.Rmd` or `dataAnalysis_CDCRdata_Nutrient`.  
  - The latter are then appended with the treatment (`Dist`, `NoDist`, `Minus5y`) and the model type (SAM: `CoarseEnv`, lm: `Slopes`).  
  - All scripts are written as R Markdown (`.Rmd`) files.  

## Data

Below is the specific folder location where the file downloaded from [DRYAD](link) should be added to, and a description of the CSV file columns:

### *1. `data/L0/da_full_010924.csv`*: 
- family = plant family (character), 
- species = species name (character), 
- field = field identifier within the experiment [A, B, C] (character), 
- year = year of sampling (double), exp = experimental identifier [1 = intact, 2 = disturbed] (double), 
- disk = whether soil was disked or not [0 = intact, 1 = disturbed] (double), 
- plot = plot number within a field (double), 
- subplot = subplot identifier within a plot [Whole, East, West] (character), 
- ntrt = numeric identifier for nutrient treatment [0 to 9] (double), 
- other.add = indicator of whether micronutrients were added [0 = no, 1 = yes] (double), 
- fence.origin = indicator of fenced origin [1 = fenced, NA = missing] (double), 
- mass.above = aboveground biomass per species in g/m² (double), 
- live = indicator that biomass represents live material [1 = live biomass, 0 = litter] (double), 
- sorted = indicator that material was sorted by species [1 = sorted by species or genus, 2 = not sorted and usually refers to litter] (double), 
- wood = indicator of woody species [1 = woody, 0 = non-woody] (double), 
- functional.group = functional group code [F = forbs, L = legumes, C3 = C3 grasses, C4 = C4 grasses, S = sedges, W = woody species, G = grasses of uncertain type (e.g., Panicum spp. or mixed grass seedlings), O = other non-vascular taxa (e.g., horsetail, fungi, moss, lichen), UNKNOWN = unclassified or miscellaneous species, NA = litter] (character), duration = lifespan category [PERENNIAL, ANNUAL, NA] (character), 
- lifeform = general lifeform category [WOODY, FORB, GRASS, LEGUME, SEDGE, HORSETAIL, FUNGI, MOSS & LICHENS, UNKNOWN, NA] (character), 
- pathway = photosynthetic pathway [C3, C4, UNKNOWN] (character), 
- origin = species origin [N = native, I = introduced, UNK = unknown, NA] (character).

### *2. `data/L0/da_weather_020625.csv`*: 
- Year = calendar year of observation (double), 
- Month = calendar month of observation [1–12] (double), 
- MaxTemp_C = mean monthly maximum temperature (°C) (double), 
- MinTemp_C = mean monthly minimum temperature (°C) (double), 
- Precip_mm = total monthly precipitation (mm) (double), 
- PDSI = Palmer Drought Severity Index (unitless), indicating relative wetness or dryness; missing for early years [NA] (double).

### *3. Climate means files*
#### Files:
- *`data/L1/DataModel_CoarseEnv/climateMeans/clim.means_Dist_CoarseEnv.csv`* 
- *`data/L1/DataModel_CoarseEnv/climateMeans/clim.means_NoDist_CoarseEnv.csv`*
- *`data/L1/DataModel_CoarseEnv/climateMeans/clim.means_DistMinus5y_CoarseEnv.csv`*
- *`data/L1/DataModel_CoarseEnv/climateMeans/clim.means_NoDistMinus5y_CoarseEnv.csv`*

#### Columns:
- Precip_mm = mean annual precipitation (mm) (double),
- MaxTemp_C = mean annual maximum temperature (°C) (double).

#### Periods:
- 1977–2004 for `Dist` and `NoDist`
- 1982–2004 for `DistMinus5y` and `NoDistMinus5y`

### *4. Model parameter files* 
#### Files:
- *`parameters/CoarseEnv/Dist_CoarseEnv_ParametersValues.csv`*
- *`parameters/CoarseEnv/DistMinus5y_CoarseEnv_ParametersValues.csv`*
- *`parameters/CoarseEnv/NoDist_CoarseEnv_ParametersValues.csv`*
- *`parameters/CoarseEnv/NoDistMinus5y_CoarseEnv_ParametersValues.csv`*
- *`parameters/CoarseEnv/abg/precipData_ParametersValues.csv`* 
- *`parameters/CoarseEnv/abg/tempData_ParametersValues.csv`* 
- *`parameters/CoarseEnv/abgMinus5y/precipData_ParametersValues.csv`*
- *`parameters/CoarseEnv/abgMinus5y/tempData_ParametersValues.csv`* 

#### Columns:
- parameter = model parameter name, including hierarchical indices (character),
- statistics.Mean = posterior mean estimate of the parameter (double),
- statistics.SD = posterior standard deviation of the parameter (double),
- quantiles.2.5. = lower 2.5% quantile of the posterior distribution (double),
- quantiles.97.5. = upper 97.5% quantile of the posterior distribution (double).

## Scripts

Scripts within the sub folders in `/code` are designed to be run in sequence, from `L1_*` to `L2_*` (these in alphabetical order). However, the script in `L2_Figures` is a stand alone one if needed (see details under **Instructions** in [Workflow](#Workflow)).

### *`L1_dataWrangling_DataForModels_CoarseEnv.Rmd`*

- **Purpose**: Process data to be used in the models.
- **Inputs**: Raw data files in `/data/L0` folder.
- **Outputs**: Processed data files in `/data/L1/DataModel_CoarseEnv`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_Dist_CoarseEnv.Rmd`*

- **Purpose**: Runs SAM model for Disturbed plots including all years of data.
- **Inputs**: Processed data files in `/data/L1/DataModel_CoarseEnv`.
- **Outputs**: Model assessment figures in `figures`; two CSV files with model parameters, and `.RData` file containing all the model input and output data, a pdf with the trace plots of selected parameters in `/parameters/CoarseEnv`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_DistMinus5y_CoarseEnv.Rmd`*

- **Purpose**: Runs SAM model for Disturbed plots excluding the first 5 years of data.
- **Inputs**: Processed data files in `/data/L1/DataModel_CoarseEnv`.
- **Outputs**: Model assessment figures in `figures`; two CSV files with model parameters, and `.RData` file containing all the model input and output data, a pdf with the trace plots of selected parameters in `/parameters/CoarseEnv`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_NoDist_CoarseEnv.Rmd`*

- **Purpose**: Runs SAM model for Intact plots including all years of data.
- **Inputs**: Processed data files in `/data/L1/DataModel_CoarseEnv`.
- **Outputs**: Model assessment figures in `figures`; two CSV files with model parameters, and `.RData` file containing all the model input and output data, a pdf with the trace plots of selected parameters in `/parameters/CoarseEnv`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_NoDistMinus5y_CoarseEnv.Rmd`*

- **Purpose**: Runs SAM model for Intact plots excluding the first 5 years of data.
- **Inputs**: Processed data files in `/data/L1/DataModel_CoarseEnv`.
- **Outputs**: Model assessment figures in `figures`; two CSV files with model parameters, and `.RData` file containing all the model input and output data, a pdf with the trace plots of selected parameters in `/parameters/CoarseEnv`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_DistAndNoDist_CoarseEnv_Slopes.Rmd`*

- **Purpose**: Runs a linear model to estimate the relationship between the strength of precipitation and maximum temperature effects as a function of nutrient addition levels, across treatments and including all years of data.
- **Inputs**: SAM model outputs in `/parameters/CoarseEnv`.
- **Outputs**: Model assessment figures in `figures`; a CSV file with model parameters, and `.RData` file containing all the model input and output data, a pdf with the trace plots of selected parameters in `/parameters/CoarseEnv_Slopes/abg`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_DistAndNoDist_CoarseEnv_Slopes.Rmd`*

- **Purpose**: Runs a linear model to estimate the relationship between the strength of precipitation and maximum temperature effects as a function of nutrient addition levels, across treatments and excluding the first 5 years of data.
- **Inputs**: SAM model outputs in `/parameters/CoarseEnv`.
- **Outputs**: Model assessment figures in `figures`; a CSV file with model parameters, and `.RData` file containing all the model input and output data, a pdf with the trace plots of selected parameters in `/parameters/CoarseEnv_Slopes/abgMinus5y`.

### *`L2_dataAnalysis_CDCRdata_SuppTable.Rmd`*

- **Purpose**: Format model parameter estimates outputs in a format closer to the one used in the supplemental tables.
- **Inputs**: SAM model outputs in `/parameters/CoarseEnv`.
- **Outputs**: Excel files with model parameters a bit more ready to go to the supplement `/parameters/SuppTable`.

### *`L2_dataAnalysis_CDCRdata_Nutrient_AllDist_Calc_CoarseEnv.Rmd`*

- **Purpose**: Perform calculations on effects based on estimated model parameters to add to the results section.
- **Inputs**: SAM model outputs in `/parameters/CoarseEnv` and `/parameters/CoarseEnv_Slopes`.
- **Outputs**: Nothing is exported, all values are displayed in the Console when code is run.

### *`L2_dataAnalysis_CDCRdata_Nutrient_AllDist_Figures_AllCoarseEnv_LiveBiomassOnly.Rmd`*

- **Purpose**: Format model parameter estimates outputs in a format closer to the one used in the supplemental tables.
- **Inputs**: Model parameters in `/parameters/CoarseEnv` and `/parameters/CoarseEnv_Slopes`, climate means from `/data/L1/DataModel_CoarseEnv/climateMeans`, and icons from `/parameters/CoarseEnv/PNG`.
- **Outputs**: Figures included in the main manuscript exported to `/figures/final`.

## Funding Sources

Data collection at Cedar Creek Ecosystem Science Reserve was funded, in part, by the University of Minnesota and NSF grant DEB-2425352. DMPP was supported by the USGS-Southwest Climate Adaptation Science Center.

## Acknowledgements

We thank the Sullivan Lab for their valuable feedback. We also acknowledge everyone who contributed to data collection over the years of experiments E001 and E002 at the Cedar Creek Ecosystem Science Reserve. This is Kellogg Biological Station Contribution no.___.

## Contributors

Plant data has been collected since 1982 at [Cedar Creek Ecosystem Science Reserve LTER](https://cedarcreek.umn.edu/research/data), and pre-processed data used to generate the files we store here in `data/L0` was obtained from [DeSiervo et al. 2023](https://doi.org/10.1111/ele.14229).

## Contact Information

Please contact Laís Petri at petrila1<at>msu.edu re: questions about the scripts and data.
