# Andean Fire Ecosystem Dynamics (2017-2025)
**Code and Data Repository**

[![DOI](https://zenodo.org/badge/DOI/[https://doi.org/10.5281/zenodo.20669734].svg)](https://doi.org/10.5281/zenodo.20669734)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![R 4.0+](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Google Earth Engine](https://img.shields.io/badge/GEE-Python_API-green.svg)](https://earthengine.google.com/)

## Overview
This repository contains the source code and datasets required to reproduce the spatial and statistical analyses presented in the manuscript: **"[Fire recurrence drives structural collapse masked by greenness in non-fire-adapted Andean ecosystems]"**. 

The study evaluates wildfire spatiotemporal variability, environmental drivers, and ecological recovery in high-Andean ecosystems (specifically the Titankayocc Regional Conservation Area) utilizing a multi-sensor harmonization approach (Sentinel-2, Landsat 8/9) via Google Earth Engine (GEE), followed by robust econometric validation.

## Repository Structure

The analytical workflow is structured into two main phases: first, spatial data extraction and harmonization using 5 Python scripts (designed for Google Colab); and second, econometric modeling and validation using 5 R scripts, accompanied by their corresponding intermediate datasets.

```text
├── data/
│   ├── 1_Global_Monthly_Stats.csv        # Harmonized monthly NDVI/NBR time-series
│   ├── 2_Annual_Impact.csv               # Global burned area and mean dNBR per year
│   ├── 3_Zonal_Annual_Stats.csv          # Disaggregated impact by management zone
│   ├── 4_Precipitation_Monthly.csv       # CHIRPS monthly hydrological baseline
│   └── 5_Topographic_Samples.csv         # Stratified sampling (Burned vs Unburned)
│
├── scripts_python_gee/
│   ├── Script_01_Time_Series_Extraction.ipynb
│   ├── Script_02_Fire_Frequency_Mapping.ipynb
│   ├── Script_03_Climate_Data_CHIRPS.ipynb
│   ├── Script_04_Spatiotemporal_Trends.ipynb
│   └── Script_05_Topographic_Sampling.ipynb
│
├── scripts_r_statistics/
│   ├── 01_Eco_Hydrological_Dynamics.R
│   ├── 02_Zonal_Vulnerability.R
│   ├── 03_Topographic_&_Lag_Analysis.R
│   ├── 04_Spatiotemporal_&_Index_Sensitivity.R
│   └── 05_Statistical_Validation_Topography_ENSO.R
│
├── README.md
└── LICENSE


