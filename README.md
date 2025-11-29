    ##Global Mosquito Hotspot Analysis: Mapping Aedes aegypti, Aedes albopictus, and Culex pipiens Distributions & Climate-Driven Risk##

**Description**

This project performs an end-to-end spatial analysis of three major disease-vector mosquitoes using species occurrence data from the BOLD (Barcode of Life Data Systems) dataset. It integrates species richness with global climate variables (temperature and precipitation) to identify current and projected mosquito hotspots under warming scenarios. The workflow merges species distribution patterns with climate suitability modeling to assess present-day and future climate-driven risk.

**Workflow Overview**

    1. Data Ingestion & Preparation (R)

Load raw BOLD occurrence data for three mosquito species:

aedesaegypti.tsv

aedesalbopictus.tsv

culexpipiens.tsv

Standardize column names and filter for species of interest.

Remove incomplete records lacking country-level information.

    2. Species Distribution Mapping

Summarize presence counts by country for each species.

Generate individual species maps using ggplot2 and rnaturalearth shapefiles.

Combine datasets to compute species richness (number of mosquito vectors present per country).

    3. Climate Integration

Load climate rasters from WorldClim v2.1 for monthly temperature and precipitation.

Extract country-level climate averages and scale variables to produce a climate suitability score.

Compute a hotspot score as:
species richness × climate suitability

     4. Hotspot Visualization & Analysis

Map current global mosquito hotspots.

Fit linear regression models to quantify temperature vs. precipitation contributions.

Project hotspot scores under:

+2°C warming

+3°C warming

Calculate suitability changes relative to current climate.

**Datasets Used** 

Primary Dataset:
BOLD Species Distribution Data
Source: https://boldsystems.org/

Climate Dataset:
WorldClim v2.1, Temperature & Precipitation
Source: https://www.worldclim.org/data/worldclim21.html

**Processed / Generated Files**

aedesaegypti.tsv

aedesalbopictus.tsv

culexpipiens.tsv

Climate rasters (temperature & precipitation)

**Packages Used**

R Packages

tidyverse

sf

rnaturalearth

terra

viridis

Bash / Unix Tools

(None required)

**Key Results**

Global Distribution of Culex pipiens.png

Global Distribution of Aedes albopictus.png

Global Distribution of Aedes aegypti.png

Global Overlap of Disease-vector Mosquitoes.png

Global Mosquito Hotspots (Current Climate).png

Projected Mosquito Hotspots (+2°C).png

Projected Mosquito Hotspots (+3°C).png

Change in Mosquito Hotspots (+2°C).png

Change in Mosquito Hotspots (+3°C).png

**Files in This Repository**

global_mosquito_hotspot_analysis.R

**Important Notes**

Fully reproducible pipeline for species distribution & climate-driven risk modeling.

Adaptable to other vector species and environmental datasets.

Future projections shown are January only; the same workflow applies to other months.

**Real-World Relevance**

Supports global health planning by identifying current and emerging mosquito hotspots.

Prioritizes regions for vector surveillance and climate adaptation strategies.

Provides a scalable, reproducible framework for vector-borne disease risk assessment.
