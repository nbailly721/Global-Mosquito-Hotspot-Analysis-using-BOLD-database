    ##Global Mosquito Hotspot Analysis: Mapping Aedes aegypti, Aedes albopictus, and Culex pipiens Distributions & Climate-Driven Risk##

-This project performs an end-to-end spatial analysis of three major disease-vector mosquitoes using species occurrence data from the BOLD (Barcode of Life Data Systems) dataset. It integrates species richness with climate variables (temperature and precipitation) to identify current and projected global mosquito hotspots under warming scenarios.

🖥️ Workflow Overview

**Data Ingestion & Cleaning (R Script)**
-Load raw BOLD occurrence data for the three mosquito species:

-Aedes aegypti: aedesaegypti.tsv

-Aedes albopictus: aedesalbopictus.tsv

-Culex pipiens: culexpipiens.tsv

-Standardize column names, filter for species of interest, and remove records missing country-level information.

**Species Distribution Mapping**
-Summarize presence by country and create separate maps for each species using ggplot2 and rnaturalearth shapefiles.

-Combine datasets to calculate species richness per country (number of vector species present).

**Climate Integration**
-Load climate raster data (WorldClim v2.1) for monthly temperature and precipitation:

 -Temperature data
 
 -Precipitation data
 
-Extract country-level climate averages and scale variables to create a climate suitability score.

-Compute a hotspot score as the product of species richness and climate suitability.

**Hotspot Visualization & Analysis**
-Map current global mosquito hotspots.

-Perform linear regression to quantify the contribution of temperature vs. precipitation to hotspot scores.

-Model projected hotspots under +2°C and +3°C warming scenarios.

-Calculate changes in suitability compared to current climate.


🔧 R Packages & Tools

-tidyverse – Data wrangling, summarization, visualization

-sf & rnaturalearth – Mapping and spatial data handling

-terra – Raster and climate data processing

-viridis – Color scales for plots

📊 Key Results - Figures

-Global distribution maps for each species (Culex pipiens, Aedes albopictus, Aedes aegypti)

-Combined species richness map

-Climate-integrated hotspot map (current conditions)

-Regression analysis of climate effects on hotspots

-Projected hotspots under +2°C and +3°C warming

-Change in hotspot suitability maps (Δ hotspot scores)

📂 Files

-global_mosquito_hotspot_analysis.R – Complete R script covering data ingestion, cleaning, mapping, climate integration, regression, and hotspot prediction

🧠 Notes

-Demonstrates a reproducible pipeline for species distribution and climate-driven risk analysis.

-Can be adapted for other vector species, regions, or environmental datasets.

-Integrates BOLD species occurrence data with WorldClim climate data for global risk modeling.

-All the graphs showing future projections only represent the hotspots for **January**. The same code structure can be used to produce visualizations for the rest of months.

📌 Policy-Based Relevance

-Supports global health planning by identifying current and emerging mosquito hotspots.

-Helps prioritize regions for vector surveillance, disease prevention, and climate adaptation measures.

-Provides a scalable, reproducible framework for vector-borne disease risk assessment.

🏛️ Data Sources

-Species distribution data (BOLD dataset): https://boldsystems.org/

-Climate data (WorldClim v2.1): https://www.worldclim.org/data/worldclim21.html
