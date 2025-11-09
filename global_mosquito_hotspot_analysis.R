## ========================================================
## Project: Global Mosquito Hotspot Analysis
## Description:
##   This script uses publicly available BOLD datasets to map the
##   global distributions of three disease-vector mosquito species:
##   Aedes aegypti, Aedes albopictus, and Culex pipiens.
##   It combines these distributions to calculate species richness,
##   integrates climate variables (temperature and precipitation),
##   identifies current and projected hotspots under warming scenarios,
##   and visualizes the results with maps.
## ========================================================

##_ Load packages------------

install.packages('tidyverse')
install.packages('rnaturalearth')
install.packages('rnaturalearthdata')
install.packages('sf')
install.packages('terra')
install.packages('viridis')

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(terra)
library(viridis)

##_ Load data files------------

abopictus_raw <- read_tsv('aedesalbopictus.tsv')
# Load raw data for Aedes albopictus from BOLD dataset

cpipiens_raw <- read_tsv('culexpipiens.tsv')
# Load raw data for Culex pipiens from BOLD dataset

aaegypti_raw <- read_tsv('aedesaegypti.tsv')
# Load raw data for Aedes aegypti from BOLD dataset

##_ Data cleaning and selection------------

cpipiens_new <- cpipiens_raw %>%
  select('species','country/ocean') %>%
  filter(species == 'Culex pipiens', !is.na(`country/ocean`)) %>%
  rename(country='country/ocean')
# Select only relevant columns and species, remove missing countries, standardize column name

abopictus_new <- abopictus_raw %>%
  select('species','country/ocean') %>%
  filter(species == 'Aedes albopictus', !is.na(`country/ocean`)) %>%
  rename(country='country/ocean')
# Same process for Aedes albopictus

aaegypti_new <- aaegypti_raw %>%
  select('species','country/ocean') %>%
  filter(species == 'Aedes aegypti', !is.na(`country/ocean`)) %>%
  rename(country='country/ocean')
# Same process for Aedes aegypti

##_ Map 1: Culex pipiens Distribution------------

cpipiens_country <- cpipiens_new %>%
  group_by(country) %>%
  summarise(present = 1)
# Create a table marking presence of C. pipiens by country

world1 <- ne_countries(scale = "medium", returnclass = "sf")
# Load world country shapes for mapping

world_cpipiens <- left_join(world1, cpipiens_country, by = c("name" = "country"))
# Merge species presence with world map

world_cpipiens$present <- factor(
  ifelse(is.na(world_cpipiens$present), "Absent", "Present"),
  levels = c("Absent", "Present")
)
# Create factor to differentiate absent vs present for plotting

ggplot(world_cpipiens) +
  geom_sf(aes(fill = present), color = "grey40", size = 0.2) +
  scale_fill_manual(values = c("white", "purple")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) +
  labs(title = "Global Distribution of Culex pipiens",
       subtitle = "Based on publicly available BOLD dataset")
# Plot the global map showing where C. pipiens is present

##_ Map 2: Aedes albopictus Distribution------------

abopictus_country <- abopictus_new %>%
  group_by(country) %>%
  summarise(present = 1)
# Summarize presence by country

world2 <- ne_countries(scale = 'medium', returnclass = 'sf')
# Load world map for Aedes albopictus

world_abopictus <- left_join(world2, abopictus_country, by = c('name'='country'))
# Merge presence data with world map

world_abopictus$present <- factor(
  ifelse(is.na(world_abopictus$present), "Absent", "Present"),
  levels = c("Absent", "Present")
)
# Convert NA to "Absent" and factorize for plotting

ggplot(world_abopictus) +
  geom_sf(aes(fill = present), color = 'grey40', size = 0.2) +
  scale_fill_manual(values = c('white', 'orange')) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) +
  labs(title='Global Distribution of Aedes albopictus',
       subtitle = 'Based on publicly available BOLD dataset')
# Plot global map for Aedes albopictus distribution

##_ Map 3: Aedes aegypti Distribution------------

aaegypti_country <- aaegypti_new %>%
  group_by(country) %>%
  summarise(present = 1)
# Summarize presence by country

world3 <- ne_countries(scale='medium', returnclass='sf')
# Load world map for Aedes aegypti

world_aaegypti <- left_join(world3, aaegypti_country, by = c('name'='country'))
# Merge presence data with world map

world_aaegypti$present <- factor(
  ifelse(is.na(world_aaegypti$present), "Absent", "Present"),
  levels = c("Absent", "Present")
)
# Factorize presence for plotting

ggplot(world_aaegypti) +
  geom_sf(aes(fill = present), color ='grey40', size=0.2) +
  scale_fill_manual(values = c('white', 'brown')) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size=16, face='bold', hjust=0.5),
        plot.subtitle = element_text(size=12, hjust=0.5)) +
  labs(title='Global Distribution of Aedes aegypti',
       subtitle = 'Based on publicly available BOLD dataset')
# Plot global map for Aedes aegypti distribution

##_ Combine Species Distributions------------

cpipiens_country <- cpipiens_country %>% mutate(species = 'Culex pipiens')
aaegypti_country <- aaegypti_country %>% mutate(species = 'Aedes aegypti')
abopictus_country <- abopictus_country %>% mutate(species = 'Aedes albopictus')
# Add species column to each dataset for merging

combined_data <- bind_rows(cpipiens_country, aaegypti_country, abopictus_country)
# Combine all three species datasets

species_by_country <- combined_data %>%
  pivot_wider(names_from = species, values_from = present, values_fill = 0)
# Pivot data so each species has its own column (1=present, 0=absent)

world4 <- ne_countries(scale='medium', returnclass='sf')
# Load base world map

world_combined <- left_join(world4, species_by_country, by=c('name'='country'))
# Merge combined species data with world map

world_combined <- world_combined %>%
  mutate(species_richness = `Culex pipiens` + `Aedes aegypti` + `Aedes albopictus`)
# Calculate species richness (number of species present per country)

ggplot(world_combined) +
  geom_sf(aes(fill=species_richness), color='grey40', size=0.2) +
  scale_fill_gradient(low='yellow', high='red', name='Number of Species') +
  theme_minimal() +
  theme(legend.position='right',
        plot.title= element_text(size=16, face='bold', hjust=0.5),
        plot.subtitle = element_text(size=12, hjust=0.5)) +
  labs(title='Global Overlap of Disease-vector Mosquitoes',
       subtitle='Aedes aegypti, Aedes albopictus, and Culex pipiens')
# Visualize combined species richness as a gradient map

##_ Merge Climate Data------------

tavg_files <- list.files("temperature", pattern = "\\.tif$", full.names = TRUE)
tavg_stack <- rast(tavg_files)
# Load monthly average temperature rasters

prec_files <- list.files("precipitation", pattern = ".tif$", full.names = TRUE)
prec_raster <- rast(prec_files)
# Load monthly precipitation rasters

world_vect <- vect(world_combined)
# Convert world map to spatial vector format for extraction

temp_mean <- terra::extract(tavg_stack[[1]], world_vect, fun=mean, na.rm=TRUE)
prec_mean <- terra::extract(prec_raster[[1]], world_vect, fun=mean, na.rm=TRUE)
# Extract mean January temperature and precipitation per country

world_combined$temp_mean <- temp_mean[,2]
world_combined$prec_mean <- prec_mean[,2]
# Store extracted climate values in main dataframe

world_combined <- world_combined %>%
  mutate(temp_scaled = scale(temp_mean)[,1],
         prec_scaled = scale(prec_mean)[,1],
         climate_score = temp_scaled + prec_scaled,
         hotspot_score = species_richness * climate_score)
# Standardize climate variables and compute hotspot score combining richness and climate

ggplot(world_combined) +
  geom_sf(aes(fill=hotspot_score), color='grey40', size=0.2) +
  scale_fill_viridis(option="magma", name="Hotspot Score") +
  theme_minimal() +
  labs(title="Global Mosquito Hotspots (Current Climate)",
       subtitle="Based on BOLD and WorldClim datasets",
       caption="Higher scores = more species + favorable climate") +
  theme(plot.title=element_text(size=16,face='bold',hjust=0.5),
        plot.subtitle=element_text(size=12,hjust=0.5))
# Plot global hotspots combining species richness and climate suitability

##_ Regression Analysis------------

lm_hotspot <- lm(hotspot_score ~ temp_mean + prec_mean, data = world_combined)
print(lm_hotspot)
# Perform linear regression to see which climate variable contributes more to hotspot score

##_ Predictive Hotspots under Warming------------

##__ +2°C and +3°C Warming Scenario Hotspot Scores ------------

world_combined$temp_plus2 <- world_combined$temp_mean + 2
world_combined$temp_plus3 <- world_combined$temp_mean + 3
# Create +2°C and +3°C warming scenarios

world_combined$temp_scaled_plus2 <- scale(world_combined$temp_plus2)[,1]
world_combined$temp_scaled_plus3 <- scale(world_combined$temp_plus3)[,1]
# Scale new temperature scenarios

world_combined$climate_score_plus2 <- world_combined$temp_scaled_plus2
world_combined$climate_score_plus3 <- world_combined$temp_scaled_plus3
# Compute climate score using temperature only

world_combined$hotspot_score_plus2 <- world_combined$species_richness * world_combined$climate_score_plus2
world_combined$hotspot_score_plus3 <- world_combined$species_richness * world_combined$climate_score_plus3
# Predict hotspot scores under warming

ggplot(world_combined) +
  geom_sf(aes(fill=hotspot_score_plus2), color='grey40', size=0.2) +
  scale_fill_viridis(option="plasma", name="Hotspot") +
  theme_minimal() +
  labs(title="Projected Mosquito Hotspots (+2°C)",
       subtitle="Species richness × climate suitability",
       caption="Higher values = more favorable under +2°C warming") +
  theme(plot.title=element_text(size=16,face='bold',hjust=0.5),
        plot.subtitle=element_text(size=12,hjust=0.5))
# Plot hotspots under +2°C warming

ggplot(world_combined) +
  geom_sf(aes(fill=hotspot_score_plus3), color='grey40', size=0.2) +
  scale_fill_viridis(option="plasma", name="Hotspot") +
  theme_minimal() +
  labs(title="Projected Mosquito Hotspots (+3°C)",
       subtitle="Species richness × climate suitability",
       caption="Higher values = more favorable under +3°C warming") +
  theme(plot.title=element_text(size=16,face='bold',hjust=0.5),
        plot.subtitle=element_text(size=12,hjust=0.5))
# Plot hotspots under +3°C warming

##__ Change (Δ) in Hotspot Scores Compared to Current Conditions ------------

world_combined$delta_plus2 <- world_combined$hotspot_score_plus2 - world_combined$hotspot_score
world_combined$delta_plus3 <- world_combined$hotspot_score_plus3 - world_combined$hotspot_score
# Compute changes in hotspot scores compared to current climate

ggplot(world_combined) +
  geom_sf(aes(fill=delta_plus2), color="grey40", size=0.2) +
  scale_fill_viridis(option="plasma", name="Δ Hotspot", direction=-1) +
  theme_minimal() +
  labs(title="Change in Mosquito Hotspots (+2°C)",
       subtitle="Increase in hotspot score",
       caption="Positive values = more favorable under warming") +
  theme(plot.title=element_text(size=16,face='bold',hjust=0.5),
        plot.subtitle=element_text(size=12,hjust=0.5))
# Map showing increase or decrease in suitability under +2°C warming

ggplot(world_combined) +
  geom_sf(aes(fill=delta_plus3), color="grey40", size=0.2) +
  scale_fill_viridis(option="plasma", name="Δ Hotspot", direction=-1) +
  theme_minimal() +
  labs(title="Change in Mosquito Hotspots (+3°C)",
       subtitle="Increase in hotspot score",
       caption="Positive values = more favorable under warming") +
  theme(plot.title=element_text(size=16,face='bold',hjust=0.5),
        plot.subtitle=element_text(size=12,hjust=0.5))
# Map showing increase or decrease in suitability under +3°C warming