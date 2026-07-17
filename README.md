# Abiotic and Biotic Drivers of Gorgonian Octocoral Distribution in the Southern California Bight

**Schaffzin, M.** | Vantuna Research Group, Occidental College | *In preparation*

---

## Overview

This repository contains all data, analysis scripts, and figure code associated with a species-resolved assessment of the environmental and habitat drivers shaping gorgonian octocoral distributions across the Southern California Bight (SCB). The study integrates long-term subtidal survey data from two complementary monitoring programs — the Vantuna Research Group (VRG) and the Partnership for Interdisciplinary Studies of Coastal Oceans (PISCO) — spanning 145 natural and artificial reef sites surveyed between 2016 and 2023.

Four gorgonian species are examined: *Eugorgia rubens*, *Leptogorgia chilensis*, *Muricea californica*, and *Muricea fruticosa*. Predictors include survey depth, reef type (natural vs. artificial), substrate composition and heterogeneity, reef relief and heterogeneity, crustose coralline algae (CCA) percent cover, giant kelp stipe density, sea surface temperature (SST), chlorophyll-a concentration, distance to the 200 m bathymetric contour, and a custom wind-weighted wave exposure index (WEI) derived from NDBC buoy wind climatology and GSHHG coastline fetch calculations. All continuous predictors were z-scored prior to model fitting to allow direct comparison of standardized effect sizes.

---

## Repository Structure

```
Manuscript_Work_MS/
│
├── Final Scripts/                        # Production-ready R scripts for all analyses and figures
│   ├── 01_data_prep.R                    # Data cleaning, column standardization, VRG/PISCO merge
│   ├── 02_wave_exposure_index.R          # GSHHG fetch calculation, NDBC wind weighting, WEI derivation
│   ├── 03_GLMM_analysis.R               # Per-species LMM/LM models, R² extraction, results export
│   ├── 04_AIC_model_selection.R          # AICc-based dredge and model averaging (MuMIn)
│   ├── 05_figures_main.R                 # All main manuscript figures (Figs. 1-3)
│   ├── 06_sensitivity_analyses.R         # GLMM sensitivity and robustness checks
│   └── 07_figures_supplemental.R         # Supplemental figures
│
├── Datasets/                             # Processed datasets (see Data section below)
│
├── Figures/                              # Output figures (PDF and PNG at 300-600 dpi)
│
├── Old Versions of Scripts/              # Version history of analysis scripts
│
└── Data and Figure Exploration/          # Exploratory analyses and visualizations (pre-manuscript)
```

---

## Data Sources and Processing

### Survey Data

**VRG (Vantuna Research Group)**
Subtidal benthic surveys were conducted by the VRG at natural rocky reefs and artificial reefs along the mainland coast of the SCB, spanning sites from Point Conception south to San Diego, as well as on Santa Catalina, San Clemente, and Santa Barbara Islands. Surveys followed the standardized Co-operative Research and Assessment of Nearshore Ecosystems (CRANE) protocol (Pondella et al., 2015), employing stratified SCUBA surveys across four depth zones: Inner (~5 m), Middle (~10 m), Outer (~15 m), and Deep (~20 m). Each survey consisted of two 30 m transects oriented parallel to depth contours. Gorgonian density was quantified using the Uniform Point Contact (UPC) method (Meese & Tomich, 1992; Pondella et al., 2015) and calculated as the number of colonies per 100 m² based on transect dimensions (30 m × 2 m = 60 m² per transect). Benthic predictors including CCA percent cover, reef relief, substrate composition, and giant kelp stipe density were measured simultaneously following established VRG protocols.

**PISCO (Partnership for Interdisciplinary Studies of Coastal Oceans)**
PISCO surveys expanded spatial coverage to include the Northern Channel Islands (Anacapa, Santa Cruz, Santa Rosa, San Miguel) and adjacent Santa Barbara mainland reefs, using comparable belt transect methods (Carr & Reed, 2016). PISCO data are publicly archived and available at:
https://search.dataone.org/view/doi%3A10.6085%2FAA%2FPISCO_kelpforest.1.11

**Integration**
VRG and PISCO datasets were merged by standardizing column names, depth zone labels, and species nomenclature. All continuous predictors were averaged across transects and survey years (2016–2023) within each site to produce single time-integrated site-level estimates. A single mean depth value per site was derived by averaging depth values recorded by dive computers across all transects and years. Artificial reef VRG data are available directly from the Vantuna Research Group (Occidental College) and are not publicly archived.

### Environmental Covariates

**Sea Surface Temperature and Chlorophyll-a**
Mean SST (°C) and chlorophyll-a concentration (mg/m³) were extracted from 8-day composite MODIS Aqua satellite products (4 km resolution) obtained from NASA's Ocean Color Web (https://oceancolor.gsfc.nasa.gov). Values were extracted from the grid cell nearest each survey site and averaged across the full study period (2016–2023).

**Distance to 200 m Bathymetric Contour**
Distance to the 200 m isobath was used as a proxy for proximity to the continental shelf break and associated upwelling influence. Distances were calculated as the shortest Euclidean distance from each survey site to the 200 m depth contour, derived from bathymetric data obtained from the Southern California Coastal Ocean Observing System (SCCOOS) and NOAA's National Centers for Environmental Information, computed in ArcGIS Pro 3.0.

**Wave Exposure Index**
A custom site-level wave exposure index (WEI) was derived using a ray-casting algorithm implemented in R. Fetch was calculated for each of 145 survey sites across 16 compass bearings (0–337.5° at 22.5° intervals) by intersecting projected rays against the GSHHG high-resolution coastline (version 2.3.7; Wessel & Smith, 1996). A maximum fetch distance of 500 km was applied. Raw fetch distances were weighted by the prevailing wind direction climatology from the nearest of five NDBC buoys distributed across the SCB (stations 46025 — Santa Monica Basin; 46053 — East Santa Barbara; 46054 — West Santa Barbara; 46086 — San Clemente Basin; 46047 — Point Conception/San Diego), using hourly wind direction observations from 2014–2024. Weighted fetch values were min-max normalized to a 0–1 scale across all 145 sites to produce the final WEI, where 0 = most sheltered and 1 = most exposed. Full methodological details are provided in `02_wave_exposure_index.R` and in the manuscript Supplementary Methods S1.

---

## Processed Datasets

| File | Description |
|------|-------------|
| `Unified_VRG_PISCO_gorgonian_dataset.csv` | Merged VRG and PISCO gorgonian density and environmental/habitat data by site and depth zone, prior to wave exposure integration |
| `Unified_VRG_PISCO_gorgonian_dataset_with_exposure.csv` | Final analysis dataset with wave exposure index appended — primary input for all models and figures |
| `wave_exposure_index_by_site.csv` | Per-site mean fetch, max fetch, wind-weighted fetch, wave energy proxy, and scaled WEI |
| `ndbc_wind_weight_summary.csv` | Wind direction climatology frequency weights per bearing for each of 5 NDBC buoys (2014–2024) |
| `GLMM_exposure_results_all_species.csv` | Per-species model coefficient estimates, standard errors, t-values, and R² values |
| `GLMM_exposure_R2_summary.csv` | Marginal and conditional R² by species and model type (LM vs. LMM) |
| `GLMM_exposure_overall_results.csv` | Community-level (pooled species) model fixed effect estimates |

### Key Variables in Final Analysis Dataset

| Variable | Description | Units | Source |
|----------|-------------|-------|--------|
| `mean_gorgonian_density` | Mean gorgonian density per site × depth zone × species | ind./100 m² | VRG / PISCO |
| `BenthicReefSpecies` | Gorgonian species (*E. rubens*, *L. chilensis*, *M. californica*, *M. fruticosa*) | — | VRG / PISCO |
| `mean_surveydepth` | Mean survey depth averaged across transects and years | m | VRG / PISCO |
| `Reef_Type` | Reef origin: Natural Reef or Artificial Reef | — | VRG |
| `mean_CCA_cover` | Mean percent cover of crustose coralline algae | % | VRG / PISCO |
| `mean_giantkelp_density_m2` | Mean giant kelp stipe density | stipes/m² | VRG / PISCO |
| `mean_Relief_index` | Mean reef relief index (weighted average of relief categories: 0, 1, 2) | 0–2 | VRG |
| `mean_Relief_SD` | Within-site standard deviation of relief index across transects | — | VRG |
| `mean_Substrate_index` | Mean substrate index (weighted average: sand=1, cobble=2, boulder=3, bedrock=4) | 1–4 | VRG |
| `mean_Substrate_SD` | Within-site standard deviation of substrate index across transects | — | VRG |
| `mean_sst_C` | Mean sea surface temperature (2016–2023) | °C | MODIS Aqua |
| `mean_chl_mg_m3` | Mean chlorophyll-a concentration (2016–2023) | mg/m³ | MODIS Aqua |
| `dist_200m_bath` | Euclidean distance to 200 m bathymetric contour | m | SCCOOS / NOAA |
| `exposure_index_scaled` | Wind-weighted wave exposure index, min-max normalized | 0–1 | NDBC / GSHHG (this study) |
| `nearest_buoy` | ID of nearest NDBC buoy used for WEI wind weighting | — | NDBC |
| `Source` | Dataset origin (VRG or PISCO) | — | — |

---

## Statistical Analysis

Linear mixed-effects models (LMMs) were fit separately for each of the four species using `lme4`, with site included as a random intercept to account for non-independence of observations within sites. All continuous predictors were z-scored prior to model fitting to allow direct comparison of standardized effect sizes across variables measured on different scales. Models with singular fits (site random effect variance ≈ 0) were refit as plain linear models (LMs); this occurred for species with limited among-site variance. Model fit was assessed using marginal R² (variance explained by fixed effects alone) and conditional R² (variance explained by fixed and random effects combined), calculated using the `performance` package.

### R Packages

| Package | Version | Use |
|---------|---------|-----|
| `lme4` | 1.1-35 | Linear mixed-effects models |
| `performance` | 0.10.8 | Marginal and conditional R² |
| `broom.mixed` | 0.2.9 | Model tidying and coefficient extraction |
| `MuMIn` | 1.47.5 | AICc-based model selection and averaging |
| `sf` | 1.0-14 | Spatial operations and fetch ray-casting |
| `lwgeom` | 0.2-15 | Geometric operations for fetch calculation |
| `tidyverse` | 2.0.0 | Data wrangling and visualization |
| `ggplot2` | 3.4.4 | Figure production |
| `cowplot` | 1.1.1 | Multi-panel figure assembly |
| `rnaturalearth` | 0.3.4 | Basemap data for figures |
| `ggspatial` | 1.1.9 | Map annotations |
| `viridis` | 0.6.4 | Color scales |

**R version:** 4.4.2 (2024-10-31, "Pile of Leaves"), Windows x86_64

---

## Notes on Species Coverage

*Leptogorgia alba* was excluded from all analyses due to insufficient occurrence data (fewer than three individuals detected across the full survey dataset) and unresolved taxonomic identification at the time of analysis. Positive species-level identification could not be confirmed in consultation with taxonomic specialists at the Santa Barbara Museum of Natural History.

---

## Reproducibility

All analysis scripts are numbered in order of execution (`01` through `04`). Running scripts in order from a fresh R session with the raw datasets in `Datasets/` will reproduce all results and figures. The wave exposure index computation (`02_wave_exposure_index.R`) downloads a ~100 MB GSHHG shapefile on first run and caches NDBC wind weight results locally to avoid redundant downloads on subsequent runs.

---

## Data Availability

PISCO kelp forest monitoring data are publicly available at:
https://search.dataone.org/view/doi%3A10.6085%2FAA%2FPISCO_kelpforest.1.11

VRG natural reef data are available through the same archive. Artificial reef VRG data can be obtained directly from the Vantuna Research Group, Occidental College.

Satellite environmental data (SST, Chl-a) are available from NASA Ocean Color Web:
https://oceancolor.gsfc.nasa.gov

NDBC buoy wind data are available from:
https://www.ndbc.noaa.gov

GSHHG coastline data are available from:
https://github.com/GenericMappingTools/gshhg-gmt

---

## Citation

Schaffzin, M. (*in preparation*). Abiotic and biotic drivers of gorgonian octocoral distribution in the Southern California Bight. Vantuna Research Group, Occidental College.

---

## Contact

Margaret Schaffzin — maggies124@verizon.net

