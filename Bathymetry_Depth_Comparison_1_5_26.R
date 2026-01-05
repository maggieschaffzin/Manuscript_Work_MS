# Project: Comparing Dive Computer Depth to USGS Bathymetry
# Data: Unified_VRG_PISCO_gorgonian_dataset
# By: Maggie Schaffzin (with help from Dr. Zoe Kitchel's bathy function)
# Created: December 15th, 2025
# Modified: January 5th, 2026

#Loading packages
library(sf)
library(data.table)
library(dplyr)
library(raster)
library(marmap)
library(ggplot2)

getwd()
#Sourcing Zoe's bathymetry function
source("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/main/scripts/bathy_join_function.R")

#Preparing data for bathymetry join
#Loading dataset
Unified_VRG_PISCO_gorgonian_dataset <- read_csv(
  "Unified_VRG_PISCO_gorgonian_dataset.csv"
)

#Selecting and renaming columns to match function requirements
lat_lon <- Unified_VRG_PISCO_gorgonian_dataset %>%
  dplyr::select(
    Site,
    Latitude  = mean_Latitude,
    Longitude = mean_Longitude,
    mean_surveydepth
  ) %>%
  distinct()  

#Running bathymetry join
df.with_depth <- add_depth_columns(
  lat_lon,
  grouping_var = "Site",
  USGS_socal = TRUE
)

#Cleaning and preparing comparison dataset
depth_comparison <- df.with_depth %>%
  dplyr::rename(
    bathy_depth = highrez_depth,
    dive_depth  = mean_surveydepth
  ) %>%
  dplyr::filter(!is.na(bathy_depth), !is.na(dive_depth)) %>%
  dplyr::mutate(
    depth_difference = dive_depth - bathy_depth
  )

#Summarizing statistics
depth_summary <- depth_comparison %>%
  summarise(
    n_sites = n(),
    mean_difference_m = mean(depth_difference),
    sd_difference_m   = sd(depth_difference),
    correlation_r     = cor(dive_depth, bathy_depth)
  )

print(depth_summary)

#Visualizing dive computer depth vs Bathymetry
ggplot(depth_comparison,
       aes(x = bathy_depth, y = dive_depth)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "USGS DEM Bathymetry Depth (m)",
    y = "Mean Survey Depth from Dive Computers (m)"
    
  ) +
  theme_classic()

##Visualizing depth difference vs bathymetry (helpful for discussion)
ggplot(depth_comparison,
       aes(x = bathy_depth, y = depth_difference)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "USGS DEM Bathymetry Depth (m)",
    y = "Dive Depth – Bathymetry Depth (m)"
  
  ) +
  theme_classic()

