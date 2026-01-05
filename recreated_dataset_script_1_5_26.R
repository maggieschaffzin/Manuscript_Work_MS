#Project: Recreating dataset averages from honors and analyses for Manuscript
#By: Maggie Schaffzin
#Created: October 13th, 2025
#Last edited: December 15th, 2025

#Installing and loading required packages
required_packages <- c("tidyverse", "readr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))
library(tidyverse)
library(readr)
library(dplyr)

#Re-creating a project directory (Getting new computer messed this up)
proj_dir <- "C:/Users/bethm/Documents/Manuscript Work/Datasets"
if(!dir.exists(proj_dir)) dir.create(proj_dir, recursive = TRUE)
setwd(proj_dir)

#Loading Environmental Data 
env_file <- "C:/Users/bethm/Downloads/all_env_lat_lon_jan15_2025.csv"
all_env <- read_csv(env_file)

#Selecting relevant columns
env_selected <- all_env %>%
  dplyr::select(
    Site,
    mean_chl_mg_m3,
    mean_sst_C,
    DepthZone,
    dist_200m_bath,
    giantkelp_density_m2,
    Relief_index,
    Relief_SD,
    Substrate_index,
    Substrate_SD,
    Latitude,
    Longitude
  )

#Creating averages for Relief Index, Relief SD, Substrate Index, Substrate SD, and Giant Kelp Density
env_means <- env_selected %>%
  group_by(Site, DepthZone) %>%
  summarise(
    mean_giantkelp_density_m2 = mean(giantkelp_density_m2, na.rm = TRUE),
    mean_Relief_index = mean(Relief_index, na.rm = TRUE),
    mean_Relief_SD = mean(Relief_SD, na.rm = TRUE),
    mean_Substrate_index = mean(Substrate_index, na.rm = TRUE),
    mean_Substrate_SD = mean(Substrate_SD, na.rm = TRUE),
    mean_chl_mg_m3 = mean(mean_chl_mg_m3, na.rm = TRUE),
    mean_sst_C = mean(mean_sst_C, na.rm = TRUE),
    dist_200m_bath = mean(dist_200m_bath, na.rm = TRUE),
    mean_Latitude = mean(Latitude, na.rm = TRUE),
    mean_Longitude = mean(Longitude, na.rm = TRUE)
  ) %>%
  ungroup()

#Saving new averaged dataset
write_csv(env_means, "env_selected_means_by_site_depthzone.csv")

#Recalculating gorgonian densities (mean by site)
gorg_file <- "C:/Users/bethm/Downloads/dat_gorgonian_oct18_2024 (1).csv"
library(readr)
dat_gorgonian <- read_csv(gorg_file)
head(dat_gorgonian)

#Refining dataset and calculating gorgonian density
dat_gorgonian_refined <- dat_gorgonian %>%
  dplyr::select(
    Region,
    Site,
    BenthicReefSpecies,
    DepthZone,
    Abundance,
    area.m2
  ) %>%
  dplyr::mutate(
    gorgonian_density = (Abundance / area.m2) * 100
  ) %>%
  dplyr::select(
    Region,
    Site,
    BenthicReefSpecies,
    DepthZone,
    gorgonian_density
  )
#Calculating mean gorgonian density by site and depth zone
mean_gorgonian_density <- dat_gorgonian_refined %>%
  group_by(Site, DepthZone, BenthicReefSpecies) %>%
  summarise(mean_gorgonian_density = mean(gorgonian_density, na.rm = TRUE)) %>%
  ungroup()
head(mean_gorgonian_density)

#Merging mean gorgonian data and environmental data by site and depth zone
env_gorg_combined <- env_means %>%
  left_join(mean_gorgonian_density, by = c("Site", "DepthZone"))

#Checking the first few rows
head(env_gorg_combined)

#Creating column with reef type
env_gorg_combined <- env_gorg_combined %>%
  dplyr::mutate(
    Reef_Type = dplyr::case_when(
      stringr::str_detect(Site, "AR|PVR") ~ "Artificial Reef",
      TRUE ~ "Natural Reef"
    )
  )
#Saving combined dataset
write_csv(env_gorg_combined, "env_gorg_means_by_site_depthzone.csv")

#Adding in average depth values for each site
depth_file <- "C:/Users/bethm/Downloads/gorgonian/Data/VRG_CRANE_site_depthzone_depth.csv"
depth_data <- read_csv(depth_file)
head(depth_data)
env_gorg_combined_depth <- env_gorg_combined %>%
  left_join(depth_data, by = c("Site", "DepthZone"))
head(env_gorg_combined_depth)
write_csv(env_gorg_combined_depth, "env_gorg_means_by_site_depthzone_with_avgdepth.csv")

#Adding in CCA data by site and depthzone
cca_file <- "C:/Users/bethm/Downloads/gorgonian/Data/UPC_CCA_oct18_2024.csv"
cca_data <- read_csv(cca_file)
head(cca_data)
cca_means <- cca_data %>%
  group_by(Site, DepthZone) %>%
  summarise(mean_CCA_cover = mean(BRS_per_cov, na.rm = TRUE)) %>%
  ungroup()
head(cca_means)
env_gorg_combined_depth <- read_csv("env_gorg_means_by_site_depthzone_with_avgdepth.csv")

#Merging mean CCA data with  combined dataset
env_gorg_final <- env_gorg_combined_depth %>%
  left_join(cca_means, by = c("Site", "DepthZone"))

#Checking dataset
head(env_gorg_final)

#Saving final dataset with CCA included
write_csv(env_gorg_final, "env_gorg_means_by_site_depthzone_with_avgdepth_CCA.csv")


#Synthesizing PISCO Gorgonian Data
# Loading required new libraries
library(stringr)

#Loading data
pisco_file <- "C:/Users/bethm/Downloads/Gorgonian/Data/PISCO_gorgonian_swath_16_23.csv"
PISCO_gorgonian <- read_csv(pisco_file)
head(PISCO_gorgonian)

#Calculating gorgonian density for PISCO data
PISCO_gorgonian_refined <- PISCO_gorgonian %>%
  dplyr::select(species_definition, site, zone, count, depth) %>%
  dplyr::mutate(
    mean_density = (count / 60) * 100   
  )

head(PISCO_gorgonian_refined)

# Calculating mean depth per site, depth zone, and species
PISCO_gorgonian_means <- PISCO_gorgonian_refined %>%
  dplyr::filter(!is.na(depth)) %>%     
  dplyr::group_by(site, species_definition, zone) %>%
  dplyr::summarise(
    mean_depth   = mean(depth, na.rm = TRUE),
    mean_gorg_density = mean(mean_density),
    .groups = "drop"
  ) %>%

#Creating Reef Type category for PISCO data (they're all natural reefs but need for merging w VRG data)
   dplyr::mutate(
    Reef_Type = dplyr::case_when(
      stringr::str_detect(site, "AR|PVR") ~ "Artificial Reef",
      TRUE ~ "Natural Reef"
    )
  )

#Checking results
head(PISCO_gorgonian_means)

# Ensuring columns match environmental dataset naming conventions
PISCO_gorg_averages <- PISCO_gorgonian_means %>%
  rename(
    DepthZone = zone,
    Site = site
  )

#Synthesizing PISCO environmental dataset
pisco_env_file <- "C:/Users/bethm/Downloads/Gorgonian/Data/PISCO_all_env_lat_lon.csv"
PISCO_all_env <- read_csv(pisco_env_file)

head(PISCO_all_env)

#Selecting relevant columns (same structure as my first dataset)
PISCO_env_selected <- PISCO_all_env %>%
  dplyr::select(
    Site,
    mean_chl_mg_m3,
    mean_sst_C,
    DepthZone,
    dist_200m_bath,
    giantkelp_plant_density_m2,
    Relief_index,
    Relief_SD,
    Substrate_index,
    Substrate_SD,
    Latitude,
    Longitude
  )

#Creating averages for each site and depth zone
PISCO_env_means <- PISCO_env_selected %>%
  group_by(Site, DepthZone) %>%
  summarise(
    mean_giantkelp_density_m2 = mean(giantkelp_plant_density_m2, na.rm = TRUE),
    mean_Relief_index = mean(Relief_index, na.rm = TRUE),
    mean_Relief_SD = mean(Relief_SD, na.rm = TRUE),
    mean_Substrate_index = mean(Substrate_index, na.rm = TRUE),
    mean_Substrate_SD = mean(Substrate_SD, na.rm = TRUE),
    mean_chl_mg_m3 = mean(mean_chl_mg_m3, na.rm = TRUE),
    mean_sst_C = mean(mean_sst_C, na.rm = TRUE),
    dist_200m_bath = mean(dist_200m_bath, na.rm = TRUE),
    mean_Latitude = mean(Latitude, na.rm = TRUE),
    mean_Longitude = mean(Longitude, na.rm = TRUE)
  ) %>%
  ungroup()

#Checking dataset
head(PISCO_env_means)

#Merging the new gorgonian averages dataset with the environmental dataset
PISCO_merged <- PISCO_gorg_averages %>%
  left_join(PISCO_env_means, by = c("Site", "DepthZone"))

#Checking merged dataset
head(PISCO_merged)

#Saving the merged dataset
write_csv(PISCO_merged, "PISCO_gorgonian_env_merged.csv")

#Refining PISCO CCA data
pisco_cca <- read_csv("PISCO_CCA_UPC_16_23.csv")

#Selecting relevant columns and calculate mean pct_cover for each site/ depth zone
pisco_summary <- pisco_cca %>%
  dplyr::select(species_definition, site, zone, pct_cov) %>%
  dplyr::group_by(site, zone) %>%
  dplyr::summarise(mean_pct_cover = mean(pct_cov, na.rm = TRUE)) %>%
  dplyr::ungroup()

#Checking dataset
head(pisco_summary)

#Merging only sites/zones that exist in environmental dataset
PISCO_final_merged <- pisco_summary %>%
  inner_join(PISCO_merged, by = c("site" = "Site", "zone" = "DepthZone")) %>%
  drop_na()   

#Checking merged dataset
head(PISCO_final_merged)

#Saving merged dataset
write_csv(PISCO_final_merged, "PISCO_final_env_CCA_merged.csv")

#Merging VRG Dataset with PISCO dataset
VRG_final <- env_gorg_final
PISCO_final <- PISCO_final_merged

#Standardizing PISCO column names to match VRG column names
PISCO_final <- PISCO_final_merged %>%
  dplyr::rename(
    Site                   = site,
    DepthZone              = zone,
    mean_CCA_cover         = mean_pct_cover,
    BenthicReefSpecies     = species_definition,
    mean_gorgonian_density = mean_density,
    mean_surveydepth       = mean_depth  
  )

#Checking cleaned dataset
head(PISCO_final)

#Adding an identifier column to keep track of dataset origin
VRG_final <- VRG_final %>%
  mutate(Source = "VRG")
PISCO_final <- PISCO_final %>%
  mutate(Source = "PISCO")

#Aligning column names before merging
common_cols <- intersect(names(VRG_final), names(PISCO_final))

#Combining the two datasets, keeping only matching columns
Unified_gorgonian_dataset <- bind_rows(
  VRG_final[, common_cols],
  PISCO_final[, common_cols]
)

#Checking structure of the final dataset
glimpse(Unified_gorgonian_dataset)

#Saving the unified dataset for linear analyses
write_csv(Unified_gorgonian_dataset, "Unified_VRG_PISCO_gorgonian_dataset.csv")
