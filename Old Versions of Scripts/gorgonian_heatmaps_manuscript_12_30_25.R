# Project: Making Species-specific Gorgonian Heatmaps
# By: Maggie Schaffzin
# Created: December 29th, 2025
# Last edited: December 30th, 2025

#Loading required packages
library(tidyverse)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(MBA)
library(cowplot)
library(patchwork)

#Loading and cleaning gorg density data
gorg_data <- read.csv("C:/Users/bethm/Documents/Manuscript Work/Datasets/Unified_VRG_PISCO_gorgonian_dataset.csv")
gorg_data_clean <- gorg_data %>%
  filter(!is.na(mean_Latitude) & !is.na(mean_Longitude) & !is.na(mean_gorgonian_density))

#Loading in california coastline
coastline <- ne_states(country = "United States of America", returnclass = "sf") %>%
  st_transform(crs = 4326) %>%
  filter(name == "California")

#Defining bounding box for california  image and southern california bight image
scb_bbox <- st_as_sfc(st_bbox(c(xmin = -122, xmax = -116.5, ymin = 32.5, ymax = 34.75), crs = st_crs(4326)))
california_bbox <- st_as_sfc(st_bbox(coastline))

#Creating heat map for each species
create_species_heatmap <- function(species_name, data, save_path){
  
#Filtering for species
  species_data <- data %>% filter(BenthicReefSpecies == species_name)
  
  if(nrow(species_data) == 0){
    message(paste("No data for", species_name))
    return(NULL)
  }
  
#MBA interpolation
  mba_fit <- mba.surf(
    cbind(species_data$mean_Longitude, 
          species_data$mean_Latitude, 
          species_data$mean_gorgonian_density),
    no.X = 300, no.Y = 300, extend = TRUE
  )$xyz.est
  
  interp_df <- expand.grid(
    Longitude = mba_fit$x,
    Latitude = mba_fit$y
  )
  interp_df$Density <- as.vector(mba_fit$z)
  interp_df <- interp_df %>% filter(!is.na(Density))
  
#Plotting main heatlap (overall density)
  main_plot <- ggplot() +
    geom_tile(data = interp_df, aes(x = Longitude, y = Latitude, fill = Density)) +
    geom_sf(data = coastline, fill = "grey80", color = "black") +
    scale_fill_viridis(name = "Gorgonian\nDensity", option = "C", na.value = "white") +
    coord_sf(xlim = c(-120.1, -116.5), ylim = c(32.5, 34.75)) +
    theme_minimal() +
    labs(
      title = species_name,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    )
  
# Creating california inset map
  inset_map <- ggplot() +
    geom_sf(data = coastline, fill = "lightgrey", color = "black") +
    geom_sf(data = california_bbox, fill = NA, color = "black", size = 1) +
    geom_sf(data = scb_bbox, fill = NA, color = "red", size = 1) +
    annotate("text", x = -119, y = 32, label = "California", size = 3.5, fontface = "bold") +
    theme_void()
  
#Combining main plot w inset
  final_plot <- ggdraw() +
    draw_plot(main_plot) +
    draw_plot(inset_map, x = 0.59, y = 0.54, width = 0.2, height = 0.2)
  
#Saving individual species heatmap
  ggsave(
    filename = paste0(species_name, "_Heatmap.png"),
    plot = final_plot,
    path = save_path,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  return(final_plot)
}

#Creating/checking directory to save heatmaps
save_folder <- "C:/Users/bethm/Documents/Manuscript Work/Figures/Species_Heatmaps"
dir.create(save_folder, showWarnings = FALSE)

#Trying out another way to create heatmaps for a paneled approach
species_list <- unique(gorg_data_clean$BenthicReefSpecies)
species_plots <- list()

for(species in species_list){
  message("Creating heatmap for: ", species)
  p <- create_species_heatmap(species, gorg_data_clean, save_folder)
  if(!is.null(p)) species_plots[[species]] <- p
}

#Making sure all plots have legend on the right
species_plots <- lapply(species_plots, function(p) p + theme(legend.position = "right"))

#Arranging paneled plots into 3 columns
combined_plot <- wrap_plots(species_plots, ncol = 3) 

# Saving combined panel figure
ggsave(
  filename = "All_Species_Heatmaps_Panel.png",
  plot = combined_plot,
  path = save_folder,
  width = 20,
  height = 15,
  dpi = 300
)

