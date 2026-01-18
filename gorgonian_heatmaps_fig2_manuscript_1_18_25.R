# Project: Making Species-specific Gorgonian Heatmaps (FIGURE 2)
# By: Maggie Schaffzin
# Created: December 29th, 2025
# Last edited: January 17th, 2026

#Loading packages
library(tidyverse)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(akima)
library(cowplot)
library(patchwork)
library(ggrepel)
library(ggspatial)

#Loading and clean data
gorg_data <- read.csv("C:/Users/bethm/Documents/Manuscript Work/Datasets/Unified_VRG_PISCO_gorgonian_dataset.csv")

gorg_data_clean <- gorg_data %>% 
  filter(!is.na(mean_Latitude), 
         !is.na(mean_Longitude), 
         !is.na(mean_gorgonian_density), 
         !is.na(BenthicReefSpecies))

#Loading California coastline
coastline <- ne_states(
  country = "United States of America", 
  returnclass = "sf"
) %>%
  st_transform(4326) %>% 
  filter(name == "California")

#Defining bounding boxes
scb_bbox <- st_as_sfc(st_bbox(
  c(xmin = -121.2, xmax = -116.5, ymin = 32.5, ymax = 34.75), 
  crs = st_crs(4326)
))

#Defining geographic labels (Channel Islands)
island_labels <- data.frame(
  location = c("Santa Cruz\nIsland", "Santa Rosa\nIsland", 
               "San Miguel\nIsland", "Anacapa\nIsland",
               "Santa Barbara\nIsland", "San Nicolas\nIsland",
               "Santa Catalina\nIsland", "San Clemente\nIsland"),
  lon = c(-119.85, -120.10, -120.40, -119.40,
          -119.03, -119.48, -118.42, -118.50),
  lat = c(34.00, 34.00, 34.05, 34.00,
          33.48, 33.25, 33.35, 32.87)
)

#Calculating global max density across all species (for consistent color scales)
global_max_density <- gorg_data_clean %>%
  group_by(BenthicReefSpecies) %>%
  summarise(max_density = max(mean_gorgonian_density, na.rm = TRUE)) %>%
  pull(max_density) %>%
  max(na.rm = TRUE)

#Creating species-specific heatmap
create_species_heatmap <- function(species_name, data, global_max = NULL, save_path = NULL) {
  
  #Filtering for species
  species_data <- data %>% filter(BenthicReefSpecies == species_name)
  
  if(nrow(species_data) == 0) {
    message(paste("No data for", species_name))
    return(NULL)
  }
  
  #Separating positive densities from zeros
  species_pos <- species_data %>% filter(mean_gorgonian_density > 0)
  species_zero <- species_data %>% filter(mean_gorgonian_density == 0)
  
  #Calculating max density for scaling
  max_density <- max(species_data$mean_gorgonian_density, na.rm = TRUE)
  
  #Using global max if provided (for consistent cross-species comparison)
  scale_max <- ifelse(!is.null(global_max), global_max, max_density)
  
  #Checking if this species has very sparse/low density data
  is_sparse <- (nrow(species_pos) < 3)
  
  #Initializing interpolation dataframe
  interp_df <- NULL
  
  #Interpolating ONLY if there are positive observations and not too sparse
  if(nrow(species_pos) >= 3 && !is_sparse) {
    
    #Using akima interpolation
    interp_res <- with(species_pos, akima::interp(
      x = mean_Longitude, 
      y = mean_Latitude, 
      z = mean_gorgonian_density,
      xo = seq(min(species_pos$mean_Longitude) - 0.02, 
               max(species_pos$mean_Longitude) + 0.02, 
               length = 300),
      yo = seq(min(species_pos$mean_Latitude) - 0.02, 
               max(species_pos$mean_Latitude) + 0.02, 
               length = 300),
      linear = FALSE,
      extrap = FALSE,
      duplicate = "mean"
    ))
    
    #Converting to dataframe
    interp_df <- expand.grid(
      Longitude = interp_res$x,
      Latitude = interp_res$y
    )
    interp_df$Density <- as.vector(interp_res$z)
    
    #Removing NAs
    interp_df <- interp_df %>% filter(!is.na(Density) & Density >= 0)
    
    # Additional filtering (removing interpolated points that are too far from any survey point)
    if(nrow(interp_df) > 0 && nrow(species_data) > 0) {
      #Calculating distance to nearest survey point for each interpolated point
      for(i in 1:nrow(interp_df)) {
        min_dist <- min(sqrt(
          (interp_df$Longitude[i] - species_data$mean_Longitude)^2 + 
            (interp_df$Latitude[i] - species_data$mean_Latitude)^2
        ))
        # Removing if more than 5km from any survey point
        if(min_dist > 0.05) {
          interp_df$Density[i] <- NA
        }
      }
      interp_df <- interp_df %>% filter(!is.na(Density))
    }
  }
  
#Creating main plot
  main_plot <- ggplot() +
    
    #Ocean background
    geom_rect(
      aes(xmin = -120.1, xmax = -116.5, ymin = 32.5, ymax = 34.75), 
      fill = "aliceblue"
    ) +
    
    #Interpolating density surface 
    {if(!is.null(interp_df) && nrow(interp_df) > 0) {
      geom_raster(
        data = interp_df, 
        aes(x = Longitude, y = Latitude, fill = Density), 
        interpolate = TRUE
      )
    }} +
    
    #Loading in california coastline
    geom_sf(
      data = coastline, 
      fill = "grey85", 
      color = "black", 
      linewidth = 0.4
    ) +
    
    #Sampling points (positive densities)
    {if(nrow(species_pos) > 0 && !is_sparse) {
      geom_point(
        data = species_pos,
        aes(x = mean_Longitude, y = mean_Latitude),
        shape = 21,
        size = 1.2,
        fill = "white",
        color = "black",
        stroke = 0.3,
        alpha = 0.6
      )
    }} +
    
    #Showing density as sized/colored points instead (for  sparse L. alba data)
    {if(is_sparse && nrow(species_pos) > 0) {
      geom_point(
        data = species_pos,
        aes(x = mean_Longitude, y = mean_Latitude, 
            size = mean_gorgonian_density,
            fill = mean_gorgonian_density),
        shape = 21,
        color = "black",
        stroke = 0.5,
        alpha = 0.8
      )
    }} +
    
    #Zero observations as open circles
    {if(nrow(species_zero) > 0) {
      geom_point(
        data = species_zero,
        aes(x = mean_Longitude, y = mean_Latitude),
        shape = 1,  # Open circle
        size = 1.5,
        color = "grey30",
        stroke = 0.4
      )
    }} +
    
    #Adding island labels
    geom_text_repel(
      data = island_labels,
      aes(x = lon, y = lat, label = location),
      size = 2.5,
      fontface = "italic",
      color = "grey20",
      bg.color = "white",
      bg.r = 0.12,
      min.segment.length = 0.1,
      box.padding = 0.3,
      point.padding = 0.3,
      force = 1.5,
      segment.color = "grey50",
      segment.size = 0.25
    ) +
    
    #Coloring scale
    scale_fill_viridis(
      name = "Density\n(individuals/m²)",
      option = "plasma",
      limits = c(0, scale_max),
      na.value = NA,
      breaks = scales::pretty_breaks(n = 5)
    ) +
    
    #Sizing scale for sparse L. alba data
    {if(is_sparse) {
      scale_size_continuous(
        name = "Density\n(individuals/m²)",
        range = c(2, 8),
        limits = c(0, scale_max)
      )
    }} +
    
    #Adding coordinate system
    coord_sf(
      xlim = c(-120.1, -116.5), 
      ylim = c(32.5, 34.75), 
      expand = FALSE
    ) +
    
    #Adding scale bar
    annotation_scale(
      location = "br",
      width_hint = 0.25,
      text_cex = 0.7,
      style = "ticks",
      line_width = 0.8
    ) +
    
    #Adding labels
    labs(
      title = bquote(italic(.(species_name))),
      x = "Longitude",
      y = "Latitude"
    ) +
    
    #Setting theme
    theme_minimal(base_size = 11) +
    theme(
      panel.background = element_rect(fill = "aliceblue", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      plot.title = element_text(hjust = 0.5, size = 13, face = "italic"),
      axis.title = element_text(face = "bold", size = 10),
      axis.text = element_text(size = 9)
    ) +
    
    #Merging fill and size legends for L. alba and other species
    {if(is_sparse) {
      guides(fill = guide_legend(), size = guide_legend())
    }}
  

  #Loading inset map
  inset_map <- ggplot() +
    geom_sf(
      data = coastline, 
      fill = "lightgrey", 
      color = "black",
      linewidth = 0.3
    ) +
    geom_sf(
      data = scb_bbox, 
      fill = NA, 
      color = "red", 
      linewidth = 1
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(
        color = "black", 
        linewidth = 0.4, 
        fill = NA
      )
    )
  
#Combining main plot with inset
  final_plot <- cowplot::ggdraw() + 
    draw_plot(main_plot) + 
    draw_plot(inset_map, x = 0.58, y = 0.52, width = 0.20, height = 0.20)
  

#Saving individual plot

  if(!is.null(save_path)) {
    ggsave(
      filename = paste0(gsub(" ", "_", species_name), "_Heatmap.png"), 
      plot = final_plot,
      path = save_path, 
      width = 8, 
      height = 7, 
      dpi = 600,
      bg = "white"
    )
    
#Also saving as PDF
    ggsave(
      filename = paste0(gsub(" ", "_", species_name), "_Heatmap.pdf"), 
      plot = final_plot,
      path = save_path, 
      width = 8, 
      height = 7,
      device = cairo_pdf
    )
  }
  
  return(final_plot)
}

# Creating output folder
save_folder <- "C:/Users/bethm/Documents/Manuscript Work/Figures/Species_Heatmaps"
dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)

#Generating individual heatmaps for each species
species_list <- unique(gorg_data_clean$BenthicReefSpecies)
species_plots <- list()

for(species in species_list) {
  message("Creating heatmap for: ", species)
  plot <- create_species_heatmap(species, gorg_data_clean, global_max = global_max_density, save_path = save_folder)
  if(!is.null(plot)) {
    species_plots[[species]] <- plot
  }
}

#Creating multi-panel figure
species_plots_clean <- species_plots[!sapply(species_plots, is.null)]

if(length(species_plots_clean) > 0) {
  
  #arranging in grid
  combined_plot <- wrap_plots(species_plots_clean, ncol = 3)
  
  #saving combined panel as PNG
  ggsave(
    filename = "All_Species_Heatmaps_Panel.png",
    plot = combined_plot, 
    path = save_folder,
    width = 20, 
    height = ceiling(length(species_plots_clean) / 3) * 7,
    dpi = 600,
    bg = "white"
  )
  
  #Saving combined panel as PDF
  ggsave(
    filename = "All_Species_Heatmaps_Panel.pdf",
    plot = combined_plot, 
    path = save_folder,
    width = 20, 
    height = ceiling(length(species_plots_clean) / 3) * 7,
    device = cairo_pdf
  )}

  
 
