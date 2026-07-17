#Project: Main Figures (excluding heatmap) for Manuscript
#By: Maggie Schaffzin
#Created: December 30th, 2025
#Last edited: July 17th, 2026

#Loading packages
library(tidyverse)
library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggplot2)
library(cowplot)
library(dplyr)
library(ggspatial)

#Setting theme
theme_set(
  theme_classic(base_size = 13) +
    theme(
      legend.position = "left",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
)

#Loading my data
dat <- read_csv("C:/Users/bethm/Documents/Manuscript Work/Datasets/Unified_VRG_PISCO_gorgonian_dataset_with_exposure.csv")

#Computing mean density per site (across all species)
site_density <- dat %>%
  group_by(Site, mean_Longitude, mean_Latitude) %>%
  summarise(mean_density = mean(mean_gorgonian_density, na.rm = TRUE), .groups = "drop")

#Excluding the single outlier (ANACAPA_WEST_ISLE_W, density = 204)
site_density <- site_density %>%
  filter(Site != "ANACAPA_WEST_ISLE_W")

cat("Sites after removing outlier:", nrow(site_density), "\n")
cat("Density range after exclusion:", range(site_density$mean_density, na.rm = TRUE), "\n")

#Binning based on actual data spread (quartile-informed)
density_breaks <- c(0, 5, 20, 50, 90)
density_labels <- c("0-5", "5-20", "20-50", "50-90")

site_density <- site_density %>%
  mutate(density_bin = cut(mean_density,
                           breaks = density_breaks,
                           labels = density_labels,
                           include.lowest = TRUE))

cat("Bin counts:\n")
print(table(site_density$density_bin, useNA = "always"))

#Defining legend title once so both scales are truly identical and merge into one legend
legend_title <- expression(atop("Mean gorgonian density", "(ind./100m"^-2*")"))

#Loading California land for background fill
california <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "California")

#Creating detailed coast object for Southern California
coast <- ne_coastline(scale = "large", returnclass = "sf") %>%
  st_crop(xmin = -121.5, xmax = -117, ymin = 32.5, ymax = 34.5)

#Plotting figure with southern california study sites
fig1_main <- ggplot() +
  #Land fill (grey)
  geom_sf(data = california, fill = "grey80", color = NA) +
  #Coastline
  geom_sf(data = coast, fill = NA, color = "black", linewidth = 0.4) +
  #Points: fill AND size by mean density bin, with black outline
  geom_point(
    data = site_density,
    aes(
      x = mean_Longitude,
      y = mean_Latitude,
      fill = density_bin,
      size = density_bin
    ),
    shape = 21,
    color = "black",
    stroke = 0.4,
    alpha = 0.9
  ) +
  #Viridis-style fill palette
  scale_fill_manual(
    name = legend_title,
    values = c(
      "0-5"   = "#440154",  # dark purple
      "5-20"  = "#31688e",  # blue
      "20-50" = "#35b779",  # green
      "50-90" = "#fde725"   # yellow
    ),
    
  ) +
  #Size scale - same name as fill scale to merge legends
  scale_size_manual(
    name = legend_title,
    values = c(
      "0-5"   = 1.5,
      "5-20"  = 2.0,
      "20-50" = 3.0,
      "50-90" = 4.0
    ),
   
  ) +
  coord_sf(
    xlim = c(-121.5, -117),
    ylim = c(32.5, 34.5),
    expand = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    pad_x = unit(0.2, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  ) +
  scale_x_continuous(
    labels = function(x) paste0(abs(x), "\u00b0E")
  ) +
  scale_y_continuous(
    labels = function(y) paste0(y, "\u00b0N")
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    legend.position = c(0.12, 0.25),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.margin = margin(4, 6, 4, 6),
    axis.text = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  ) +
  #Creating region labels
  annotate("text", x = -119.9, y = 34.30, label = "Santa Barbara",
           size = 3, fontface = "plain") +
  annotate("text", x = -120.55, y = 34.15, label = "San Miguel",
           size = 3, fontface = "plain") +
  annotate("text", x = -120.15, y = 33.75, label = "Santa Rosa",
           size = 3, fontface = "plain") +
  annotate("text", x = -119.75, y = 34.15, label = "Santa Cruz",
           size = 3, fontface = "plain") +
  annotate("text", x = -119.35, y = 33.92, label = "Anacapa",
           size = 3, fontface = "plain") +
  annotate("text", x = -118.75, y = 34.1, label = "Malibu",
           size = 3, fontface = "plain") +
  annotate("text", x = -118.16, y = 33.97, label = "Santa Monica Bay",
           size = 3, fontface = "plain") +
  annotate("text", x = -118.15, y = 33.85, label = "Palos Verdes",
           size = 3, fontface = "plain") +
  annotate("text", x = -117.45, y = 33.58, label = "Newport Beach",
           size = 3, fontface = "plain") +
  annotate("text", x = -119, y = 33.32, label = "Santa Barbara\nIsland",
           size = 3, fontface = "plain") +
  annotate("text", x = -119.6, y = 33.18, label = "San Nicolas",
           size = 3, fontface = "plain") +
  annotate("text", x = -118.35, y = 33.20, label = "Santa Catalina",
           size = 3, fontface = "plain") +
  annotate("text", x = -118.3, y = 32.68, label = "San Clemente",
           size = 3, fontface = "plain") +
  annotate("text", x = -117.55, y = 32.68, label = "Point Loma",
           size = 3, fontface = "plain")

#Saving FIG 1 as PDF
ggsave(
  filename = "fig1_gorgonian_density.pdf",
  plot = fig1_main,
  width = 9,
  height = 7
)

#Also saving as high-res PNG for quick viewing
ggsave(
  filename = "fig1_gorgonian_density.png",
  plot = fig1_main,
  width = 9,
  height = 7,
  dpi = 300
)

####FIGURE 2: Creating Species-specific density maps
#Checking species in dataset
cat("Species in dataset:\n")
print(sort(unique(dat$BenthicReefSpecies)))

#Loading California land for background fill
california <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "California")

#Creating detailed coast object for Southern California
coast <- ne_coastline(scale = "large", returnclass = "sf") %>%
  st_crop(xmin = -121.5, xmax = -117, ymin = 32.5, ymax = 34.5)

#Creating region label annotations (reused across all maps)
region_labels <- list(
  annotate("text", x = -119.9,  y = 34.30, label = "Santa Barbara",        size = 2.5, fontface = "plain"),
  annotate("text", x = -120.55, y = 34.15, label = "San Miguel",            size = 2.5, fontface = "plain"),
  annotate("text", x = -120.15, y = 33.75, label = "Santa Rosa",            size = 2.5, fontface = "plain"),
  annotate("text", x = -119.75, y = 34.15, label = "Santa Cruz",            size = 2.5, fontface = "plain"),
  annotate("text", x = -119.35, y = 33.92, label = "Anacapa",               size = 2.5, fontface = "plain"),
  annotate("text", x = -118.75, y = 34.10, label = "Malibu",                size = 2.5, fontface = "plain"),
  annotate("text", x = -118.16, y = 33.97, label = "Santa Monica Bay",      size = 2.5, fontface = "plain"),
  annotate("text", x = -118.15, y = 33.85, label = "Palos Verdes",          size = 2.5, fontface = "plain"),
  annotate("text", x = -117.45, y = 33.58, label = "Newport Beach",         size = 2.5, fontface = "plain"),
  annotate("text", x = -119.00, y = 33.32, label = "Santa Barbara\nIsland", size = 2.5, fontface = "plain"),
  annotate("text", x = -119.60, y = 33.18, label = "San Nicolas",           size = 2.5, fontface = "plain"),
  annotate("text", x = -118.35, y = 33.20, label = "Santa Catalina",        size = 2.5, fontface = "plain"),
  annotate("text", x = -118.30, y = 32.68, label = "San Clemente",          size = 2.5, fontface = "plain"),
  annotate("text", x = -117.55, y = 32.68, label = "Point Loma",            size = 2.5, fontface = "plain")
)

#Making one species map with continuous color scale
make_species_map <- function(species_name, dat, california, coast, region_labels) {
  
  #Filtering and summarising for this species
  sp_density <- dat %>%
    filter(BenthicReefSpecies == species_name) %>%
    group_by(Site, mean_Longitude, mean_Latitude) %>%
    summarise(mean_density = mean(mean_gorgonian_density, na.rm = TRUE),
              .groups = "drop") %>%
    filter(!is.na(mean_density))
  
  #Excluding only the specific Eugorgia rubens Anacapa outlier (density > 200)
  if (species_name == "Eugorgia rubens") {
    outliers <- sp_density %>% filter(mean_density > 200)
    if (nrow(outliers) > 0) {
      cat("  Excluding", nrow(outliers), "Eugorgia rubens outlier site(s):",
          paste(outliers$Site, collapse = ", "), "\n")
      sp_density <- sp_density %>% filter(mean_density <= 200)
    }
  }
  
  cat("\nSpecies:", species_name, "| Sites:", nrow(sp_density),
      "| Density range:", round(range(sp_density$mean_density), 2), "\n")
  
  #Building plot with continuous color scale
  p <- ggplot() +
    geom_sf(data = california, fill = "grey80", color = NA) +
    geom_sf(data = coast, fill = NA, color = "black", linewidth = 0.4) +
    geom_point(
      data = sp_density,
      aes(x = mean_Longitude, y = mean_Latitude,
          color = mean_density),
      size  = 2.0,
      alpha = 0.9
    ) +
    scale_color_viridis_c(
      name     = expression("ind. 100m"^-2),
      option   = "plasma",
      limits   = c(0, max(sp_density$mean_density, na.rm = TRUE)),
      trans    = "sqrt",
      na.value = "grey70"
    ) +
    coord_sf(
      xlim = c(-121.5, -117),
      ylim = c(32.5, 34.5),
      expand = FALSE
    ) +
    labs(
      x     = NULL,
      y     = NULL,
      title = bquote(italic(.(species_name)))
    ) +
    scale_x_continuous(labels = function(x) paste0(abs(x), "\u00b0E")) +
    scale_y_continuous(labels = function(y) paste0(y, "\u00b0N")) +
    theme_classic(base_size = 11) +
    theme(
      plot.title       = element_text(face = "italic", size = 11, hjust = 0.5),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9),
      legend.position  = c(0.12, 0.22),
      legend.background = element_rect(fill = "white", color = "black",
                                       linewidth = 0.3),
      legend.margin    = margin(3, 5, 3, 5),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.3, "cm"),
      axis.text        = element_text(size = 8, color = "black"),
      panel.background = element_rect(fill = "white"),
      panel.border     = element_rect(color = "black", fill = NA,
                                      linewidth = 0.5)
    )
  
  #Adding region labels
  for (lbl in region_labels) {
    p <- p + lbl
  }
  
  return(p)
}

#Computing global max density across all species (excluding Eugorgia outlier)
global_max <- dat %>%
  filter(!(BenthicReefSpecies == "Eugorgia rubens" &
             Site %in% c("ANACAPA_WEST_ISLE_W", "ANACAPA_WEST_ISLE_CEN"))) %>%
  group_by(Site, BenthicReefSpecies) %>%
  summarise(mean_density = mean(mean_gorgonian_density, na.rm = TRUE),
            .groups = "drop") %>%
  pull(mean_density) %>%
  max(na.rm = TRUE)

cat("Global max density (excluding outlier):", round(global_max, 2), "\n")

#Getting species list
species_list <- sort(unique(dat$BenthicReefSpecies))
cat("\nGenerating maps for", length(species_list), "species\n")

#Generating and saving one map per species
for (sp in species_list) {
  p        <- make_species_map(sp, dat, california, coast, region_labels)
  sp_clean <- gsub(" ", "_", sp)
  
  ggsave(paste0("fig2_", sp_clean, "_density.pdf"), plot = p, width = 9, height = 7)
  ggsave(paste0("fig2_", sp_clean, "_density.png"), plot = p, width = 9, height = 7, dpi = 300)
  cat("Saved map for", sp, "\n")
}

#Combining multi-panel figure
all_plots <- lapply(species_list, function(sp) {
  make_species_map(sp, dat, california, coast, region_labels)
})

combined <- plot_grid(plotlist = all_plots, ncol = 2, labels = "AUTO",
                      label_size = 12)

ggsave("fig2_all_species_density.png", combined,
       width = 18, height = 7 * ceiling(length(species_list) / 2),
       dpi = 300, bg = "white")
ggsave("fig2_all_species_density.pdf", combined,
       width = 18, height = 7 * ceiling(length(species_list) / 2))


###FIGURE 3:STANDARD EFFECT SIZES
#Load results if not already in environment
all_results <- read_csv("C:/Users/bethm/Documents/Manuscript Work/Datasets/GLMM_exposure_results_all_species.csv")

#Creating coef_df from model results
coef_df <- all_results %>%
  filter(effect == "fixed") %>%
  filter(term != "(Intercept)") %>%
  select(Species, term, estimate, std.error)

#Cleaning up term names for publication-ready y-axis labels
term_labels <- c(
  "mean_giantkelp_density_m2" = "KELP",
  "mean_sst_C"                = "SST",
  "mean_chl_mg_m3"            = "CHL",
  "mean_CCA_cover"            = "CCA",
  "mean_Relief_index"         = "RI",
  "mean_Relief_SD"            = "RISD",
  "mean_Substrate_index"      = "SI",
  "mean_Substrate_SD"         = "SISD",
  "dist_200m_bath"            = "DIST",
  "mean_surveydepth"          = "DEPTH",
  "Reef_TypeNatural Reef"          = "RT",
  "exposure_index_scaled"     = "WEI"
)


#Applying clean labels, italic species names, and significance flag
coef_df <- all_results %>%
  filter(is.na(effect) | effect == "fixed") %>%
  filter(term != "(Intercept)") %>%
  mutate(term = stringr::str_squish(term)) %>%
  filter(term %in% names(term_labels)) %>%
  mutate(
    term_label    = factor(unname(term_labels[term]), levels = rev(unname(term_labels))),
    Species_label = paste0("italic('", Species, "')"),
    t_value       = estimate / std.error,
    significant   = abs(t_value) > 1.96,
    direction     = estimate > 0
  )

#Building figure (legend below for myself)
#shape 21 = circle with separate fill and color (border)
#significant + positive  = filled blue
#significant + negative  = filled red
#non-significant         = white fill, colored border
fig3 <- ggplot(coef_df,
               aes(x = estimate, y = term_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_errorbarh(
    aes(
      xmin  = estimate - 1.96 * std.error,
      xmax  = estimate + 1.96 * std.error,
      color = interaction(direction, significant)
    ),
    height    = 0.2,
    linewidth = 0.5
  ) +
  geom_point(
    aes(
      fill  = interaction(direction, significant),
      color = interaction(direction, significant)
    ),
    shape  = 21,
    size   = 2.5,
    stroke = 0.6
  ) +
  scale_fill_manual(
    values = c(
      "FALSE.FALSE" = "white",    #negative, non-significant: open
      "TRUE.FALSE"  = "white",    #positive, non-significant: open
      "FALSE.TRUE"  = "#d73027",  #negative, significant: filled red
      "TRUE.TRUE"   = "#4575b4"   #positive, significant: filled blue
    ),
    guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "FALSE.FALSE" = "#d73027",  #negative, non-significant: red border
      "TRUE.FALSE"  = "#4575b4",  #positive, non-significant: blue border
      "FALSE.TRUE"  = "#d73027",  #negative, significant: red
      "TRUE.TRUE"   = "#4575b4"   #positive, significant: blue
    ),
    guide = "none"
  ) +
  facet_wrap(~ Species_label, scales = "free_x", labeller = label_parsed) +
  labs(
    x       = "Standardized coefficient estimate (\u03b2 \u00b1 95% CI)",
    y       = NULL,
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.text         = element_text(face = "italic", size = 11),
    strip.background   = element_blank(),
    axis.text.y        = element_text(size = 10, color = "black"),
    axis.text.x        = element_text(size = 10, color = "black"),
    axis.title.x       = element_text(size = 11, margin = margin(t = 8)),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    panel.spacing      = unit(1, "lines"),
    plot.margin        = margin(10, 15, 10, 10),
    plot.caption       = element_text(size = 9, color = "grey40", hjust = 0)
  )

fig3

#Saving figure 3
ggsave(
  filename = "Fig3_Model_Coefficients.png",
  plot     = fig3,
  width    = 10,
  height   = 8,
  dpi      = 600
)

ggsave(
  filename = "Fig3_Model_Coefficients.pdf",
  plot     = fig3,
  width    = 10,
  height   = 8
)

#Checking Directory to save figures
fig_dir <- "C:/Users/bethm/Documents/Manuscript_Figures"
if(!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)


#Saving figure 3 in PNG form
ggsave(filename = file.path(fig_dir, "Fig2_Model_Coefficients.png"),
       plot = fig3, width = 9, height = 7, dpi = 600)


