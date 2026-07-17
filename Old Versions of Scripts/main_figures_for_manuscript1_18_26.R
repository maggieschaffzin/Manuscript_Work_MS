#Project: Main Figures (excluding heatmap) for Manuscript
#By: Maggie Schaffzin
#Created: December 30th, 2025
#Last edited: January 17th, 2026

#Loading packages
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(cowplot)
library(marmap)
library(ggrepel)
library(ggspatial)


#Defining directory to save figures
fig_dir <- "C:/Users/bethm/Documents/Manuscript Work/Figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

#Choosing global theme
theme_set(
  theme_classic(base_size = 13) +
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold")
    )
)

#Creating list of study sites and coordinates (Table S2)
site_coordinates <- Unified_gorgonian_dataset %>%
  group_by(Site, Reef_Type) %>%
  summarise(
    Latitude = mean(mean_Latitude, na.rm = TRUE),
    Longitude = mean(mean_Longitude, na.rm = TRUE),
    depth = mean(mean_surveydepth, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Site)

#Saving file
write_csv(site_coordinates, "VRG_PISCO_site_coordinates.csv")

#Loading California basemap
california <- ne_states(
  country = "United States of America",
  returnclass = "sf"
) %>%
  filter(name == "California")


# Loading NOAA bathymetry (ETOPO1)
bathy <- getNOAA.bathy(
  lon1 = -121.5,
  lon2 = -117,
  lat1 = 32.5,
  lat2 = 34.5,
  resolution = 1
)

#Converting bathymetry to data frame
bathy_df <- fortify.bathy(bathy)


#Extracting 200 m contour line 
bathy_200 <- ggplot_build(
  ggplot(bathy_df, aes(x = x, y = y, z = z)) +
    geom_contour(breaks = -200)
)$data[[1]]

#Identifying the main contour segments 
bathy_200 <- bathy_200 %>%
  group_by(group) %>%
  mutate(segment_length = n()) %>%
  ungroup() %>%
  filter(segment_length > 50) 

#Creating strategic label positions
bathy_200_labels <- bathy_200 %>%
  group_by(group) %>%
  slice(n() %/% 2) %>%
  ungroup() %>%
  select(x, y, group)

#Defining geographic labels
geo_labels <- data.frame(
  location = c("Los Angeles", "Santa Barbara", "San Diego",
               "Santa Cruz\nIsland", "Santa Rosa\nIsland", 
               "San Miguel\nIsland", "Anacapa\nIsland",
               "Santa Barbara\nIsland", "San Nicolas\nIsland",
               "Santa Catalina\nIsland", "San Clemente\nIsland"),
  lon = c(-118.25, -119.70, -117.16,
          -119.85, -120.10, -120.40, -119.40,
          -119.03, -119.48, -118.42, -118.50),
  lat = c(34.05, 34.42, 32.72,
          34.00, 34.00, 34.05, 34.00,
          33.48, 33.25, 33.35, 32.87),
  type = c("city", "city", "city",
           "island", "island", "island", "island",
           "island", "island", "island", "island")
)


#Main figure (study sites + bathymetry)
fig1_main <- ggplot() +
  
  #California land
  geom_sf(
    data = california,
    fill = "grey95",
    color = "grey70",
    linewidth = 0.3
  ) +
  
  #200m contour line
  geom_path(
    data = bathy_200,
    aes(x = x, y = y, group = group),
    color = "steelblue",
    linewidth = 0.5,
    alpha = 0.7
  ) +
  
  #Study sites
  geom_point(
    data = site_coordinates,
    aes(
      x = Longitude,
      y = Latitude,
      shape = Reef_Type,
      color = depth
    ),
    size = 2.5,
    alpha = 0.85
  ) +
  
  
  
  #Adding geographic labels (cities)
  geom_text_repel(
    data = filter(geo_labels, type == "city"),
    aes(x = lon, y = lat, label = location),
    size = 3.5,
    fontface = "bold",
    min.segment.length = 0.2,
    box.padding = 0.5,
    point.padding = 0.5,
    force = 3,
    max.overlaps = 20,
    bg.color = "white",
    bg.r = 0.15,
    segment.color = "grey50",
    segment.size = 0.3
  ) +
  
  #Adding geographic labels (islands)
  geom_text_repel(
    data = filter(geo_labels, type == "island"),
    aes(x = lon, y = lat, label = location),
    size = 2.8,
    fontface = "italic",
    min.segment.length = 0.2,
    box.padding = 0.4,
    point.padding = 0.4,
    force = 2,
    max.overlaps = 20,
    color = "grey30",
    bg.color = "white",
    bg.r = 0.15,
    segment.color = "grey50",
    segment.size = 0.3
  ) +
  
  scale_color_viridis(
    name = "Mean survey\ndepth (m)",
    option = "viridis",
    direction = -1
  ) +
  
  scale_shape_manual(
    name = "Reef type",
    values = c(17, 16),  
    labels = c("Natural", "Artificial")
  ) +
  
  coord_sf(
    xlim = c(-121.5, -117),
    ylim = c(32.5, 34.5),
    expand = FALSE
  ) +
  
  #Adding scale bar
  annotation_scale(
    location = "br", 
    width_hint = 0.25,
    text_cex = 0.8,
    style = "ticks",
    line_width = 1,
    height = unit(0.15, "cm")
  ) +
  
  #Adding north arrow
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    style = north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  guides(
    color = guide_colorbar(order = 1, barwidth = 1, barheight = 8),
    shape = guide_legend(order = 2, override.aes = list(size = 3))
  ) +
  
  theme(
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

#Creating inset map (California state reference)
inset_map <- ggplot() +
  geom_sf(
    data = california,
    fill = "grey90",
    color = "grey70",
    linewidth = 0.3
  ) +
  geom_rect(
    aes(
      xmin = -121.5,
      xmax = -117,
      ymin = 32.5,
      ymax = 34.5
    ),
    fill = NA,
    color = "red",
    linewidth = 1
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(
      color = "black",
      linewidth = 0.5,
      fill = NA
    )
  )

#Combining main figure + inset

fig1 <- ggdraw() +
  draw_plot(fig1_main) +
  draw_plot(
    inset_map,
    x = 0.08,
    y = 0.29,
    width = 0.22,
    height = 0.22
  )

#Displaying figure to check
fig1

#Saving figure as PNG
ggsave(
  filename = file.path(fig_dir, "fig1_improved.png"),
  plot = fig1,
  width = 10,
  height = 7,
  dpi = 600,
  bg = "white"
)

#Saving figure as PDF
ggsave(
  filename = file.path(fig_dir, "fig1_improved.pdf"),
  plot = fig1,
  width = 10,
  height = 7,
  device = cairo_pdf
)

#Figure 3 (figre 2 = heatmaps in different script)
#Loading required packages
library(broom.mixed)

#Loading GLMM model
load("GLMM_scaled_species_models_1_5_26.RData") 

#Extracting and preparing coefficient data
coef_df <- bind_rows(
  lapply(names(model_results), function(sp) {
    broom.mixed::tidy(
      model_results[[sp]],
      effects = "fixed"
    ) %>%
      mutate(Species = sp)
  })
) %>%
  filter(term != "(Intercept)") %>%

#Creating significance indicator based on t-value (used ecological literature-based threshold for t-value significance)
  mutate(significant = abs(statistic) > 1.96)

#Creating readable labels for predictor variables
variable_labels <- c(
  "Reef_TypeNatural Reef" = "Reef type (Natural)",
  "mean_surveydepth" = "Survey depth",
  "mean_Substrate_SD" = "Substrate heterogeneity",
  "mean_Substrate_index" = "Substrate composition",
  "mean_sst_C" = "Sea surface temperature",
  "mean_Relief_SD" = "Relief heterogeneity",
  "mean_Relief_index" = "Reef relief",
  "mean_giantkelp_density_m2" = "Giant kelp density",
  "mean_chl_mg_m3" = "Chlorophyll-a",
  "mean_CCA_cover" = "CCA cover",
  "dist_200m_bath" = "Distance to 200 m contour"
)

#Applying labels
coef_df <- coef_df %>%
  mutate(
    term_label = ifelse(term %in% names(variable_labels), 
                        variable_labels[term], 
                        term),
    #Formating species names properly
    Species_clean = str_replace(Species, "_", " "),  
    Species_clean = paste0(
      str_to_title(str_extract(Species_clean, "^\\w+")),  
      " ",
      str_to_lower(str_extract(Species_clean, "\\w+$"))   
    ),
    Species_label = paste0("italic('", Species_clean, "')")
  )

#Calculating mean absolute effect size for each term
term_order <- coef_df %>%
  group_by(term_label) %>%
  summarise(mean_abs_effect = mean(abs(estimate))) %>%
  arrange(desc(mean_abs_effect)) %>%
  pull(term_label)

coef_df <- coef_df %>%
  mutate(term_label = factor(term_label, levels = rev(term_order)))

#Color scheme for plot
pos_color <- "#0072B2" 
neg_color <- "#D55E00"

fig3 <- ggplot(coef_df, aes(x = estimate, y = term_label, color = estimate > 0)) +
  
#Adding in zero reference line
geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  
#Adding confidence intervals
geom_errorbarh(
    aes(
      xmin = estimate - 1.96 * std.error,
      xmax = estimate + 1.96 * std.error,
      alpha = significant
    ),
    height = 0.2,
    linewidth = 0.6
  ) +
  
  #Creating effect size points
  geom_point(
    aes(shape = significant, alpha = significant), 
    size = 3,
    stroke = 0.8
  ) +
  
  #Adding color scales
  scale_color_manual(
    values = c("TRUE" = pos_color, "FALSE" = neg_color),
    guide = "none"
  ) +
  
  #Filling for significance
  scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1), 
    guide = "none"
  ) +
  
  #Transparency for non-significance
  scale_alpha_manual(
    values = c("TRUE" = 1, "FALSE" = 0.5),
    guide = "none"
  ) +
  
  #Faceting by species
  facet_wrap(
    ~ Species_label, 
    scales = "free_x", 
    labeller = label_parsed,
    ncol = 3  
  ) +
  
  #Adding labels
  labs(
    x = expression(bold("Standardized effect size (β ± 95% CI)")),
    y = NULL
  ) +
  
  #Setting theme
  theme_classic(base_size = 11) +
  theme(
    #Panel styling
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    strip.text = element_text(face = "bold.italic", size = 11),
    
    #Axis styling
    axis.title.x = element_text(face = "bold", size = 11, margin = margin(t = 10)),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    
    #Panel spacing
    panel.spacing = unit(0.8, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    
    #Plot margins
    plot.margin = margin(10, 10, 10, 10)
  )

#Displaying figure to check
print(fig3)

#Saving figure as PNG
ggsave(
  filename = file.path(fig_dir, "Figure3_Coefficient_Plot.png"),
  plot = fig3,
  width = 10,
  height = 7,
  dpi = 600,
  bg = "white"
)

#Saving figure as PDF
ggsave(
  filename = file.path(fig_dir, "Figure3_Coefficient_Plot.pdf"),
  plot = fig3,
  width = 10,
  height = 7,
  device = cairo_pdf
)

#FIGURE 4: Predicted Gorgonian density with depth
# Loading ggeffects package
##if(!require(ggeffects)) install.packages("ggeffects")
library(ggeffects)

#Defining species in model
all_species <- data.frame(
  Species = c("Eugorgia rubens", "Leptogorgia alba",
              "Leptogorgia chilensis", "Muricea californica",
              "Muricea fruticosa"),
  stringsAsFactors = FALSE
)

#Choosing color palette
species_colors <- c(
  "Eugorgia rubens" = "#0072B2",
  "Leptogorgia alba" = "#D55E00",
  "Leptogorgia chilensis" = "#009E73",
  "Muricea californica" = "#F0E442",
  "Muricea fruticosa" = "#CC79A7"
)

#Extracting p-values
get_pvals <- function(term_name) {
  bind_rows(lapply(names(model_results), function(sp) {
    mod <- model_results[[sp]]
    if(is.null(mod)) return(data.frame(Species=sp, p_label="model failed", stringsAsFactors=FALSE))
    
    td <- tryCatch(broom.mixed::tidy(mod, effects="fixed"), error=function(e) NULL)
    if(is.null(td) || !("term" %in% names(td))) {
      return(data.frame(Species=sp, p_label="model error", stringsAsFactors=FALSE))
    }
    
    row <- td %>% filter(term == term_name)
    if(nrow(row) == 0) return(data.frame(Species=sp, p_label="n.s.", stringsAsFactors=FALSE))
    
    t_stat <- row$estimate / row$std.error
    pval <- 2 * (1 - pnorm(abs(t_stat)))
    
    label <- dplyr::case_when(
      pval < 0.001 ~ "p < 0.001",
      pval < 0.01  ~ paste0("p = ", sprintf("%.3f", pval)),
      pval < 0.05  ~ paste0("p = ", sprintf("%.3f", pval)),
      TRUE         ~ "n.s."
    )
    
    data.frame(Species = sp, p_label = label, p_value = pval, t_value = t_stat, stringsAsFactors=FALSE)
  }))
}

#Getting p-values for depth
depth_pvals <- get_pvals("mean_surveydepth")

#Generating predictions across full depth range
depth_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) {
    cat("  Skipping", sp, "- no model\n")
    return(NULL)
  }
  
  pred <- tryCatch({
    #Forcing prediction across specific depth range (4-26m in 0.5m increments)
    ggpredict(mod, 
              terms = "mean_surveydepth [4:26 by=0.5]",
              type = "fixed")
  }, error = function(e) {
    cat("  Error for", sp, ":", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pred)) return(NULL)
  
  data.frame(
    Species = sp,
    depth = pred$x,
    predicted = pred$predicted,
    se = pred$std.error,
    conf.low = pred$conf.low,
    conf.high = pred$conf.high,
    stringsAsFactors = FALSE
  )
}))

if(is.null(depth_preds) || nrow(depth_preds) == 0) {
  stop("ERROR: No depth predictions generated!")
}


#Combining data
fig4_df <- all_species %>%
  left_join(depth_preds, by="Species", relationship="many-to-many") %>%
  left_join(depth_pvals, by="Species") %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )

lines_df <- subset(fig4_df, has_pred)

#Calculating sample sizes
sample_sizes <- df %>%
  group_by(BenthicReefSpecies) %>%
  summarize(n_transects = n(), .groups = "drop") %>%
  rename(Species = BenthicReefSpecies) %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    n_label = paste0("n = ", n_transects)
  )

#Adjusting p-value positions (top-left)
pval_positions <- lines_df %>%
  group_by(Species, Species_label, p_label) %>%
  summarize(
    x_pval = min(depth, na.rm=TRUE) + 1,
    y_pval = max(conf.high, na.rm=TRUE) * 0.98,
    .groups="drop"
  ) %>%
  left_join(sample_sizes, by = c("Species", "Species_label"))

#Adjusting sample size positions (bottom-right)
sample_size_positions <- lines_df %>%
  group_by(Species, Species_label) %>%
  summarize(
    x_n = max(depth, na.rm=TRUE) - 1.5,
    y_n = min(conf.low, na.rm=TRUE) + 
      (max(conf.high, na.rm=TRUE) - min(conf.low, na.rm=TRUE)) * 0.05,
    .groups="drop"
  ) %>%
  left_join(sample_sizes, by = c("Species", "Species_label"))

#Creating figure 4
fig4 <- ggplot() +
  geom_ribbon(data = lines_df,
              aes(x = depth, ymin = conf.low, ymax = conf.high, fill = Species),
              alpha = 0.25) +
  geom_line(data = lines_df,
            aes(x = depth, y = predicted, color = Species),
            linewidth = 1.3,
            lineend = "round") +
  geom_label(data = pval_positions,
             aes(x = x_pval, y = y_pval, label = p_label),
             hjust = 0, vjust = 1, size = 3.2, fontface = "bold",
             color = "black", fill = "white", alpha = 0.85,
             label.padding = unit(0.15, "lines"),
             label.r = unit(0, "lines")) +
  geom_text(data = sample_size_positions,
            aes(x = x_n, y = y_n, label = n_label),
            hjust = 1, vjust = 0, size = 2.8,
            color = "gray30", fontface = "italic") +
  facet_wrap(~ Species_label, scales = "free_y", 
             labeller = label_parsed, ncol = 3) +
  labs(x = "Depth (m)",
       y = expression(bold("Predicted density (colonies 100 m"^-2*")"))) +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  scale_x_continuous(breaks = seq(0, 30, by = 5),
                     minor_breaks = seq(0, 30, by = 2.5),
                     expand = c(0.02, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_classic(base_size = 12) +
  theme(
    strip.text = element_text(face = "italic", size = 11, margin = margin(b = 5)),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    panel.spacing = unit(1.3, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(size = 10, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Previewing figure
print(fig4)

#Saving figure as PNG, PDF, and TIFF
ggsave(file.path(fig_dir, "Figure_4_Depth_Effects.png"),
       plot = fig4, width = 10, height = 7, dpi = 600, bg = "white")

ggsave(file.path(fig_dir, "Figure_4_Depth_Effects.pdf"),
       plot = fig4, width = 10, height = 7, device = cairo_pdf)

ggsave(file.path(fig_dir, "Figure_4_Depth_Effects.tiff"),
       plot = fig4, width = 10, height = 7, dpi = 600, compression = "lzw", bg = "white")

#FIGURE 5: Reef characteristics effects on gorgonian density
#Extracting p-values
get_pvals <- function(term_name) {
  bind_rows(lapply(names(model_results), function(sp) {
    mod <- model_results[[sp]]
    if(is.null(mod)) return(data.frame(Species=sp, p_label="model failed", stringsAsFactors=FALSE))
    
    td <- tryCatch(broom.mixed::tidy(mod, effects="fixed"), error=function(e) NULL)
    if(is.null(td) || !("term" %in% names(td))) {
      return(data.frame(Species=sp, p_label="model error", stringsAsFactors=FALSE))
    }
    
    row <- td %>% filter(term == term_name)
    if(nrow(row) == 0) return(data.frame(Species=sp, p_label="n.s.", stringsAsFactors=FALSE))
    
    t_stat <- row$estimate / row$std.error
    pval <- 2 * (1 - pnorm(abs(t_stat)))
    
    label <- dplyr::case_when(
      pval < 0.001 ~ "p < 0.001",
      pval < 0.01  ~ paste0("p = ", sprintf("%.3f", pval)),
      pval < 0.05  ~ paste0("p = ", sprintf("%.3f", pval)),
      TRUE         ~ "n.s."
    )
    
    data.frame(Species = sp, p_label = label, p_value = pval, t_value = t_stat, stringsAsFactors=FALSE)
  }))
}

# Getting p-values for substrate predictors
hetero_pvals <- get_pvals("mean_Substrate_SD")
relief_pvals <- get_pvals("mean_Relief_index")

#Generating predictions for substrate heterogeneity
substrate_hetero_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) {
    cat("  Skipping", sp, "- no model\n")
    return(NULL)
  }
  
  pred <- tryCatch({
    ggpredict(mod, terms = "mean_Substrate_SD [all]", type = "fixed")
  }, error = function(e) {
    cat("  Error for", sp, "- substrate SD:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pred)) {
    cat("  No predictions for", sp, "substrate SD\n")
    return(NULL)
  }
  
  data.frame(
    Species = sp,
    predictor = "Substrate heterogeneity",
    x_value = pred$x,
    predicted = pred$predicted,
    conf.low = pred$conf.low,
    conf.high = pred$conf.high,
    stringsAsFactors = FALSE
  )
}))

#Generating predictions for reef relief
reef_relief_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) {
    cat("  Skipping", sp, "- no model\n")
    return(NULL)
  }
  
  pred <- tryCatch({
    ggpredict(mod, terms = "mean_Relief_index [all]", type = "fixed")
  }, error = function(e) {
    cat("  Error for", sp, "- relief:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pred)) {
    cat("  No predictions for", sp, "reef relief\n")
    return(NULL)
  }
  
  data.frame(
    Species = sp,
    predictor = "Reef relief",
    x_value = pred$x,
    predicted = pred$predicted,
    conf.low = pred$conf.low,
    conf.high = pred$conf.high,
    stringsAsFactors = FALSE
  )
}))

#Combining both predictors
substrate_preds <- bind_rows(substrate_hetero_preds, reef_relief_preds)

#Combining with species info and p-values
hetero_pvals <- hetero_pvals %>% mutate(predictor = "Substrate heterogeneity")
relief_pvals <- relief_pvals %>% mutate(predictor = "Reef relief")
all_pvals <- bind_rows(hetero_pvals, relief_pvals)

fig5_df <- all_species %>%
  crossing(predictor = c("Substrate heterogeneity", "Reef relief")) %>%
  left_join(substrate_preds, by = c("Species", "predictor"), relationship = "many-to-many") %>%
  left_join(all_pvals, by = c("Species", "predictor")) %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )

lines_df_5 <- subset(fig5_df, has_pred)

#Calculating sample sizes per species
sample_sizes <- df %>%
  group_by(BenthicReefSpecies) %>%
  summarize(n_transects = n(), .groups = "drop") %>%
  rename(Species = BenthicReefSpecies) %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    n_label = paste0("n = ", n_transects)
  )

print(sample_sizes %>% select(Species, n_transects))

#Identifying significant species for ordering
sig_hetero <- hetero_pvals %>% filter(p_value < 0.05) %>% pull(Species)
sig_relief <- relief_pvals %>% filter(p_value < 0.05) %>% pull(Species)

# Reorder species: significant ones first
species_order <- c(
  "Eugorgia rubens",        # sig for substrate
  "Leptogorgia chilensis",  # sig for substrate  
  "Muricea californica",    # sig for substrate
  "Muricea fruticosa",      # sig for relief
  "Leptogorgia alba"        # not sig for either
)

#Updating factor levels
lines_df_5 <- lines_df_5 %>%
  mutate(Species = factor(Species, levels = species_order),
         Species_label = factor(Species_label, levels = paste0("italic('", species_order, "')")))

all_species <- all_species %>%
  mutate(Species = factor(Species, levels = species_order))

sample_sizes <- sample_sizes %>%
  mutate(Species = factor(Species, levels = species_order),
         Species_label = factor(Species_label, levels = paste0("italic('", species_order, "')")))

#Adjusting p-value positions
pval_positions_5 <- lines_df_5 %>%
  group_by(Species, Species_label, predictor, p_label) %>%
  summarize(
    x_pval = -Inf,  
    y_pval = Inf,   
    .groups="drop"
  ) %>%
  left_join(sample_sizes, by = c("Species", "Species_label"))

#Adjusting sample size positions
sample_size_positions_5 <- lines_df_5 %>%
  group_by(Species, Species_label, predictor) %>%
  summarize(
    x_n = Inf,   
    y_n = -Inf, 
    .groups="drop"
  ) %>%
  left_join(sample_sizes, by = c("Species", "Species_label"))

#Panel labels data
panel_labels <- data.frame(
  predictor = c("Substrate heterogeneity", "Reef relief"),
  label = c("(A)", "(B)"),
  Species = factor(species_order[1], levels = species_order),
  Species_label = factor(paste0("italic('", species_order[1], "')"), 
                         levels = paste0("italic('", species_order, "')")),
  x = -Inf,
  y = Inf
)

#Creating figure 5
fig5 <- ggplot() +
  #Adding confidence ribbons
  geom_ribbon(data = lines_df_5,
              aes(x = x_value, ymin = conf.low, ymax = conf.high, fill = Species),
              alpha = 0.25) +
  
  #Adding prediction lines
  geom_line(data = lines_df_5,
            aes(x = x_value, y = predicted, color = Species),
            linewidth = 1.3,
            lineend = "round") +
  
  #Adding p-value labels
  geom_label(data = pval_positions_5,
             aes(x = x_pval, y = y_pval, label = p_label),
             hjust = -0.1, vjust = 1.1,
             size = 3.2, fontface = "bold",
             color = "black", fill = "white", 
             alpha = 0.9,
             label.padding = unit(0.2, "lines"),
             label.r = unit(0.1, "lines")) +
  
  #Adding sample size labels
  geom_label(data = sample_size_positions_5,
             aes(x = x_n, y = y_n, label = n_label),
             hjust = 1.1, vjust = -0.1,
             size = 2.8,
             color = "gray20", fill = "white", 
             alpha = 0.9,
             fontface = "italic",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines")) +
  
  #Faceting by species
  facet_grid(predictor ~ Species_label,
             scales = "free",
             labeller = labeller(Species_label = label_parsed,
                                 predictor = label_value),
             switch = "y") +
  
  labs(x = "Standardized habitat characteristic (z-score)",
       y = expression(bold("Predicted density (colonies 100 m"^-2*")"))) +
  
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
  
  theme_classic(base_size = 12) +
  theme(
    strip.text.x = element_text(face = "italic", size = 8, margin = margin(b = 5)),
    strip.text.y = element_text(face = "bold", size = 10.5, angle = 0, hjust = 0),
    strip.background.x = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    strip.background.y = element_rect(fill = "grey85", color = "black", linewidth = 0.5),
    strip.placement = "outside",
    panel.spacing.x = unit(1.0, "lines"),
    panel.spacing.y = unit(1.0, "lines"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title = element_text(face = "bold", size = 12),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(size = 10, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10),
    plot.background = element_rect(fill = "white", color = NA)
  )

#Displaying figure 5 to check
print(fig5)

#Saving figure 5 as PNG, PDF, and TIFF
ggsave(file.path(fig_dir, "Figure_5_Reef_Characteristics.png"),
       plot = fig5, width = 12, height = 6, dpi = 600, bg = "white")

ggsave(file.path(fig_dir, "Figure_5_Reef_Characteristics.pdf"),
       plot = fig5, width = 12, height = 6, device = cairo_pdf)

ggsave(file.path(fig_dir, "Figure_5_Reef_Characteristics.tiff"),
       plot = fig5, width = 12, height = 6, dpi = 600, compression = "lzw", bg = "white")
