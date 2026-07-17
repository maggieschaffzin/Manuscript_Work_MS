#Project: Making Plots to Conceptualize Data
#By: Maggie Schaffzin
#Created: December 6th, 2025
#Last edited: July 15th, 2026

#Installing and loading required packages
library(ggplot2)
library(dplyr)

#Removing missing depths or densities
plot_data <- Unified_gorgonian_dataset %>%
  filter(!is.na(mean_gorgonian_density),
         !is.na(mean_surveydepth),
         !is.na(BenthicReefSpecies))

#Making a species vs depth plot, faceted by species
p <- ggplot(plot_data, aes(x = mean_surveydepth,
                      y = mean_gorgonian_density)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
  labs(
    
    x = "Mean Survey Depth (m)",
    y = "Mean Gorgonian Density (per 100 m²)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


p

#Just PISCO DATA
PISCO_only <- Unified_gorgonian_dataset %>%
  filter(Source == "PISCO") %>%
  filter(!is.na(mean_gorgonian_density),
         !is.na(mean_surveydepth),
         !is.na(BenthicReefSpecies))

ggplot(PISCO_only, aes(x = mean_surveydepth,
                       y = mean_gorgonian_density)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
  labs(
   ,
    x = "Mean Survey Depth (m)",
    y = "Mean Gorgonian Density (per 100 m²)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

#VRG and PISCO side-by-side
clean_data <- Unified_gorgonian_dataset %>%
  filter(!is.na(BenthicReefSpecies))
ggplot(clean_data,
       aes(x = mean_surveydepth,
           y = mean_gorgonian_density,
           color = Source)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
  facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
  
  labs(
    x = "Mean Survey Depth (m)",
    y = "Mean Gorgonian Density",
    color = "Dataset"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    legend.position = "top"
  )

#PVR Exploration
#Filtering VRG data for PVR sites
VRG_PVR <- Unified_gorgonian_dataset %>%
  filter(Source == "VRG" & grepl("PVR", Site, ignore.case = TRUE)) %>%
  filter(!is.na(mean_surveydepth)) %>%      
  mutate(mean_surveydepth = as.numeric(mean_surveydepth))  

#Plotting Density vs Depth, faceted by species
ggplot(VRG_PVR, aes(x = mean_surveydepth, y = mean_gorgonian_density)) +
  geom_point(alpha = 0.7, size = 2, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkblue", linewidth = 1) +
  facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
  labs(
    title = "VRG Gorgonian Density at PVR Sites by Depth",
    x = "Mean Survey Depth (m)",
    y = "Mean Gorgonian Density"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

#Comparing CCA and Gorgonian density at PVR
library(tidyverse)

#Filtering VRG data for PVR sites
VRG_PVR <- Unified_gorgonian_dataset %>%
  filter(Source == "VRG" & grepl("PVR", Site, ignore.case = TRUE)) %>%
  filter(!is.na(mean_surveydepth)) %>%
  mutate(mean_surveydepth = as.numeric(mean_surveydepth))

#Determining scaling factor for secondary axis
max_density <- max(VRG_PVR$mean_gorgonian_density, na.rm = TRUE)
max_CCA <- max(VRG_PVR$mean_CCA_cover, na.rm = TRUE)
scale_factor <- max_density / max_CCA

#Plotting CCA vs Gorgonian with species-colored lines
ggplot(VRG_PVR, aes(x = mean_surveydepth, y = mean_gorgonian_density, color = BenthicReefSpecies)) +
  
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = mean_CCA_cover * scale_factor), method = "loess", se = FALSE,
              color = "darkgreen", linetype = "dashed") +

  labs(
  
    x = "Mean Survey Depth (m)",
    y = "Mean Gorgonian Density",
    color = "Species"
  ) +
  
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Mean CCA Cover (%)")
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold"),
    axis.title.y.right = element_text(size = 12, face = "bold")  
  )

#Saving CCA vs gorgonian plot as a JPEG
ggsave(
  filename = "VRG_PVR_Gorgonian_CCA.jpeg",  
  plot = last_plot(),                        
  path = "C:/Users/bethm/Documents/Manuscript Work/Datasets",
  width = 8,                                 
  height = 6,                                
  units = "in",
  dpi = 300                               
)  

#Comparing entire dataset GORG density to CCA cover by depth/species
# Filtering out any remaining NAs
Unified_clean <- Unified_gorgonian_dataset %>%
  filter(!is.na(mean_gorgonian_density) & !is.na(mean_CCA_cover) & !is.na(mean_surveydepth))
max_density_all <- max(Unified_clean$mean_gorgonian_density, na.rm = TRUE)
max_CCA_all <- max(Unified_clean$mean_CCA_cover, na.rm = TRUE)
scale_factor_all <- max_density_all / max_CCA_all

#Plotting CCA vs GORG
ggplot(Unified_clean, aes(x = mean_surveydepth, y = mean_gorgonian_density, color = BenthicReefSpecies)) +
  
  geom_point(alpha = 0.5, size = 2, aes(shape = Source)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = mean_CCA_cover * scale_factor_all), method = "loess", se = FALSE,
              color = "darkgreen", linetype = "dashed") +
  
  labs(
    
    x = "Mean Survey Depth (m)",
    y = "Mean Gorgonian Density",
    color = "Species",
    shape = "Source"
  ) +
    scale_y_continuous(
    limits = c(0, 50),
    sec.axis = sec_axis(~ . / scale_factor_all, name = "Mean CCA Cover (%)")
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold"),
    axis.title.y.right = element_text(size = 12, face = "bold")
  )

#Saving GORG CCA entire dataset plot as a JPEG
ggsave(
  filename = "VRG_PISCO_Gorgonian_CCA.jpeg",  
  plot = last_plot(),                          
  path = "C:/Users/bethm/Documents/Manuscript Work/Datasets",        
  width = 10,                                  
  height = 6,                                  
  units = "in",
  dpi = 300                                    
)

#Comparing mean gorgonian density to substrate SD
#Filtering out substrate rows with NAs
gorg_substrate <- Unified_gorgonian_dataset %>%
  filter(!is.na(mean_gorgonian_density) & !is.na(mean_Substrate_SD))

#Plotting GORG vs substrate SD, faceted by species
gorg_substrate_plot<- ggplot(gorg_substrate, aes(x = mean_Substrate_SD, y = mean_gorgonian_density)) +
  
  geom_point(alpha = 0.7, size = 2, aes(color = Source)) +  # color by dataset source
  geom_smooth(method = "loess", se = FALSE, color = "darkblue", linewidth = 1) +
  facet_wrap(~ BenthicReefSpecies, scales = "free_y", shrink = TRUE) +
  
  labs(
  
    x = "Substrate Standard Deviation",
    y = "Mean Gorgonian Density",
    color = "Dataset Source"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

#Saving GORG vs Substrate SD Plot as JPEG
ggsave(
  filename = "Gorgonian_Density_vs_SubstrateSD.jpeg",  
  plot = gorg_substrate_plot,                            
  path = "C:/Users/bethm/Documents/Manuscript Work/Datasets",                     
  width = 10,                                          
  height = 6,                                           
  units = "in",
  dpi = 300                                             
)

#Comparing mean gorgonian density to Relief Index and CCA
#Filter out Relief rows with NAs
gorg_relief <- Unified_gorgonian_dataset %>%
  filter(!is.na(mean_gorgonian_density) & 
           !is.na(mean_Relief_index) & 
           !is.na(mean_CCA_cover))

#Determining scaling factor for secondary axis
max_density <- max(gorg_relief$mean_gorgonian_density, na.rm = TRUE)
max_CCA <- max(gorg_relief$mean_CCA_cover, na.rm = TRUE)
scale_factor <- max_density / max_CCA

#Separating Muricea californica from the rest (just for exploration)
gorg_mc <- gorg_relief %>% filter(BenthicReefSpecies == "Muricea californica")
gorg_other <- gorg_relief %>% filter(BenthicReefSpecies != "Muricea californica")

#Plotting GORG vs Relief Index dataset 
plot_gorg <- function(data, y_max) {
  ggplot(data, aes(x = mean_Relief_index, y = mean_gorgonian_density, color = BenthicReefSpecies)) +
    geom_point(alpha = 0.7, size = 2, aes(shape = Source)) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
    geom_smooth(aes(y = mean_CCA_cover * scale_factor), method = "loess", se = FALSE,
                color = "darkgreen", linetype = "dashed", linewidth = 1) +
    facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
    labs(
      
      x = "Mean Relief Index",
      y = "Mean Gorgonian Density",
      color = "Species",
      shape = "Dataset Source"
    ) +
    scale_y_continuous(
      limits = c(0, y_max),
      sec.axis = sec_axis(~ . / scale_factor, name = "Mean CCA Cover (%)")
    ) +
    theme_bw(base_size = 14) +
    theme(
      strip.background = element_rect(fill = "gray90"),
      strip.text = element_text(face = "bold")
    )
}

#Plotting for just Muricea californica (0-100)
gorg_mc_plot <- plot_gorg(gorg_mc, y_max = 100)

#Plotting for all other species (0-25)
gorg_other_plot <- plot_gorg(gorg_other, y_max = 25)

#Combining plots using patchwork
library(patchwork)
final_plot <- gorg_other_plot / gorg_mc_plot

#Displaying the combined plot (to check)
final_plot

#Saving GORG vs Relief Index as JPEG
ggsave(
  filename = "Gorgonian_Density_vs_ReliefIndex_CCA_customY.jpeg",
  plot = final_plot,
  path = "C:/Users/bethm/Documents/Manuscript Work/Datasets",
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)

#Plotting Substrate SD + Gorg density +CCA cov by species (Adding CCA line--copy and pasted code from above)
#Filtering out rows with NAs
gorg_substrate <- Unified_gorgonian_dataset %>%
  filter(!is.na(mean_gorgonian_density) & 
           !is.na(mean_Substrate_SD) & 
           !is.na(mean_CCA_cover)) %>%
  mutate(y_max = ifelse(BenthicReefSpecies == "Muricea californica", 100, 25))

max_CCA <- max(gorg_substrate$mean_CCA_cover, na.rm = TRUE)
scale_factor <- max(gorg_substrate$y_max) / max_CCA

#Plotting GORG vs Substrate SD with CCA line
gorg_substrate_plot <- ggplot(gorg_substrate, aes(x = mean_Substrate_SD, y = mean_gorgonian_density, color = BenthicReefSpecies)) +
  

  geom_point(alpha = 0.7, size = 2, aes(shape = Source)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = mean_CCA_cover * scale_factor), method = "loess", se = FALSE,
              color = "darkgreen", linetype = "dashed", linewidth = 1) +
  
  facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
  labs(
    
    x = "Substrate Standard Deviation",
    y = "Mean Gorgonian Density",
    color = "Species",
    shape = "Dataset Source"
  ) +
  
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Mean CCA Cover (%)")
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold")
  )

#Displaying plot
gorg_substrate_plot

#Saving GORG vs Substrate SD w/ CCA line as JPEG
ggsave(
  filename = "Gorgonian_Density_vs_SubstrateSD_CCA_facet_freeY.jpeg",
  plot = gorg_substrate_plot,
  path = "C:/Users/bethm/Documents/Manuscript Work/Datasets",
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)

#Loading GLMM model
load("GLMM_scaled_species_models_12_29.RData") 

#Defining all species
all_species <- data.frame(
  Species = c("Eugorgia rubens", "Leptogorgia alba",
              "Leptogorgia chilensis", "Muricea californica",
              "Muricea fruticosa")
)


#Getting p-values for depth
get_pvals <- function(term_name) {
  bind_rows(lapply(names(model_results), function(sp) {
    mod <- model_results[[sp]]
    if(is.null(mod)) return(data.frame(Species=sp, p_label="model failed"))
    
    td <- tryCatch(broom.mixed::tidy(mod, effects="fixed"), error=function(e) NULL)
    if(is.null(td) || !("term" %in% names(td))) return(data.frame(Species=sp, p_label="model error"))
    
    row <- td %>% filter(term == term_name)
    if(nrow(row) == 0) return(data.frame(Species=sp, p_label="n.s."))
    
    pval <- 2 * (1 - pnorm(abs(row$estimate / row$std.error)))
    label <- dplyr::case_when(
      pval < 0.001 ~ "p < 0.001",
      pval < 0.01  ~ paste0("p = ", round(pval,3)),
      pval < 0.05  ~ paste0("p = ", round(pval,3)),
      TRUE         ~ "n.s."
    )
    data.frame(Species=sp, p_label=label)
  }))
}

depth_pvals <- get_pvals("mean_surveydepth")

#Creating full depth range for predictions
depth_range <- seq(
  min(df$mean_surveydepth, na.rm = TRUE),
  max(df$mean_surveydepth, na.rm = TRUE),
  length.out = 100
)

#Generating predictions across full depth range for each species
depth_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) return(NULL)
  
  # All predictors set to mean (0 for scaled)
  numeric_cols <- names(mod@frame)[sapply(mod@frame, is.numeric)]
  pred_df <- data.frame(matrix(0, nrow=length(depth_range), ncol=length(numeric_cols)))
  colnames(pred_df) <- numeric_cols
  pred_df$mean_surveydepth <- scale(depth_range) 
  
  # Factor predictors (Reef_Type) set to reference
  if("Reef_Type" %in% names(mod@frame)) pred_df$Reef_Type <- levels(df$Reef_Type)[1]
  
  # Predict with standard errors
  out <- tryCatch(
    predict(mod, newdata=pred_df, type="response", re.form=NA, se.fit=TRUE, allow.new.levels=TRUE),
    error=function(e) NULL
  )
  if(is.null(out)) return(NULL)
  
  data.frame(
    Species=sp,
    x=depth_range,
    predicted=out$fit,
    conf.low=out$fit - 1.96*out$se.fit,
    conf.high=out$fit + 1.96*out$se.fit
  )
}))

#Merging species labels and p-values

fig3_df <- all_species %>%
  left_join(depth_preds, by="Species") %>%
  left_join(depth_pvals, by="Species") %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )


#Preparing data for plotting
lines_df <- subset(fig3_df, has_pred)

#Labeling "n.s" label for species without predictions
nosig_df <- subset(fig3_df, !has_pred) %>%
  group_by(Species, Species_label) %>%
  summarize(x_label = mean(x, na.rm=TRUE), .groups="drop") %>%
  mutate(y_label = 0, text="Depth not significant")

# P-value labels at top-left corner of each facet
pval_df <- depth_pvals %>%
  left_join(all_species %>% mutate(Species_label = paste0("italic('", Species, "')")), by="Species") %>%
  mutate(x_label = -Inf, y_label = Inf)

#Plotting Figure 3
fig3 <- ggplot() +
  geom_line(data=lines_df, aes(x=x, y=predicted), size=1.3, color="steelblue") +
  geom_ribbon(data=lines_df, aes(x=x, ymin=conf.low, ymax=conf.high),
              alpha=0.25, fill="steelblue") +
  geom_text(data=nosig_df, aes(x=x_label, y=y_label, label=text),
            inherit.aes=FALSE, color="grey40", size=4) +
  geom_text(data=pval_df, aes(x=x_label, y=y_label, label=p_label),
            inherit.aes=FALSE, hjust=-0.1, vjust=1.2, size=4, fontface="bold") +
  facet_wrap(~ Species_label, scales="free_y", labeller=label_parsed) +
  labs(x="Depth (m)", y="Predicted gorgonian density (per 100 m²)") +
  theme_classic(base_size=12) +
  theme(
    strip.text = element_text(face="italic", size=12),
    axis.title = element_text(face="bold"),
    axis.text = element_text(size=11),
    panel.border = element_rect(color="black", fill=NA),
    panel.spacing = unit(1, "lines")
  )
fig3
#Saving figure
ggsave(file.path(fig_dir, "Fig3_Depth_Effects_FullRange_v2.png"), plot=fig3, width=9, height=7, dpi=600)
ggsave(file.path(fig_dir, "Fig3_Depth_Effects_FullRange_v2.pdf"), plot=fig3, width=9, height=7)


#FIGURE 4: Gorgonian predicted density by substrate heterogeneity
#Getting p-values for substrate heterogeneity
get_pvals <- function(term_name) {
  bind_rows(lapply(names(model_results), function(sp) {
    mod <- model_results[[sp]]
    if(is.null(mod)) return(data.frame(Species=sp, p_label="model failed"))
    
    td <- tryCatch(broom.mixed::tidy(mod, effects="fixed"), error=function(e) NULL)
    if(is.null(td) || !("term" %in% names(td))) return(data.frame(Species=sp, p_label="model error"))
    
    row <- td %>% filter(term == term_name)
    if(nrow(row) == 0) return(data.frame(Species=sp, p_label="n.s."))
    
    pval <- 2 * (1 - pnorm(abs(row$estimate / row$std.error)))
    label <- dplyr::case_when(
      pval < 0.001 ~ "p < 0.001",
      pval < 0.01  ~ paste0("p = ", round(pval,3)),
      pval < 0.05  ~ paste0("p = ", round(pval,3)),
      TRUE         ~ "n.s."
    )
    data.frame(Species=sp, p_label=label)
  }))
}

hetero_pvals <- get_pvals("mean_Substrate_SD")

#Creating full SD range for predictions
sd_range <- seq(
  min(df$mean_Substrate_SD, na.rm = TRUE),
  max(df$mean_Substrate_SD, na.rm = TRUE),
  length.out = 100
)

#Generating predictions for each species across full SD range
hetero_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) return(NULL)
  
  #Setting all numeric predictors to mean
  numeric_cols <- names(mod@frame)[sapply(mod@frame, is.numeric)]
  pred_df <- data.frame(matrix(0, nrow=length(sd_range), ncol=length(numeric_cols)))
  colnames(pred_df) <- numeric_cols
  pred_df$mean_Substrate_SD <- scale(sd_range)  
  
  #Factoring predictors (Reef_Type) set to reference
  if("Reef_Type" %in% names(mod@frame)) pred_df$Reef_Type <- levels(df$Reef_Type)[1]
  
  #Predicting with SE
  out <- tryCatch(
    predict(mod, newdata=pred_df, type="response", re.form=NA, se.fit=TRUE, allow.new.levels=TRUE),
    error=function(e) NULL
  )
  if(is.null(out)) return(NULL)
  
  data.frame(
    Species=sp,
    x=sd_range,
    predicted=out$fit,
    conf.low=out$fit - 1.96*out$se.fit,
    conf.high=out$fit + 1.96*out$se.fit
  )
}))

#Merging species labels and p-values
fig4_df <- all_species %>%
  left_join(hetero_preds, by="Species") %>%
  left_join(hetero_pvals, by="Species") %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )

#Preparing separate layers
lines_df <- subset(fig4_df, has_pred)

nosig_df <- subset(fig4_df, !has_pred) %>%
  group_by(Species, Species_label) %>%
  summarize(x_label = mean(x, na.rm=TRUE), .groups="drop") %>%
  mutate(y_label = 0, text="n.s.")

pval_df <- hetero_pvals %>%
  left_join(all_species %>% mutate(Species_label = paste0("italic('", Species, "')")), by="Species") %>%
  mutate(x_label = -Inf, y_label = Inf)

#Plotting Figure 4
fig4 <- ggplot() +
  geom_line(data=lines_df, aes(x=x, y=predicted), size=1.3, color="darkgreen") +
  geom_ribbon(data=lines_df, aes(x=x, ymin=conf.low, ymax=conf.high),
              alpha=0.25, fill="darkgreen") +
  geom_text(data=nosig_df, aes(x=x_label, y=y_label, label=text),
            inherit.aes=FALSE, color="grey40", size=4) +
  geom_text(data=pval_df, aes(x=x_label, y=y_label, label=p_label),
            inherit.aes=FALSE, hjust=-0.1, vjust=1.2, size=4, fontface="bold") +
  facet_wrap(~ Species_label, scales="free_y", labeller=label_parsed) +
  labs(x="Substrate heterogeneity (SD)", y="Predicted gorgonian density (per 100 m²)") +
  theme_classic(base_size=12) +
  theme(
    strip.text = element_text(face="italic", size=12),
    axis.title = element_text(face="bold"),
    axis.text = element_text(size=11),
    panel.border = element_rect(color="black", fill=NA),
    panel.spacing = unit(1, "lines")
  )
fig4

#Saving Figure 4

ggsave(file.path(fig_dir, "Fig4_Heterogeneity_Effects_FullRange.png"), plot=fig4, width=9, height=7, dpi=600)
ggsave(file.path(fig_dir, "Fig4_Heterogeneity_Effects_FullRange.pdf"), plot=fig4, width=9, height=7)

#FIGURE 5: murcal predicted densities vs CCA densities

#Extracting the model for Muricea californica
mc_model <- model_results[["Muricea californica"]]

#Creating prediction grid across full observed CCA range
cca_range <- seq(
  min(df$mean_CCA_cover, na.rm = TRUE),
  max(df$mean_CCA_cover, na.rm = TRUE),
  length.out = 100
)

#Setting other numeric predictors to 0 (scaled) and factors to reference
numeric_cols <- names(mc_model@frame)[sapply(mc_model@frame, is.numeric)]
pred_df <- data.frame(matrix(0, nrow=length(cca_range), ncol=length(numeric_cols)))
colnames(pred_df) <- numeric_cols
pred_df$mean_CCA_cover <- scale(cca_range)  # scaled like in model

if("Reef_Type" %in% names(mc_model@frame)) {
  pred_df$Reef_Type <- levels(df$Reef_Type)[1]
}

#Predicting with SE
mc_pred <- predict(mc_model, newdata=pred_df, type="response", re.form=NA, se.fit=TRUE, allow.new.levels=TRUE)

mc_pred_df <- data.frame(
  CCA_cover = cca_range,
  predicted = mc_pred$fit,
  conf.low = mc_pred$fit - 1.96 * mc_pred$se.fit,
  conf.high = mc_pred$fit + 1.96 * mc_pred$se.fit
)


#Plotting figure 5
fig5 <- ggplot(mc_pred_df, aes(x = CCA_cover, y = predicted)) +
  geom_line(size=1.3, color="darkorange") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.25, fill="darkorange") +
  labs(
    x = "Mean CCA cover (%)",
    y = "Predicted M. californica density (per 100 m²)"
  ) +
  theme_classic(base_size=12) +
  theme(
    axis.title = element_text(face="bold"),
    axis.text = element_text(size=11),
    panel.border = element_rect(color="black", fill=NA)
  )
fig5

#Saving Figure 5
ggsave(file.path(fig_dir, "Fig5_MuriceaCCA_Effect_realone.png"), plot=fig6, width=7, height=5, dpi=600)
ggsave(file.path(fig_dir, "Fig5_MuriceaCCA_Effect.pdf"), plot=fig6, width=7, height=5)

# FIGURE 6: Effects of distance to 200-m bathymetry on gorgonian density
library(dplyr)
library(ggplot2)
library(lme4)
library(cowplot)


#Defining all species
all_species <- data.frame(
  Species = c("Eugorgia rubens", "Leptogorgia alba",
              "Leptogorgia chilensis", "Muricea californica",
              "Muricea fruticosa")
)

#Getting p-values for distance predictor
get_pvals <- function(term_name) {
  bind_rows(lapply(names(model_results), function(sp) {
    mod <- model_results[[sp]]
    if(is.null(mod)) return(data.frame(Species=sp, p_label="model failed"))
    
    td <- tryCatch(broom.mixed::tidy(mod, effects="fixed"), error=function(e) NULL)
    if(is.null(td) || !("term" %in% names(td))) return(data.frame(Species=sp, p_label="model error"))
    
    row <- td %>% filter(term == term_name)
    if(nrow(row) == 0) return(data.frame(Species=sp, p_label="n.s."))
    
    pval <- 2 * (1 - pnorm(abs(row$estimate / row$std.error)))
    label <- dplyr::case_when(
      pval < 0.001 ~ "p < 0.001",
      pval < 0.01  ~ paste0("p = ", round(pval,3)),
      pval < 0.05  ~ paste0("p = ", round(pval,3)),
      TRUE         ~ "n.s."
    )
    data.frame(Species=sp, p_label=label)
  }))
}

dist_pvals <- get_pvals("dist_200m_bath")

#Generating prediction grids for each species
dist_range <- seq(
  min(df$dist_200m_bath, na.rm=TRUE),
  max(df$dist_200m_bath, na.rm=TRUE),
  length.out=100
)

dist_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) return(NULL)
  
  numeric_cols <- names(mod@frame)[sapply(mod@frame, is.numeric)]
  pred_df <- data.frame(matrix(0, nrow=length(dist_range), ncol=length(numeric_cols)))
  colnames(pred_df) <- numeric_cols
  pred_df$dist_200m_bath <- scale(dist_range)  # scale like in model
  
  if("Reef_Type" %in% names(mod@frame)) pred_df$Reef_Type <- levels(df$Reef_Type)[1]
  
  out <- tryCatch(
    predict(mod, newdata=pred_df, type="response", re.form=NA, se.fit=TRUE, allow.new.levels=TRUE),
    error=function(e) NULL
  )
  if(is.null(out)) return(NULL)
  
  data.frame(
    Species=sp,
    x=dist_range,
    predicted=out$fit,
    conf.low=out$fit - 1.96*out$se.fit,
    conf.high=out$fit + 1.96*out$se.fit
  )
}))

#Merging species labels and p-values
fig7_df <- all_species %>%
  left_join(dist_preds, by="Species") %>%
  left_join(dist_pvals, by="Species") %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )


#Separating layers
lines_df <- subset(fig7_df, has_pred)

nosig_df <- subset(fig7_df, !has_pred) %>%
  group_by(Species, Species_label) %>%
  summarize(x_label = mean(x, na.rm=TRUE), .groups="drop") %>%
  mutate(y_label = 0, text="n.s.")

pval_df <- dist_pvals %>%
  left_join(all_species %>% mutate(Species_label=paste0("italic('", Species, "')")), by="Species") %>%
  mutate(x_label=-Inf, y_label=Inf)


#Plotting Figure 6
fig6 <- ggplot() +
  geom_line(data=lines_df, aes(x=x, y=predicted), size=1.3, color="purple") +
  geom_ribbon(data=lines_df, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.25, fill="purple") +
  geom_text(data=nosig_df, aes(x=x_label, y=y_label, label=text),
            inherit.aes=FALSE, color="grey40", size=4) +
  geom_text(data=pval_df, aes(x=x_label, y=y_label, label=p_label),
            inherit.aes=FALSE, hjust=-0.1, vjust=1.2, size=4, fontface="bold") +
  facet_wrap(~Species_label, scales="free_y", labeller=label_parsed) +
  labs(
    x="Distance to 200-m bathymetric contour (m)",
    y="Predicted gorgonian density (per 100 m²)"
  ) +
  theme_classic(base_size=12) +
  theme(
    strip.text = element_text(face="italic", size=12),
    axis.title = element_text(face="bold"),
    axis.text = element_text(size=11),
    panel.border = element_rect(color="black", fill=NA),
    panel.spacing = unit(1, "lines")
  )

fig6

#Saving Figure 6
ggsave(file.path(fig_dir, "Fig6_DistanceBathy_Effects.png"), plot=fig7, width=9, height=7, dpi=600)
ggsave(file.path(fig_dir, "Fig6_DistanceBathy_Effects.pdf"), plot=fig7, width=9, height=7)
