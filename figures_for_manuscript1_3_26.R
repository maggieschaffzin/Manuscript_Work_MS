#Project: Figures (excluding heatmap) for Manuscript
#By: Maggie Schaffzin
#Created: December 30th, 2025
#Last edited: January 3rd, 2026

#Installing relevant packages
install.packages ("ggeffects")

#Loading packages
library(tidyverse)
library(sf)
library(ggeffects)
library(lme4)
library(broom.mixed)
library(viridis)
library(rnaturalearth)
library(rnaturalearthhires)
library(ggplot2)
library(sf)
library(viridis)
library(cowplot)
library(dplyr)

#Setting theme
theme_set(
  theme_classic(base_size = 13) +
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
)

#Loading california map
california <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "California")

#Plotting figure with southern california study sites
fig1_main <- ggplot() +
  geom_sf(data = coast, fill = "grey95", color = "grey70") +
  geom_point(
    data = dat,
    aes(
      x = mean_Longitude,
      y = mean_Latitude,
      shape = Reef_Type,
      color = mean_surveydepth
    ),
    size = 3.5,
    alpha = 0.85
  ) +
  scale_color_viridis(
    name = "Mean survey depth (m)",
    option = "C"
  ) +
  scale_shape_manual(values = c(16, 17)) +
  coord_sf(
    xlim = c(-121.5, -117),
    ylim = c(32.5, 34.5),
    expand = FALSE
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    shape = "Reef type"
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 13),
    legend.title = element_text(face = "bold")
  )

#Creating inset map of california for reference
inset_map <- ggplot() +
  geom_sf(data = california, fill = "grey90", color = "grey70", alpha = 0.8) +  
  geom_rect(
    aes(xmin = -121.5, xmax = -117, ymin = 32.5, ymax = 34.5),
    fill = NA, color = "red", size = 1
  ) +
  geom_text(
    aes(x = -119, y = 42, label = "California"),  
    size = 2.5, fontface = "bold"
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(color = "black", size = 0.5, fill = NA) 
  )

#Combining main figure and california reference image
fig1 <- ggdraw() +
  draw_plot(fig1_main) +
  draw_plot(inset_map, x = 0.55, y = 0.57, width = 0.15, height = 0.15)

#Saving as PDF 
ggsave(
  filename = "fig1.pdf",
  plot = fig1,
  width = 8,
  height = 6
)

##Plotting FIGURE 2: Model Coefficient Estimates
coef_df <- coef_df %>%
  mutate(Species_label = paste0("italic('", Species, "')"))

fig2 <- ggplot(coef_df,
               aes(x = estimate, y = term, color = estimate > 0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5) +
  geom_errorbarh(
    aes(
      xmin = estimate - 1.96 * std.error,
      xmax = estimate + 1.96 * std.error
    ),
    height = 0.15
  ) +
  scale_color_manual(values = c("red", "blue"), guide = "none") +
  facet_wrap(~ Species_label, scales = "free_x", labeller = label_parsed) +
  labs(
    x = "Standardized effect size (β ± 95% CI)",
    y = NULL
  ) +
  theme_classic(base_size = 12)
fig2

#Checking Directory to save figures
fig_dir <- "C:/Users/bethm/Documents/Manuscript_Figures"
if(!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)


#Saving figure 2
ggsave(filename = file.path(fig_dir, "Fig2_Model_Coefficients.png"),
       plot = fig2, width = 9, height = 7, dpi = 600)

#FIGURE 3: Predicted Gorgonian density with depth

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
# Saving figure
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

#Table 1
# =========================================
# TABLE 1: GLMM Results – All Species in One Table (Significant in Red)
# =========================================
install.packages ("kableExtra")
install.packages ("systemfont")

library (kableExtra)
library(dplyr)
library(readr)
library(knitr)
library(kableExtra)

# ----------------------------
# Load GLMM results
# ----------------------------
all_results <- read_csv("GLMM_scaled_results_all_species_12_29.csv")
r2_summary_df <- read_csv("GLMM_species_R2_summary_12_29.csv")

# ----------------------------
# Format results
# ----------------------------
table1_df <- all_results %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    t_value = round(t_value, 2),
    Marginal_R2 = round(Marginal_R2, 3),
    Conditional_R2 = round(Conditional_R2, 3),
    # Flag significant predictors
    is_significant = abs(t_value) >= 2
  ) %>%
  select(
    Species,
    Predictor = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t-value` = t_value,
    `R²m` = Marginal_R2,
    `R²c` = Conditional_R2,
    is_significant
  )

# ----------------------------
# Create HTML table with significant predictors in red
# ----------------------------
kable(table1_df,
      format = "html",
      caption = "Table 1. Linear mixed-effects model results for all species, showing the influence of environmental and habitat predictors on mean gorgonian density. Significant predictors (|t| ≥ 2) are highlighted in red. Marginal R² (R²m) represents variance explained by fixed effects, and conditional R² (R²c) represents variance explained by both fixed and random effects.",
      col.names = c("Species", "Predictor", "Estimate", "Std. Error", "t-value", "R²m", "R²c", ""),
      escape = FALSE,
      align = "lrrrrrrc") %>%
  # Highlight significant rows in red
  row_spec(which(table1_df$is_significant), color = "red") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)

# ----------------------------
# Save as HTML
# ----------------------------
save_kable(
  kable(table1_df,
        format = "html",
        caption = "Table 1. Linear mixed-effects model results for all species, showing the influence of environmental and habitat predictors on mean gorgonian density. Significant predictors (|t| ≥ 2) are highlighted in red. Marginal R² (R²m) represents variance explained by fixed effects, and conditional R² (R²c) represents variance explained by both fixed and random effects.",
        col.names = c("Species", "Predictor", "Estimate", "Std. Error", "t-value", "R²m", "R²c", ""),
        escape = FALSE,
        align = "lrrrrrrc") %>%
    row_spec(which(table1_df$is_significant), color = "red") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE),
  "Table1_GLMM_AllSpecies_SignificantRed.html"
)

