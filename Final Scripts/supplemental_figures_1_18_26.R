# Project: Supplemental Figures for Manuscript
# By: Maggie Schaffzin
# Created: January 15th, 2026
# Last edited: January 18th, 2026

#Loading required packages
library(tidyverse)
library(ggeffects)
library(performance)  
library(corrplot)     

#FIGURE S1: CCA vs Gorg density
#Getting p-values
cca_pvals <- get_pvals("mean_CCA_cover")

#Generating predictions for all species
cca_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) {
    cat("  Skipping", sp, "- no model\n")
    return(NULL)
  }
  
  pred <- tryCatch({
    ggpredict(mod, terms = "mean_CCA_cover [all]", type = "fixed")
  }, error = function(e) {
    cat("  Error for", sp, "- CCA:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pred)) {
    cat("  No predictions for", sp, "CCA\n")
    return(NULL)
  }
  

  data.frame(
    Species = sp,
    cca_cover = pred$x,
    predicted = pred$predicted,
    conf.low = pred$conf.low,
    conf.high = pred$conf.high,
    stringsAsFactors = FALSE
  )
}))

#Combining with p-values
figS1_df <- all_species %>%
  left_join(cca_preds, by = "Species", relationship = "many-to-many") %>%
  left_join(cca_pvals, by = "Species") %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )

lines_df_S1 <- subset(figS1_df, has_pred)

#Adjusting p-value positions
pval_positions_S1 <- lines_df_S1 %>%
  group_by(Species, Species_label, p_label) %>%
  summarize(
    x_pval = -Inf,
    y_pval = Inf,
    .groups = "drop"
  )

#Adjusting sample size positions
sample_size_positions_S1 <- lines_df_S1 %>%
  group_by(Species, Species_label) %>%
  summarize(
    x_n = Inf,
    y_n = -Inf,
    .groups = "drop"
  ) %>%
  left_join(sample_sizes, by = c("Species", "Species_label"))

#Creating figure S1
figS1 <- ggplot() +
  geom_ribbon(data = lines_df_S1,
              aes(x = cca_cover, ymin = conf.low, ymax = conf.high, fill = Species),
              alpha = 0.25) +
  geom_line(data = lines_df_S1,
            aes(x = cca_cover, y = predicted, color = Species),
            linewidth = 1.3,
            lineend = "round") +
  geom_label(data = pval_positions_S1,
             aes(x = x_pval, y = y_pval, label = p_label),
             hjust = -0.1, vjust = 1.1,
             size = 3.2, fontface = "bold",
             color = "black", fill = "white",
             alpha = 0.9,
             label.padding = unit(0.2, "lines"),
             label.r = unit(0.1, "lines")) +
  geom_label(data = sample_size_positions_S1,
             aes(x = x_n, y = y_n, label = n_label),
             hjust = 1.1, vjust = -0.1,
             size = 2.8,
             color = "gray20", fill = "white",
             alpha = 0.9,
             fontface = "italic",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines")) +
  facet_wrap(~ Species_label,
             scales = "free_y",
             labeller = label_parsed,
             ncol = 3) +
  labs(x = "Crustose coralline algae cover (z-score)",
       y = expression(bold("Predicted density (colonies 100 m"^-2*")"))) +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
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
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10)
  )

#Displaying figure S1 to check
print(figS1)

#Saving S1 as a PNG and PDF
ggsave(file.path(fig_dir, "Figure_S1_CCA_Effects.png"),
       plot = figS1, width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Figure_S1_CCA_Effects.pdf"),
       plot = figS1, width = 10, height = 7, device = cairo_pdf)

# FIGURE S2: Distance to 200m bath contour

#Getting p-values
dist_pvals <- get_pvals("dist_200m_bath")

#Generating predictions for all species
dist_preds <- bind_rows(lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) {
    cat("  Skipping", sp, "- no model\n")
    return(NULL)
  }
  
  pred <- tryCatch({
    ggpredict(mod, terms = "dist_200m_bath [all]", type = "fixed")
  }, error = function(e) {
    cat("  Error for", sp, "- distance:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pred)) {
    cat("  No predictions for", sp, "distance\n")
    return(NULL)
  }
  
  data.frame(
    Species = sp,
    distance = pred$x,  # Keep as z-score
    predicted = pred$predicted,
    conf.low = pred$conf.low,
    conf.high = pred$conf.high,
    stringsAsFactors = FALSE
  )
}))

#Combining with p-values
figS2_df <- all_species %>%
  left_join(dist_preds, by = "Species", relationship = "many-to-many") %>%
  left_join(dist_pvals, by = "Species") %>%
  mutate(
    Species_label = paste0("italic('", Species, "')"),
    has_pred = !is.na(predicted)
  )

lines_df_S2 <- subset(figS2_df, has_pred)

#Adjusting p-value positions
pval_positions_S2 <- lines_df_S2 %>%
  group_by(Species, Species_label, p_label) %>%
  summarize(
    x_pval = -Inf,
    y_pval = Inf,
    .groups = "drop"
  )

#Adjusting sample size positions
sample_size_positions_S2 <- lines_df_S2 %>%
  group_by(Species, Species_label) %>%
  summarize(
    x_n = Inf,
    y_n = -Inf,
    .groups = "drop"
  ) %>%
  left_join(sample_sizes, by = c("Species", "Species_label"))

#Creating figure S2
figS2 <- ggplot() +
  geom_ribbon(data = lines_df_S2,
              aes(x = distance, ymin = conf.low, ymax = conf.high, fill = Species),
              alpha = 0.25) +
  geom_line(data = lines_df_S2,
            aes(x = distance, y = predicted, color = Species),
            linewidth = 1.3,
            lineend = "round") +
  geom_label(data = pval_positions_S2,
             aes(x = x_pval, y = y_pval, label = p_label),
             hjust = -0.1, vjust = 1.1,
             size = 3.2, fontface = "bold",
             color = "black", fill = "white",
             alpha = 0.9,
             label.padding = unit(0.2, "lines"),
             label.r = unit(0.1, "lines")) +
  geom_label(data = sample_size_positions_S2,
             aes(x = x_n, y = y_n, label = n_label),
             hjust = 1.1, vjust = -0.1,
             size = 2.8,
             color = "gray20", fill = "white",
             alpha = 0.9,
             fontface = "italic",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines")) +
  facet_wrap(~ Species_label,
             scales = "free_y",
             labeller = label_parsed,
             ncol = 3) +
  labs(x = "Distance to 200 m bathymetric contour (z-score)",
       y = expression(bold("Predicted density (colonies 100 m"^-2*")"))) +
  scale_color_manual(values = species_colors) +
  scale_fill_manual(values = species_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
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
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 10)
  )

#Displaying figure S2 to check
print(figS2)

#Saving figure S2 as a PNG and PDF
ggsave(file.path(fig_dir, "Figure_S2_Distance_Effects.png"),
       plot = figS2, width = 10, height = 7, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Figure_S2_Distance_Effects.pdf"),
       plot = figS2, width = 10, height = 7, device = cairo_pdf)


#FIGURE S3: Model Diagnostics
#Creating diagnostic plots for each species
diagnostic_plots <- lapply(names(model_results), function(sp) {
  mod <- model_results[[sp]]
  if(is.null(mod)) return(NULL)
  
  #Extracting residuals and fitted values
  resid_df <- data.frame(
    fitted = fitted(mod),
    residuals = residuals(mod, type = "pearson"),
    Species = sp
  )
  
  #QQ plot data
  qq_data <- data.frame(
    theoretical = qqnorm(residuals(mod, type = "pearson"), plot.it = FALSE)$x,
    sample = qqnorm(residuals(mod, type = "pearson"), plot.it = FALSE)$y,
    Species = sp
  )
  
  list(resid_df = resid_df, qq_data = qq_data)
})

#Combining data
resid_combined <- bind_rows(lapply(diagnostic_plots, function(x) x$resid_df))
qq_combined <- bind_rows(lapply(diagnostic_plots, function(x) x$qq_data))

#Adding species labels
resid_combined <- resid_combined %>%
  mutate(Species_label = paste0("italic('", Species, "')"))

qq_combined <- qq_combined %>%
  mutate(Species_label = paste0("italic('", Species, "')"))

#Residuals vs Fitted plot
p1 <- ggplot(resid_combined, aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 0.8) +
  facet_wrap(~ Species_label, scales = "free", labeller = label_parsed, ncol = 3) +
  labs(x = "Fitted values",
       y = "Pearson residuals",
       title = "(A) Residuals vs Fitted") +
  theme_classic(base_size = 11) +
  theme(
    strip.text = element_text(face = "italic", size = 10),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    axis.title = element_text(face = "bold", size = 11),
    panel.spacing = unit(0.8, "lines")
  )

#QQ plot
p2 <- ggplot(qq_combined, aes(x = theoretical, y = sample)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", linewidth = 0.5) +
  geom_point(alpha = 0.3, size = 1) +
  facet_wrap(~ Species_label, scales = "free", labeller = label_parsed, ncol = 3) +
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles",
       title = "(B) Q-Q Plot") +
  theme_classic(base_size = 11) +
  theme(
    strip.text = element_text(face = "italic", size = 10),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    axis.title = element_text(face = "bold", size = 11),
    panel.spacing = unit(0.8, "lines")
  )

#Combining plots
figS3 <- cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1))

#Displaying figure S3 to check
print(figS3)

#Saving S3 as PNG and PDF
ggsave(file.path(fig_dir, "Figure_S3_Model_Diagnostics.png"),
       plot = figS3, width = 10, height = 12, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Figure_S3_Model_Diagnostics.pdf"),
       plot = figS3, width = 10, height = 12, device = cairo_pdf)


#FIGURE S4: Predictor Correlation Matrix
#SelectING numeric predictors from dataset
predictor_vars <- df %>%
  select(mean_surveydepth, mean_Substrate_SD, mean_Substrate_index, mean_Relief_index,
         mean_sst_C, mean_chl_mg_m3, mean_CCA_cover, mean_giantkelp_density_m2,
         dist_200m_bath) %>%
  rename(
    Depth = mean_surveydepth,
    `Substrate SD` = mean_Substrate_SD,
    `Substrate Index` = mean_Substrate_index,
    `Reef relief` = mean_Relief_index,
    SST = mean_sst_C,
    `Chl-a` = mean_chl_mg_m3,
    `CCA cover` = mean_CCA_cover,
    `Kelp density` = mean_giantkelp_density_m2,
    `Distance to 200m` = dist_200m_bath
  )

#Calculating correlation matrix
cor_matrix <- cor(predictor_vars, use = "complete.obs")

#Creating correlation plot and saving as PNG
png(file.path(fig_dir, "Figure_S4_Correlation_Matrix.png"),
    width = 8, height = 8, units = "in", res = 600)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Predictor Correlation Matrix",
         mar = c(0, 0, 2, 0))
dev.off()

#Also saving as PDF
pdf(file.path(fig_dir, "Figure_S4_Correlation_Matrix.pdf"),
    width = 8, height = 8)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         col = colorRampPalette(c("#2166AC", "white", "#B2182B"))(200),
         title = "Predictor Correlation Matrix",
         mar = c(0, 0, 2, 0))
dev.off()

# FIGURE S5: Raw Density Distributions
#Preparing density data
density_data <- df %>%
  select(BenthicReefSpecies, mean_gorgonian_density) %>%
  rename(Species = BenthicReefSpecies) %>%
  mutate(Species_label = paste0("italic('", Species, "')"))

#Creating violin/box plot
figS5 <- ggplot(density_data, aes(x = Species_label, y = mean_gorgonian_density)) +
  geom_violin(aes(fill = Species), alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(alpha = 0.1, size = 0.5, width = 0.15) +
  scale_fill_manual(values = species_colors) +
  scale_x_discrete(labels = function(x) parse(text = x)) +
  labs(x = "",
       y = expression(bold("Gorgonian Density (colonies 100 m"^-2*")"))) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

#Displaying figure S5 to check
print(figS5)

#Saving figure S5 as a PNG and PDF
ggsave(file.path(fig_dir, "Figure_S5_Raw_Data_Distributions.png"),
       plot = figS5, width = 8, height = 6, dpi = 600, bg = "white")
ggsave(file.path(fig_dir, "Figure_S5_Raw_Data_Distributions.pdf"),
       plot = figS5, width = 8, height = 6, device = cairo_pdf)
