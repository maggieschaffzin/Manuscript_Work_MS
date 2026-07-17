# Project: Comprehensive GLMM Sensitivity Analysis
# By: Maggie Schaffzin
# Created: January 29th, 2026
# Purpose: Compare GLMM results with/without the SPECIFIC extreme density observation
# NOTE: We exclude only the observation where density = 1740

library(dplyr)
library(lme4)
library(performance)
library(broom.mixed)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)

#Loading dataset
df <- read_csv("Unified_VRG_PISCO_gorgonian_dataset.csv")

#Cleaning columns
df <- df %>%
  mutate(
    Reef_Type = factor(Reef_Type),
    BenthicReefSpecies = str_trim(as.character(BenthicReefSpecies))
  )

cat("\n==============================================\n")
cat("OUTLIER OBSERVATION IDENTIFICATION\n")
cat("==============================================\n\n")

#Finding the extreme Eugorgia rubens observation
eugorgia_data <- df %>%
  filter(BenthicReefSpecies == "Eugorgia rubens") %>%
  arrange(desc(mean_gorgonian_density))

cat("Top 10 Eugorgia rubens observations by density:\n")
print(head(eugorgia_data %>% 
             select(Site, mean_gorgonian_density, mean_Substrate_SD, 
                    dist_200m_bath, mean_CCA_cover, mean_Relief_index, Reef_Type), 10))

#Identifying the specific outlier observation
outlier_obs <- eugorgia_data %>%
  slice_max(mean_gorgonian_density, n = 1)

outlier_site <- outlier_obs$Site
outlier_density <- outlier_obs$mean_gorgonian_density

cat("\n\nIdentified outlier observation:\n")
cat("Site:", outlier_site, "\n")
cat("Density:", outlier_density, "\n")
cat("Species: Eugorgia rubens\n")

#Getting all details about this specific observation
cat("\n\nComplete outlier observation details:\n")
print(outlier_obs)

#Checking how many observations at this site for Eugorgia rubens
site_eugorgia <- df %>%
  filter(Site == outlier_site, BenthicReefSpecies == "Eugorgia rubens")

cat("\n\nAll Eugorgia rubens observations at site", outlier_site, ":\n")
print(site_eugorgia %>% 
        select(Site, mean_gorgonian_density, mean_surveydepth, 
               mean_Substrate_SD, mean_Relief_index))

#Checking all species at this site
all_species_at_site <- df %>%
  filter(Site == outlier_site) %>%
  group_by(BenthicReefSpecies) %>%
  summarise(
    n_obs = n(),
    mean_density = mean(mean_gorgonian_density),
    max_density = max(mean_gorgonian_density)
  )

cat("\n\nAll species observations at site", outlier_site, ":\n")
print(all_species_at_site)

#Creating row identifier for the specific outlier
df <- df %>%
  mutate(row_id = row_number())

outlier_row_id <- df %>%
  filter(BenthicReefSpecies == "Eugorgia rubens",
         mean_gorgonian_density == outlier_density,
         Site == outlier_site) %>%
  pull(row_id)

cat("\n\nOutlier row ID(s):", outlier_row_id, "\n")
cat("(If multiple, we'll exclude all matching observations)\n")

#Creating numeric columns to scale
numeric_cols <- c(
  "mean_giantkelp_density_m2", "mean_sst_C", "mean_chl_mg_m3",
  "mean_CCA_cover", "mean_Relief_index", "mean_Relief_SD",
  "mean_Substrate_index", "mean_Substrate_SD",
  "dist_200m_bath", "mean_surveydepth"
)

#Getting species list
species_list <- unique(df$BenthicReefSpecies)
cat("\nTotal species in dataset:", length(species_list), "\n")
cat("Total observations in dataset:", nrow(df), "\n")
cat("Observations to exclude:", length(outlier_row_id), "\n")

#Running species-specific model 
run_species_model <- function(data, species_name, exclude_rows = NULL, model_name = "") {
  
  cat("\nRunning", model_name, "model for species:", species_name, "\n")
  
  #Filtering to species
  df_species <- data %>% filter(BenthicReefSpecies == species_name)
  
  #Excluding specific rows if specified
  if (!is.null(exclude_rows)) {
    original_n <- nrow(df_species)
    df_species <- df_species %>% filter(!row_id %in% exclude_rows)
    excluded_n <- original_n - nrow(df_species)
    if(excluded_n > 0) {
      cat("  Excluded", excluded_n, "specific observation(s)\n")
    }
  }
  
  if(nrow(df_species) == 0) {
    cat("  No data remaining for species:", species_name, "\n")
    return(NULL)
  }
  
  cat("  N observations:", nrow(df_species), "\n")
  cat("  N sites:", length(unique(df_species$Site)), "\n")
  
  #Scaling numeric predictors
  df_species <- df_species %>%
    mutate(across(all_of(numeric_cols), ~ scale(.)))
  
  #Fitting model
  model <- tryCatch({
    lmer(
      mean_gorgonian_density ~ mean_giantkelp_density_m2 +
        mean_sst_C + mean_chl_mg_m3 + mean_CCA_cover +
        mean_Relief_index + mean_Relief_SD +
        mean_Substrate_index + mean_Substrate_SD +
        dist_200m_bath + mean_surveydepth +
        Reef_Type +
        (1 | Site),
      data = df_species,
      REML = FALSE
    )
  }, error = function(e) {
    cat("  Model failed:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(model)) return(NULL)
  
  #Checking singularity
  if(lme4::isSingular(model, tol = 1e-4)) {
    cat("  Warning: singular fit detected.\n")
  }
  
  #Extracting results
  tidy_df <- broom.mixed::tidy(model) %>%
    mutate(
      Species = species_name,
      t_value = estimate / std.error,
      Model = model_name
    )
  
  #Getting R2
  r2_val <- performance::r2(model)
  
  #Adding R2 to results
  tidy_df <- tidy_df %>%
    mutate(
      Marginal_R2 = r2_val$R2_marginal,
      Conditional_R2 = ifelse(r2_val$R2_conditional == 0, NA, r2_val$R2_conditional)
    )
  
  cat("  Marginal R²:", round(r2_val$R2_marginal, 4), "\n")
  cat("  Conditional R²:", round(r2_val$R2_conditional, 4), "\n")
  
  return(list(
    model = model,
    tidy_results = tidy_df,
    r2 = r2_val,
    n_obs = nrow(df_species),
    n_sites = length(unique(df_species$Site)),
    species = species_name
  ))
}

cat("\n\n==============================================\n")
cat("RUNNING ALL SPECIES MODELS - WITH OUTLIER\n")
cat("==============================================\n")

models_with <- list()
results_with <- list()

for (sp in species_list) {
  result <- run_species_model(df, sp, exclude_rows = NULL, model_name = "WITH")
  if (!is.null(result)) {
    models_with[[sp]] <- result$model
    results_with[[sp]] <- result$tidy_results
  }
}

#Combining all WITH results
all_results_with <- bind_rows(results_with)

#Running all species models (without outlier observation) 
cat("\n\n==============================================\n")
cat("RUNNING ALL SPECIES MODELS - WITHOUT OUTLIER\n")
cat("==============================================\n")
cat("Excluding observation(s) with row_id:", outlier_row_id, "\n")

models_without <- list()
results_without <- list()

for (sp in species_list) {
  result <- run_species_model(df, sp, exclude_rows = outlier_row_id, model_name = "WITHOUT")
  if (!is.null(result)) {
    models_without[[sp]] <- result$model
    results_without[[sp]] <- result$tidy_results
  }
}

#Combining all WITHOUT results
all_results_without <- bind_rows(results_without)

#Running overall model (with outlier observation)
cat("\n\n==============================================\n")
cat("RUNNING OVERALL MODEL - WITH OUTLIER\n")
cat("==============================================\n")

df_overall_with <- df %>%
  mutate(
    BenthicReefSpecies = factor(BenthicReefSpecies),
    across(all_of(numeric_cols), ~ scale(.))
  )

cat("Total observations:", nrow(df_overall_with), "\n")
cat("Total sites:", length(unique(df_overall_with$Site)), "\n")
cat("Total species:", length(unique(df_overall_with$BenthicReefSpecies)), "\n")

overall_model_with <- tryCatch({
  lmer(
    mean_gorgonian_density ~ mean_giantkelp_density_m2 +
      mean_sst_C + mean_chl_mg_m3 + mean_CCA_cover +
      mean_Relief_index + mean_Relief_SD +
      mean_Substrate_index + mean_Substrate_SD +
      dist_200m_bath + mean_surveydepth +
      Reef_Type +
      (1 | BenthicReefSpecies) +
      (1 | Site),
    data = df_overall_with,
    REML = FALSE
  )
}, error = function(e) {
  cat("Overall model failed:", e$message, "\n")
  return(NULL)
})

if(!is.null(overall_model_with)) {
  if(lme4::isSingular(overall_model_with, tol = 1e-4)) {
    cat("Warning: Overall model has singular fit.\n")
  }
  
  tidy_overall_with <- broom.mixed::tidy(overall_model_with) %>%
    mutate(
      t_value = estimate / std.error,
      Model = "WITH"
    )
  
  r2_overall_with <- performance::r2(overall_model_with)
  
  tidy_overall_with <- tidy_overall_with %>%
    mutate(
      Marginal_R2 = r2_overall_with$R2_marginal,
      Conditional_R2 = r2_overall_with$R2_conditional
    )
  
  cat("\nOverall Model (WITH outlier) Summary:\n")
  cat("Marginal R²:", round(r2_overall_with$R2_marginal, 4), "\n")
  cat("Conditional R²:", round(r2_overall_with$R2_conditional, 4), "\n")
  cat("AIC:", round(AIC(overall_model_with), 2), "\n")
}

#Saving all species results 
cat("\n\n==============================================\n")
cat("SAVING SPECIES-SPECIFIC RESULTS\n")
cat("==============================================\n")

write_csv(all_results_with, "GLMM_ALL_SPECIES_WITH_outlier_obs_1_29_26.csv")
write_csv(all_results_without, "GLMM_ALL_SPECIES_WITHOUT_outlier_obs_1_29_26.csv")

#Creating R2 summary for all species
r2_summary_with <- lapply(models_with, function(m) {
  r <- performance::r2(m)
  data.frame(
    Marginal_R2 = r$R2_marginal,
    Conditional_R2 = ifelse(r$R2_conditional == 0, NA, r$R2_conditional),
    Model = "WITH"
  )
})
r2_df_with <- bind_rows(r2_summary_with, .id = "Species")

r2_summary_without <- lapply(models_without, function(m) {
  r <- performance::r2(m)
  data.frame(
    Marginal_R2 = r$R2_marginal,
    Conditional_R2 = ifelse(r$R2_conditional == 0, NA, r$R2_conditional),
    Model = "WITHOUT"
  )
})
r2_df_without <- bind_rows(r2_summary_without, .id = "Species")

#Combining R2 summaries
r2_comparison <- bind_rows(r2_df_with, r2_df_without) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(Marginal_R2, Conditional_R2)
  ) %>%
  mutate(
    Marginal_R2_diff = Marginal_R2_WITHOUT - Marginal_R2_WITH,
    Conditional_R2_diff = Conditional_R2_WITHOUT - Conditional_R2_WITH,
    Marginal_R2_pct_change = (Marginal_R2_diff / Marginal_R2_WITH) * 100
  )

write_csv(r2_comparison, "GLMM_ALL_SPECIES_R2_comparison_1_29_26.csv")

cat("\nR² Comparison Summary:\n")
print(r2_comparison %>% arrange(desc(abs(Marginal_R2_pct_change))))

#Saving overall model results
cat("\n\n==============================================\n")
cat("SAVING OVERALL MODEL RESULTS\n")
cat("==============================================\n")

if(!is.null(overall_model_with)) {
  write_csv(tidy_overall_with, "GLMM_OVERALL_WITH_outlier_obs_1_29_26.csv")
}

if(!is.null(overall_model_without)) {
  write_csv(tidy_overall_without, "GLMM_OVERALL_WITHOUT_outlier_obs_1_29_26.csv")
}

#Comparing overall models
if(!is.null(overall_model_with) && !is.null(overall_model_without)) {
  
  cat("\n\n==============================================\n")
  cat("OVERALL MODEL COMPARISON\n")
  cat("==============================================\n\n")
  
  #Extracting fixed effects
  fixed_with <- tidy_overall_with %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, t_value) %>%
    rename(
      estimate_with = estimate,
      se_with = std.error,
      t_with = t_value
    )
  
  fixed_without <- tidy_overall_without %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, t_value) %>%
    rename(
      estimate_without = estimate,
      se_without = std.error,
      t_without = t_value
    )
  
  #Merging comparisons
  overall_comparison <- fixed_with %>%
    full_join(fixed_without, by = "term") %>%
    mutate(
      estimate_diff = estimate_without - estimate_with,
      estimate_pct_change = (estimate_diff / estimate_with) * 100,
      t_diff = t_without - t_with,
      direction_change = sign(estimate_with) != sign(estimate_without)
    ) %>%
    arrange(desc(abs(estimate_pct_change)))
  
  cat("\nOverall Model Coefficient Changes:\n")
  print(overall_comparison %>%
          select(term, estimate_with, estimate_without, estimate_pct_change, direction_change) %>%
          mutate(across(where(is.numeric), ~round(., 3))))
  
  cat("\n\nOVERALL MODEL FIT COMPARISON:\n")
  cat("----------------------------------------------\n")
  cat("WITH outlier observation:\n")
  cat("  Marginal R²:", round(r2_overall_with$R2_marginal, 4), "\n")
  cat("  Conditional R²:", round(r2_overall_with$R2_conditional, 4), "\n")
  cat("  AIC:", round(AIC(overall_model_with), 2), "\n")
  cat("  BIC:", round(BIC(overall_model_with), 2), "\n\n")
  
  cat("WITHOUT outlier observation:\n")
  cat("  Marginal R²:", round(r2_overall_without$R2_marginal, 4), "\n")
  cat("  Conditional R²:", round(r2_overall_without$R2_conditional, 4), "\n")
  cat("  AIC:", round(AIC(overall_model_without), 2), "\n")
  cat("  BIC:", round(BIC(overall_model_without), 2), "\n\n")
  
  cat("CHANGES:\n")
  cat("  ΔMarginal R²:", 
      round(r2_overall_without$R2_marginal - r2_overall_with$R2_marginal, 4), "\n")
  cat("  ΔConditional R²:", 
      round(r2_overall_without$R2_conditional - r2_overall_with$R2_conditional, 4), "\n")
  cat("  ΔAIC:", round(AIC(overall_model_without) - AIC(overall_model_with), 2), "\n")
  cat("  ΔBIC:", round(BIC(overall_model_without) - BIC(overall_model_with), 2), "\n")
  cat("  Mean absolute % coefficient change:", 
      round(mean(abs(overall_comparison$estimate_pct_change), na.rm = TRUE), 2), "%\n")
  cat("  Coefficients that changed sign:", 
      sum(overall_comparison$direction_change, na.rm = TRUE), "\n")
  
  write_csv(overall_comparison, "GLMM_OVERALL_coefficient_comparison_1_29_26.csv")
  
#Creating overall model visualization
  cat("\n\nCreating overall model visualizations...\n")
  
  # Plot 1: Overall model coefficient comparison
  overall_plot_data <- overall_comparison %>%
    filter(term != "(Intercept)") %>%
    pivot_longer(
      cols = c(estimate_with, estimate_without),
      names_to = "model",
      values_to = "estimate"
    ) %>%
    mutate(
      model = ifelse(model == "estimate_with", "With Outlier", "Without Outlier"),
      term = gsub("mean_", "", term),
      term = gsub("_", " ", term)
    )
  
  p1 <- ggplot(overall_plot_data, aes(x = reorder(term, estimate), y = estimate, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(
      title = "Overall GLMM Coefficient Comparison",
      subtitle = paste0("With vs. Without Outlier Observation (density = ", outlier_density, ")"),
      x = "Predictor Variable",
      y = "Standardized Coefficient Estimate",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  ggsave("GLMM_OVERALL_coefficient_comparison.png", p1, width = 10, height = 8, dpi = 300)
  
  #Plot 2: Percent change
  p2 <- overall_comparison %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = gsub("mean_", "", term),
      term = gsub("_", " ", term),
      significant_change = abs(estimate_pct_change) > 20
    ) %>%
    ggplot(aes(x = reorder(term, abs(estimate_pct_change)), 
               y = estimate_pct_change,
               fill = significant_change)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = "Overall Model: Percent Change in Coefficients",
      subtitle = "After Removing Single Outlier Observation",
      x = "Predictor Variable",
      y = "Percent Change in Coefficient (%)",
      fill = ">20% Change"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = c(-20, 20), linetype = "dotted", alpha = 0.3, color = "red")
  
  ggsave("GLMM_OVERALL_percent_change.png", p2, width = 10, height = 8, dpi = 300)
}

# =============================================================================
# FOCUS ON EUGORGIA RUBENS (MOST AFFECTED)
# =============================================================================

cat("\n\n==============================================\n")
cat("EUGORGIA RUBENS DETAILED COMPARISON\n")
cat("==============================================\n")

if ("Eugorgia rubens" %in% names(models_with) && "Eugorgia rubens" %in% names(models_without)) {
  
  #Getting fixed effects
  fixed_with_er <- results_with[["Eugorgia rubens"]] %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, t_value) %>%
    rename(estimate_with = estimate, se_with = std.error, t_with = t_value)
  
  fixed_without_er <- results_without[["Eugorgia rubens"]] %>%
    filter(effect == "fixed") %>%
    select(term, estimate, std.error, t_value) %>%
    rename(estimate_without = estimate, se_without = std.error, t_without = t_value)
  
  #Comparing with vs without
  eugorgia_comparison <- fixed_with_er %>%
    full_join(fixed_without_er, by = "term") %>%
    mutate(
      estimate_diff = estimate_without - estimate_with,
      estimate_pct_change = (estimate_diff / estimate_with) * 100,
      direction_change = sign(estimate_with) != sign(estimate_without)
    ) %>%
    arrange(desc(abs(estimate_pct_change)))
  
  cat("\nEugorgia rubens coefficient changes:\n")
  print(eugorgia_comparison %>%
          select(term, estimate_with, estimate_without, estimate_pct_change, direction_change) %>%
          mutate(across(where(is.numeric), ~round(., 3))))
  
  #Getting R2 changes
  r2_with_er <- performance::r2(models_with[["Eugorgia rubens"]])
  r2_without_er <- performance::r2(models_without[["Eugorgia rubens"]])
  
  cat("\n\nEugorgia rubens model fit:\n")
  cat("WITH outlier:\n")
  cat("  Marginal R²:", round(r2_with_er$R2_marginal, 4), "\n")
  cat("  Conditional R²:", round(r2_with_er$R2_conditional, 4), "\n\n")
  cat("WITHOUT outlier:\n")
  cat("  Marginal R²:", round(r2_without_er$R2_marginal, 4), "\n")
  cat("  Conditional R²:", round(r2_without_er$R2_conditional, 4), "\n\n")
  cat("CHANGES:\n")
  cat("  ΔMarginal R²:", round(r2_without_er$R2_marginal - r2_with_er$R2_marginal, 4), "\n")
  cat("  Mean |coef change|:", round(mean(abs(eugorgia_comparison$estimate_pct_change), na.rm = TRUE), 2), "%\n")
  cat("  Coefficients that changed sign:", sum(eugorgia_comparison$direction_change, na.rm = TRUE), "\n")
  
  write_csv(eugorgia_comparison, "GLMM_Eugorgia_rubens_comparison_1_29_26.csv")
  
  #Creating visualization for Eugorgia rubens
  p3 <- eugorgia_comparison %>%
    filter(term != "(Intercept)") %>%
    pivot_longer(
      cols = c(estimate_with, estimate_without),
      names_to = "model",
      values_to = "estimate"
    ) %>%
    mutate(
      model = ifelse(model == "estimate_with", "With Outlier", "Without Outlier"),
      term = gsub("mean_", "", term),
      term = gsub("_", " ", term)
    ) %>%
    ggplot(aes(x = reorder(term, estimate), y = estimate, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(
      title = "Eugorgia rubens GLMM Coefficient Comparison",
      subtitle = paste0("Removing observation with density = ", outlier_density),
      x = "Predictor Variable",
      y = "Standardized Coefficient Estimate",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  ggsave("GLMM_Eugorgia_coefficient_comparison.png", p3, width = 10, height = 8, dpi = 300)
}

#Saving all models
cat("\n\nSaving all model objects...\n")

save(
  models_with, models_without,
  overall_model_with, overall_model_without,
  all_results_with, all_results_without,
  r2_comparison, 
  outlier_row_id, outlier_site, outlier_density,
  file = "GLMM_COMPLETE_sensitivity_analysis_1_29_26.RData"
)

#Creating Summary Report
cat("\n\n==============================================\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
cat("==============================================\n\n")

cat("OUTLIER DETAILS:\n")
cat("----------------------------------------------\n")
cat("Site:", outlier_site, "\n")
cat("Species: Eugorgia rubens\n")
cat("Density:", outlier_density, "\n")
cat("Row ID(s) excluded:", paste(outlier_row_id, collapse = ", "), "\n")
cat("Number of observations excluded:", length(outlier_row_id), "\n\n")

cat("FILES SAVED:\n")
cat("----------------------------------------------\n")
cat("Species-specific results:\n")
cat("  - GLMM_ALL_SPECIES_WITH_outlier_obs_1_29_26.csv\n")
cat("  - GLMM_ALL_SPECIES_WITHOUT_outlier_obs_1_29_26.csv\n")
cat("  - GLMM_ALL_SPECIES_R2_comparison_1_29_26.csv\n")
cat("  - GLMM_Eugorgia_rubens_comparison_1_29_26.csv\n\n")

cat("Overall model results:\n")
cat("  - GLMM_OVERALL_WITH_outlier_obs_1_29_26.csv\n")
cat("  - GLMM_OVERALL_WITHOUT_outlier_obs_1_29_26.csv\n")
cat("  - GLMM_OVERALL_coefficient_comparison_1_29_26.csv\n\n")

cat("Visualizations:\n")
cat("  - GLMM_OVERALL_coefficient_comparison.png\n")
cat("  - GLMM_OVERALL_percent_change.png\n")
cat("  - GLMM_Eugorgia_coefficient_comparison.png\n\n")

cat("R objects:\n")
cat("  - GLMM_COMPLETE_sensitivity_analysis_1_29_26.RData\n\n")

cat("----------------------------------------------\n")
cat("Total species analyzed:", length(species_list), "\n")
cat("Species with models WITH outlier:", length(models_with), "\n")
cat("Species with models WITHOUT outlier:", length(models_without), "\n\n")

#Creating interpretation summary
if(!is.null(overall_model_with) && !is.null(overall_model_without)) {
  
  cat("==============================================\n")
  cat("KEY FINDINGS SUMMARY\n")
  cat("==============================================\n\n")
  
  r2_change <- abs(r2_overall_without$R2_marginal - r2_overall_with$R2_marginal)
  mean_coef_change <- mean(abs(overall_comparison$estimate_pct_change), na.rm = TRUE)
  n_sign_changes <- sum(overall_comparison$direction_change, na.rm = TRUE)
  
  cat("OVERALL MODEL IMPACT:\n")
  cat("  R² change:", round(r2_change, 4), "\n")
  cat("  Mean coefficient change:", round(mean_coef_change, 2), "%\n")
  cat("  Coefficients that flipped sign:", n_sign_changes, "\n\n")
  
  cat("SPECIES-SPECIFIC IMPACTS:\n")
  cat("  Species most affected by outlier removal:\n")
  top_affected <- r2_comparison %>%
    arrange(desc(abs(Marginal_R2_pct_change))) %>%
    head(5)
  print(top_affected %>% select(Species, Marginal_R2_WITH, Marginal_R2_WITHOUT, Marginal_R2_pct_change))
  
  cat("\n\nRECOMMENDATION:\n")
  cat("----------------------------------------------\n")
  
  if(r2_change < 0.05 && mean_coef_change < 15 && n_sign_changes == 0) {
    cat("✓ Consider KEEPING the outlier observation in your main analysis\n")
    cat("  - Overall model changes are minimal\n")
    cat("  - Single extreme observation doesn't substantially affect patterns\n")
    cat("  - Document the observation's unique characteristics\n")
    cat("  - Present sensitivity analysis in supplementary materials\n\n")
    cat("  This suggests the outlier represents a genuine but rare ecological\n")
    cat("  condition that doesn't invalidate the broader patterns.\n")
  } else if(r2_change > 0.1 || mean_coef_change > 30 || n_sign_changes > 2) {
    cat("⚠ Consider EXCLUDING the outlier observation from main analysis\n")
    cat("  - Single observation substantially influences results\n")
    cat("  - Present model without outlier as primary analysis\n")
    cat("  - Discuss outlier as exceptional case requiring special attention\n")
    cat("  - Include with-outlier results in supplementary materials\n\n")
    cat("  This suggests the observation is so extreme it may not represent\n")
    cat("  general patterns and could mislead interpretation.\n")
  } else {
    cat("? REPORT BOTH models with equal weight\n")
    cat("  - Changes are moderate and worth noting\n")
    cat("  - Present results from both models\n")
    cat("  - Let readers judge biological significance\n")
    cat("  - Discuss why this observation is ecologically interesting\n\n")
    cat("  This suggests the observation is influential but the broader\n")
    cat("  patterns remain interpretable with or without it.\n")
  }
  
  cat("\n\nFor your thesis and email to Avrey:\n")
  cat("  1. The observation is from site:", outlier_site, "\n")
  cat("  2. It represents a Eugorgia rubens density of", outlier_density, "\n")
  cat("  3. Sensitivity analysis shows [describe impact level]\n")
  cat("  4. Key question for Avrey: What makes this site/depth unique?\n")
  cat("     - Is there something about the SPECIALO zone?\n")
  cat("     - Is this a known monoculture location?\n")
  cat("     - Are there recruitment events or other factors at play?\n\n")
}
