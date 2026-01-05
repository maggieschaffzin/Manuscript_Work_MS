#Project: Rerunning Generalized Linear Mixed Effects Model with all Fixed/Random Effects
#By: Maggie Schaffzin
#Created: October 25th, 2025
#Last edited: December 29th, 2025


library(dplyr)
library(lme4)
library(performance)
library(broom.mixed)
library(readr)

# Load dataset
df <- read_csv("Unified_VRG_PISCO_gorgonian_dataset.csv")

# Clean columns
df <- df %>%
  mutate(
    Reef_Type = factor(Reef_Type),
    BenthicReefSpecies = str_trim(as.character(BenthicReefSpecies))
  )

# List of species
species_list <- unique(df$BenthicReefSpecies)

# Numeric predictors to scale
numeric_cols <- c(
  "mean_giantkelp_density_m2", "mean_sst_C", "mean_chl_mg_m3",
  "mean_CCA_cover", "mean_Relief_index", "mean_Relief_SD",
  "mean_Substrate_index", "mean_Substrate_SD",
  "dist_200m_bath", "mean_surveydepth"
)

# Store results
model_results <- list()
summary_results <- list()

for (sp in species_list) {
  
  cat("\nRunning model for species:", sp, "\n")
  
  df_species <- df %>% filter(BenthicReefSpecies == sp)
  if(nrow(df_species) == 0) next
  
  # Scale numeric predictors
  df_species <- df_species %>%
    mutate(across(all_of(numeric_cols), ~ scale(.)))
  
  # Fit LMM
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
    cat("Model failed for species", sp, ":", e$message, "\n")
    return(NULL)
  })
  if(is.null(model)) next
  
  # Check for singularity
  if(lme4::isSingular(model, tol = 1e-4)) {
    cat("Warning: singular fit for species", sp, "- random effect variance is zero.\n")
  }
  
  # Extract tidy coefficients
  tidy_df <- broom.mixed::tidy(model) %>%
    mutate(Species = sp,
           t_value = estimate / std.error)
  
  # Extract R2 values
  r2_val <- performance::r2(model)
  
  # If random effect variance is zero, Conditional R2 will be NA
  tidy_df <- tidy_df %>%
    mutate(
      Marginal_R2 = r2_val$R2_marginal,
      Conditional_R2 = ifelse(r2_val$R2_conditional == 0, NA, r2_val$R2_conditional)
    )
  
  # Store results
  model_results[[sp]] <- model
  summary_results[[sp]] <- tidy_df
  
  # Optional: print short summary
  print(tidy_df[, c("term", "estimate", "std.error", "t_value")])
}

# Combine all species results
all_results <- bind_rows(summary_results)

# Save CSV and RData
write_csv(all_results, "GLMM_scaled_results_all_species_12_29.csv")
save(model_results, file = "GLMM_scaled_species_models_12_29.RData")

# Optional: create compact R2 summary table
r2_summary <- lapply(model_results, function(m) {
  r <- performance::r2(m)
  data.frame(
    Marginal_R2 = r$R2_marginal,
    Conditional_R2 = ifelse(r$R2_conditional == 0, NA, r$R2_conditional)
  )
})
r2_summary_df <- bind_rows(r2_summary, .id = "Species")
write_csv(r2_summary_df, "GLMM_species_R2_summary_12_29.csv")
