#Project: Rerunning Generalized Linear Mixed Effects Model with all Fixed/Random Effects
#By: Maggie Schaffzin
#Created: October 25th, 2025
#Last edited: January 5th, 2026

library(dplyr)
library(lme4)
library(performance)
library(broom.mixed)
library(readr)
library(stringr)


#Loading dataset
df <- read_csv("Unified_VRG_PISCO_gorgonian_dataset.csv")

#Cleaning columns
df <- df %>%
  mutate(
    Reef_Type = factor(Reef_Type),
    BenthicReefSpecies = str_trim(as.character(BenthicReefSpecies))
  )

#Listing species
species_list <- unique(df$BenthicReefSpecies)

#Scaling numeric predictors
numeric_cols <- c(
  "mean_giantkelp_density_m2", "mean_sst_C", "mean_chl_mg_m3",
  "mean_CCA_cover", "mean_Relief_index", "mean_Relief_SD",
  "mean_Substrate_index", "mean_Substrate_SD",
  "dist_200m_bath", "mean_surveydepth"
)

#Storing results
model_results <- list()
summary_results <- list()

for (sp in species_list) {
  
  cat("\nRunning model for species:", sp, "\n")
  
  df_species <- df %>% filter(BenthicReefSpecies == sp)
  if(nrow(df_species) == 0) next
  
  #Scaling numeric predictors pt2
  df_species <- df_species %>%
    mutate(across(all_of(numeric_cols), ~ scale(.)))
  
  #Fitting LMM
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
  
  #Checking model for singularity
  if(lme4::isSingular(model, tol = 1e-4)) {
    cat("Warning: singular fit for species", sp, "- random effect variance is zero.\n")
  }
  
  #Extracting tidy coefficients
  tidy_df <- broom.mixed::tidy(model) %>%
    mutate(Species = sp,
           t_value = estimate / std.error)
  
  #Extracting R2 values
  r2_val <- performance::r2(model)
  
  #Calculating marginal and conditional R2 values
  tidy_df <- tidy_df %>%
    mutate(
      Marginal_R2 = r2_val$R2_marginal,
      Conditional_R2 = ifelse(r2_val$R2_conditional == 0, NA, r2_val$R2_conditional)
    )
  
  #Storing results
  model_results[[sp]] <- model
  summary_results[[sp]] <- tidy_df
  
  #Printing short summary 
  print(tidy_df[, c("term", "estimate", "std.error", "t_value")])
}

#Combining all species results
all_results <- bind_rows(summary_results)

#Saving CSV and RData for LMM
write_csv(all_results, "GLMM_scaled_results_all_species_1_5_26.csv")
save(model_results, file = "GLMM_scaled_species_models_1_5_26.RData")

#Creating compact R2 summary table
r2_summary <- lapply(model_results, function(m) {
  r <- performance::r2(m)
  data.frame(
    Marginal_R2 = r$R2_marginal,
    Conditional_R2 = ifelse(r$R2_conditional == 0, NA, r$R2_conditional)
  )
})
r2_summary_df <- bind_rows(r2_summary, .id = "Species")

#Saving R2 Table
write_csv(r2_summary_df, "GLMM_species_R2_summary_1_15.csv")
