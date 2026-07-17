# Project: GLMM with Wave Exposure Index as Fixed Effect
# By: Maggie Schaffzin
# Last edited: July 17th, 2026

library(dplyr)
library(lme4)
library(performance)
library(broom.mixed)
library(readr)
library(stringr)

safe_conditional_r2 <- function(r2_obj) {
  val <- r2_obj$R2_conditional
  if (is.null(val) || length(val) == 0) return(NA_real_)
  val <- suppressWarnings(as.numeric(val))
  if (is.na(val) || val == 0) return(NA_real_)
  return(val)
}

safe_r2 <- function(model) {
  r <- tryCatch(
    suppressWarnings(suppressMessages(performance::r2(model))),
    error = function(e) return(list(R2_marginal = NA_real_, R2_conditional = NA_real_))
  )
  if (is.null(r$R2_marginal) && !is.null(r$R2)) {
    r$R2_marginal    <- r$R2
    r$R2_conditional <- NA_real_
  }
  if (is.null(r$R2_marginal))    r$R2_marginal    <- NA_real_
  if (is.null(r$R2_conditional)) r$R2_conditional <- NA_real_
  return(r)
}

#Loading and preparing data
df <- read_csv(
  "C:/Users/bethm/Documents/Manuscript Work/Datasets/Unified_VRG_PISCO_gorgonian_dataset_with_exposure.csv",
  show_col_types = FALSE
)

df <- df %>%
  mutate(
    Reef_Type          = factor(Reef_Type),
    BenthicReefSpecies = str_trim(as.character(BenthicReefSpecies))
  )

#Checking join coverage
n_matched <- sum(!is.na(df$exposure_index_scaled))
cat("Rows with exposure index:", n_matched, "of", nrow(df), "\n")

#Listing numeric predictors to scale (exposure index is already scaled)
numeric_cols <- c(
  "mean_giantkelp_density_m2", "mean_sst_C", "mean_chl_mg_m3",
  "mean_CCA_cover", "mean_Relief_index", "mean_Relief_SD",
  "mean_Substrate_index", "mean_Substrate_SD",
  "dist_200m_bath", "mean_surveydepth"
)

#Checking for NAs
cat("\nNA counts per predictor column:\n")
print(colSums(is.na(df[, c(numeric_cols, "mean_gorgonian_density",
                           "exposure_index_scaled", "Reef_Type", "Site")])))

#Scaling all numeric predictors ONCE on full dataset
df <- df %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(scale(.))))

#Running per-species model
species_list <- unique(df$BenthicReefSpecies)
species_list <- species_list[!is.na(species_list)]

model_results   <- list()
summary_results <- list()

for (sp in species_list) {
  
  cat("\n========================================\n")
  cat("Species:", sp, "\n")
  cat("========================================\n")
  
  df_sp <- df %>%
    filter(BenthicReefSpecies == sp) %>%
    filter(if_all(all_of(c(numeric_cols,
                           "mean_gorgonian_density",
                           "exposure_index_scaled",
                           "Reef_Type", "Site")),
                  ~ !is.na(.)))
  
  if (nrow(df_sp) < 10) {
    cat("  Skipping — too few rows after NA removal (", nrow(df_sp), ")\n")
    next
  }
  
  cat("  Rows after NA removal:", nrow(df_sp), "\n")
  
  #Fitting global LMM
  model <- tryCatch({
    suppressWarnings(
      lmer(
        mean_gorgonian_density ~
          mean_giantkelp_density_m2 +
          mean_sst_C +
          mean_chl_mg_m3 +
          mean_CCA_cover +
          mean_Relief_index +
          mean_Relief_SD +
          mean_Substrate_index +
          mean_Substrate_SD +
          dist_200m_bath +
          mean_surveydepth +
          Reef_Type +
          exposure_index_scaled +
          (1 | Site),
        data = df_sp,
        REML = FALSE
      )
    )
  }, error = function(e) {
    cat("  LMM failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(model)) next
  
  #Falling back to LM if singular fit
  use_lm <- lme4::isSingular(model, tol = 1e-4)
  
  if (use_lm) {
    cat("  Note: singular fit (Site variance ~ 0) — refitting as LM.\n")
    model <- tryCatch({
      lm(
        mean_gorgonian_density ~
          mean_giantkelp_density_m2 +
          mean_sst_C +
          mean_chl_mg_m3 +
          mean_CCA_cover +
          mean_Relief_index +
          mean_Relief_SD +
          mean_Substrate_index +
          mean_Substrate_SD +
          dist_200m_bath +
          mean_surveydepth +
          Reef_Type +
          exposure_index_scaled,
        data = df_sp
      )
    }, error = function(e) {
      cat("  LM fallback failed:", e$message, "\n")
      return(NULL)
    })
    if (is.null(model)) next
  } else {
    cat("  LMM fit OK (Site variance > 0).\n")
  }
  
  #Extracting results
  r2_val  <- safe_r2(model)
  marg_r2 <- suppressWarnings(as.numeric(r2_val$R2_marginal))
  cond_r2 <- safe_conditional_r2(r2_val)
  mtype   <- if (use_lm) "LM" else if (
    inherits(model, "lmerMod") && lme4::isSingular(model, tol = 1e-4)
  ) "LMM-singular" else "LMM"
  
  tidy_df <- broom.mixed::tidy(model) %>%
    mutate(
      Species        = sp,
      t_value        = estimate / std.error,
      Model_type     = mtype,
      Marginal_R2    = marg_r2,
      Conditional_R2 = cond_r2
    )
  
  model_results[[sp]]   <- model
  summary_results[[sp]] <- tidy_df
  
  cat("\n  --- Model coefficients ---\n")
  print(tidy_df[, c("term", "estimate", "std.error", "t_value")])
  cat("\n  Model type:    ", mtype)
  cat("\n  Marginal R²:   ", round(marg_r2, 4))
  if (!is.na(cond_r2)) cat("\n  Conditional R²:", round(cond_r2, 4))
  cat("\n")
}


#Saving per-species result
all_results <- bind_rows(summary_results)
write_csv(all_results, "GLMM_exposure_results_all_species.csv")
save(model_results, file = "GLMM_exposure_species_models.RData")

r2_summary_df <- bind_rows(
  lapply(names(model_results), function(sp) {
    r     <- safe_r2(model_results[[sp]])
    mtype <- if (!inherits(model_results[[sp]], "lmerMod")) "LM" else "LMM"
    data.frame(
      Species        = sp,
      Model_type     = mtype,
      Marginal_R2    = suppressWarnings(as.numeric(r$R2_marginal)),
      Conditional_R2 = safe_conditional_r2(r)
    )
  })
)
write_csv(r2_summary_df, "GLMM_exposure_R2_summary.csv")

#Running overall model
df_overall <- df %>%
  mutate(BenthicReefSpecies = factor(BenthicReefSpecies)) %>%
  filter(if_all(all_of(c(numeric_cols,
                         "mean_gorgonian_density",
                         "exposure_index_scaled",
                         "Reef_Type", "Site",
                         "BenthicReefSpecies")),
                ~ !is.na(.)))

cat("Overall dataset rows after NA removal:", nrow(df_overall), "\n")

overall_model <- tryCatch({
  suppressWarnings(
    lmer(
      mean_gorgonian_density ~
        mean_giantkelp_density_m2 +
        mean_sst_C +
        mean_chl_mg_m3 +
        mean_CCA_cover +
        mean_Relief_index +
        mean_Relief_SD +
        mean_Substrate_index +
        mean_Substrate_SD +
        dist_200m_bath +
        mean_surveydepth +
        Reef_Type +
        exposure_index_scaled +
        BenthicReefSpecies +
        (1 | Site),
      data      = df_overall,
      REML      = FALSE
    )
  )
}, error = function(e) {
  cat("Overall model failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(overall_model)) {
  
  if (lme4::isSingular(overall_model, tol = 1e-4)) {
    cat("Note: singular fit for overall model.\n")
  }
  
  r2_overall      <- safe_r2(overall_model)
  overall_marg_r2 <- suppressWarnings(as.numeric(r2_overall$R2_marginal))
  overall_cond_r2 <- safe_conditional_r2(r2_overall)
  
  tidy_overall <- broom.mixed::tidy(overall_model) %>%
    mutate(
      t_value        = estimate / std.error,
      Marginal_R2    = overall_marg_r2,
      Conditional_R2 = overall_cond_r2
    )
  
  cat("\n=== OVERALL MODEL FIXED EFFECTS ===\n")
  print(tidy_overall %>%
          filter(effect == "fixed") %>%
          dplyr::select(term, estimate, std.error, t_value))
  
  cat("\nMarginal R²:   ", round(overall_marg_r2, 4), "\n")
  if (!is.na(overall_cond_r2)) cat("Conditional R²:", round(overall_cond_r2, 4), "\n")
  cat("AIC:", round(AIC(overall_model), 2), "\n")
  cat("BIC:", round(BIC(overall_model), 2), "\n")

#Saving overall model
write_csv(tidy_overall, "GLMM_exposure_overall_results.csv")
save(overall_model, file = "GLMM_exposure_overall_model.RData")
}