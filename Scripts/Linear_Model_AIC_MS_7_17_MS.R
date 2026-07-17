# ==============================================================================
# Project: Linear Mixed Effects Model — AIC-based model selection
#          with model averaging across supported models (delta AICc <= 2)
# By: Maggie Schaffzin
# Created: October 25th, 2025
# Last edited: May 20th, 2026
# ==============================================================================

library(dplyr)
library(stringr)
library(lme4)
library(MuMIn)
library(performance)
library(broom.mixed)
library(readr)

#Setting Working directory
setwd("C:/Users/bethm/Documents/Manuscript Work/Datasets")

#Safe extraction of Conditional R²
safe_conditional_r2 <- function(r2_obj) {
  val <- r2_obj$R2_conditional
  if (is.null(val) || length(val) == 0) return(NA_real_)
  val <- suppressWarnings(as.numeric(val))
  if (is.na(val) || val == 0) return(NA_real_)
  return(val)
}

#Safe r2() wrapper — normalises LM vs LMM output
safe_r2 <- function(model) {
  r <- tryCatch(
    suppressWarnings(suppressMessages(performance::r2(model))),
    error = function(e) return(list(R2_marginal = NA_real_,
                                    R2_conditional = NA_real_))
  )
  if (is.null(r$R2_marginal) && !is.null(r$R2)) {
    r$R2_marginal    <- r$R2
    r$R2_conditional <- NA_real_
  }
  if (is.null(r$R2_marginal))    r$R2_marginal    <- NA_real_
  if (is.null(r$R2_conditional)) r$R2_conditional <- NA_real_
  return(r)
}

#Model type label
model_type_label <- function(model, global_was_lm) {
  if (global_was_lm)                       return("LM")
  if (!inherits(model, "lmerMod"))         return("LM")
  if (lme4::isSingular(model, tol = 1e-4)) return("LMM-singular")
  return("LMM")
}

#Loading and preparing data
df <- read_csv("Unified_VRG_PISCO_gorgonian_dataset_with_exposure.csv", show_col_types = FALSE)

df <- df %>%
  mutate(
    Reef_Type          = factor(Reef_Type),
    BenthicReefSpecies = str_trim(as.character(BenthicReefSpecies))
  )

#Numeric predictors — includes WEI
numeric_cols <- c(
  "mean_giantkelp_density_m2", "mean_sst_C", "mean_chl_mg_m3",
  "mean_CCA_cover", "mean_Relief_index", "mean_Relief_SD",
  "mean_Substrate_index", "mean_Substrate_SD",
  "dist_200m_bath", "mean_surveydepth", "exposure_index_scaled"
)

#Checking for NAs
cat("\nNA counts per predictor column:\n")
print(colSums(is.na(df[, c(numeric_cols,
                           "mean_gorgonian_density",
                           "Reef_Type", "Site")])))

#Scaling all numeric predictors once on the full dataset (so coefficients are directly comparable across species models)
df <- df %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(scale(.))))

#Collinearity check
cor_matrix <- cor(df[, numeric_cols], use = "pairwise.complete.obs")
high_cor   <- which(abs(cor_matrix) > 0.7 & upper.tri(cor_matrix),
                    arr.ind = TRUE)

if (nrow(high_cor) > 0) {
  cat("\nVariable pairs with |r| > 0.7:\n")
  for (i in seq_len(nrow(high_cor))) {
    r1 <- rownames(cor_matrix)[high_cor[i, 1]]
    r2 <- colnames(cor_matrix)[high_cor[i, 2]]
    cat(sprintf("  %s -- %s : r = %.3f\n", r1, r2,
                cor_matrix[high_cor[i, 1], high_cor[i, 2]]))
  }
} else {
  cat("\nNo pairs exceed r = 0.7 threshold.\n")
}

#Per-species AIC Model Selection + model averaging 
species_list <- unique(df$BenthicReefSpecies)
species_list <- species_list[!is.na(species_list)]

#Creating storage lists
model_results      <- list()   #best single model per species
avg_model_results  <- list()   #model-averaged object per species
dredge_results     <- list()   #full dredge table per species
importance_results <- list()   #variable importance scores per species
avg_coef_results   <- list()   #model-averaged coefficients per species
summary_results    <- list()   #tidy best-model coefficients per species

for (sp in species_list) {
  
  cat("\n========================================\n")
  cat("Species:", sp, "\n")
  cat("========================================\n")
  
  # Subset and remove NAs
  df_sp <- df %>%
    filter(BenthicReefSpecies == sp) %>%
    filter(if_all(all_of(c(numeric_cols,
                           "mean_gorgonian_density",
                           "Reef_Type", "Site")),
                  ~ !is.na(.)))
  
  if (nrow(df_sp) < 10) {
    cat("  Skipping — too few rows after NA removal (", nrow(df_sp), ")\n")
    next
  }
  
  cat("  Rows after NA removal:", nrow(df_sp), "\n")
  
  #Fitting global LMM with Site as random intercept 
  global_model <- tryCatch({
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
          exposure_index_scaled +
          Reef_Type +
          (1 | Site),
        data      = df_sp,
        REML      = FALSE,
        na.action = na.fail
      )
    )
  }, error = function(e) {
    cat("  Global model failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(global_model)) next
  

  #Checking singular fit

  use_lm <- lme4::isSingular(global_model, tol = 1e-4)
  
  if (use_lm) {
    cat("  Note: singular fit on global model (Site variance ~ 0) — refitting as LM.\n")
    global_model <- tryCatch({
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
          exposure_index_scaled +
          Reef_Type,
        data      = df_sp,
        na.action = na.fail
      )
    }, error = function(e) {
      cat("  LM fallback failed:", e$message, "\n")
      return(NULL)
    })
    if (is.null(global_model)) next
  } else {
    cat("  LMM fit OK (Site variance > 0).\n")
  }
  

  #Dredging: rank all predictor subsets by AICc
  cat("  Running dredge...\n")
  dredge_table <- tryCatch({
    suppressWarnings(suppressMessages(
      MuMIn::dredge(global_model, rank = "AICc")
    ))
  }, error = function(e) {
    cat("  Dredge failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(dredge_table)) {
    cat("  Skipping", sp, "— dredge returned NULL.\n")
    next
  }
  
  cat("\n  --- Top 5 models ---\n")
  print(head(dredge_table, 5))
  
  #Variable importance: sum of Akaike weights across all candidate models
  importance_scores <- MuMIn::sw(dredge_table)
  cat("\n  --- Variable importance scores ---\n")
  print(importance_scores)
  
  #Model averaging across supported models (delta AICc <= 2)
  #full = TRUE: predictors absent from a model are treated as zero
  #(more conservative, recommended by Burnham & Anderson 2002)
  supported_models <- tryCatch({
    MuMIn::get.models(dredge_table, subset = delta <= 2)
  }, error = function(e) {
    cat("  Could not extract supported models:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(supported_models) || length(supported_models) == 0) {
    cat("  No supported models found — skipping model averaging.\n")
    next
  }
  
  cat("\n  Number of supported models (delta AICc <= 2):", length(supported_models), "\n")
  
  avg_model <- tryCatch({
    MuMIn::model.avg(supported_models, full = TRUE)
  }, error = function(e) {
    cat("  Model averaging failed:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(avg_model)) next
  
  cat("\n  --- Model-averaged coefficients ---\n")
  print(summary(avg_model))
  
  #Extracting model-averaged coefficients and 95% CIs
  avg_coef_table <- as.data.frame(coefTable(avg_model, full = TRUE))
  avg_coef_table$lower_CI <- avg_coef_table$Estimate - 1.96 * avg_coef_table$`Std. Error`
  avg_coef_table$upper_CI <- avg_coef_table$Estimate + 1.96 * avg_coef_table$`Std. Error`
  avg_coef_table$Species  <- sp
  avg_coef_table$term     <- rownames(avg_coef_table)
  rownames(avg_coef_table) <- NULL
  
  #Flagging predictors whose 95% CI does not overlap zero
  avg_coef_table$significant <- with(avg_coef_table,
                                     lower_CI > 0 | upper_CI < 0)
  
  cat("\n  --- Model-averaged coefficients with 95% CIs ---\n")
  print(avg_coef_table[, c("term", "Estimate", "Std. Error",
                           "lower_CI", "upper_CI", "significant")])
  
  #Also extracting best single model for R² (model.avg objects don't have R²)
  best_model <- MuMIn::get.models(dredge_table, subset = 1)[[1]]
  
  r2_val  <- safe_r2(best_model)
  mtype   <- model_type_label(best_model, use_lm)
  cond_r2 <- safe_conditional_r2(r2_val)
  marg_r2 <- suppressWarnings(as.numeric(r2_val$R2_marginal))
  
  #Adding R² info to averaged coefficient table
  avg_coef_table$Model_type     <- mtype
  avg_coef_table$Marginal_R2    <- marg_r2
  avg_coef_table$Conditional_R2 <- cond_r2
  
  #Also keeping tidy best-model output for reference
  tidy_df <- broom.mixed::tidy(best_model) %>%
    mutate(
      Species        = sp,
      t_value        = estimate / std.error,
      Model_type     = mtype,
      Marginal_R2    = marg_r2,
      Conditional_R2 = cond_r2
    )
  
  #Storing results
  model_results[[sp]]      <- best_model
  avg_model_results[[sp]]  <- avg_model
  dredge_results[[sp]]     <- dredge_table
  importance_results[[sp]] <- importance_scores
  avg_coef_results[[sp]]   <- avg_coef_table
  summary_results[[sp]]    <- tidy_df
  
  cat("\n  Model type:    ", mtype)
  cat("\n  Marginal R²:   ", round(marg_r2, 4))
  if (!is.na(cond_r2)) cat("\n  Conditional R²:", round(cond_r2, 4))
  cat("\n")
  
}

#Saving per-species results
#Model-averaged coefficients — primary inference output
all_avg_coefs <- bind_rows(avg_coef_results)
write_csv(all_avg_coefs, "GLMM_AIC_modelavg_coefficients.csv")

#Best-model tidy output — for reference and R² reporting
all_results <- bind_rows(summary_results)
write_csv(all_results, "GLMM_AIC_bestmodel_coefficients.csv")

#Saving model objects
save(model_results,     file = "GLMM_AIC_best_models.RData")
save(avg_model_results, file = "GLMM_AIC_averaged_models.RData")
save(dredge_results,    file = "GLMM_AIC_dredge_tables.RData")

#Variable importance
importance_df <- bind_rows(
  lapply(importance_results, function(x) {
    data.frame(Variable   = names(x),
               Importance = as.numeric(x))
  }),
  .id = "Species"
)
write_csv(importance_df, "GLMM_AIC_variable_importance.csv")

#Creating R² summary from best models
r2_summary_df <- bind_rows(
  lapply(names(model_results), function(sp) {
    m     <- model_results[[sp]]
    r     <- safe_r2(m)
    mtype <- model_type_label(m, !inherits(m, "lmerMod"))
    data.frame(
      Species        = sp,
      Model_type     = mtype,
      Marginal_R2    = suppressWarnings(as.numeric(r$R2_marginal)),
      Conditional_R2 = safe_conditional_r2(r)
    )
  })
)
write_csv(r2_summary_df, "GLMM_AIC_R2_summary.csv")

cat("\n========================================\n")
cat("PER-SPECIES MODELS COMPLETE\n")
cat("Outputs saved:\n")
cat("  - GLMM_AIC_modelavg_coefficients.csv  (PRIMARY — use for inference)\n")
cat("  - GLMM_AIC_bestmodel_coefficients.csv (reference)\n")
cat("  - GLMM_AIC_variable_importance.csv\n")
cat("  - GLMM_AIC_R2_summary.csv\n")
cat("  - GLMM_AIC_best_models.RData\n")
cat("  - GLMM_AIC_averaged_models.RData\n")
cat("  - GLMM_AIC_dredge_tables.RData\n")
cat("========================================\n")

#Overall model (All species combined)
cat("\n========================================\n")
cat("RUNNING OVERALL MODEL (ALL SPECIES)\n")
cat("========================================\n")

df_overall <- df %>%
  mutate(BenthicReefSpecies = factor(BenthicReefSpecies)) %>%
  filter(if_all(all_of(c(numeric_cols,
                         "mean_gorgonian_density",
                         "Reef_Type", "Site",
                         "BenthicReefSpecies")),
                ~ !is.na(.)))

cat("Overall dataset rows after NA removal:", nrow(df_overall), "\n")

global_overall <- tryCatch({
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
        exposure_index_scaled +
        Reef_Type +
        (1 | BenthicReefSpecies) +
        (1 | Site),
      data      = df_overall,
      REML      = FALSE,
      na.action = na.fail
    )
  )
}, error = function(e) {
  cat("Overall global model failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(global_overall)) {
  
  overall_use_lm <- lme4::isSingular(global_overall, tol = 1e-4)
  
  if (overall_use_lm) {
    cat("Note: singular fit for overall global model — refitting as LM.\n")
    global_overall <- tryCatch({
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
          exposure_index_scaled +
          Reef_Type +
          BenthicReefSpecies,
        data      = df_overall,
        na.action = na.fail
      )
    }, error = function(e) {
      cat("Overall LM fallback failed:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("Overall LMM fit OK.\n")
  }
  
  if (!is.null(global_overall)) {
    
    cat("Running overall dredge...\n")
    dredge_overall <- tryCatch({
      suppressWarnings(suppressMessages(
        MuMIn::dredge(global_overall, rank = "AICc")
      ))
    }, error = function(e) {
      cat("Overall dredge failed:", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(dredge_overall)) {
      
      cat("\n--- Overall top 5 models ---\n")
      print(head(dredge_overall, 5))
      
      importance_overall <- MuMIn::sw(dredge_overall)
      cat("\n--- Overall variable importance scores ---\n")
      print(importance_overall)
      
      # Model averaging across supported overall models
      supported_overall <- tryCatch({
        MuMIn::get.models(dredge_overall, subset = delta <= 2)
      }, error = function(e) {
        cat("Could not extract supported overall models:", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(supported_overall) && length(supported_overall) > 0) {
        
        cat("\nNumber of supported overall models (delta AICc <= 2):",
            length(supported_overall), "\n")
        
        avg_overall <- tryCatch({
          MuMIn::model.avg(supported_overall, full = TRUE)
        }, error = function(e) {
          cat("Overall model averaging failed:", e$message, "\n")
          return(NULL)
        })
        
        if (!is.null(avg_overall)) {
          
          cat("\n--- Overall model-averaged coefficients ---\n")
          print(summary(avg_overall))
          
          # Extract coefficients and 95% CIs
          avg_overall_coefs <- as.data.frame(coefTable(avg_overall, full = TRUE))
          avg_overall_coefs$lower_CI  <- avg_overall_coefs$Estimate -
            1.96 * avg_overall_coefs$`Std. Error`
          avg_overall_coefs$upper_CI  <- avg_overall_coefs$Estimate +
            1.96 * avg_overall_coefs$`Std. Error`
          avg_overall_coefs$term      <- rownames(avg_overall_coefs)
          avg_overall_coefs$significant <- with(avg_overall_coefs,
                                                lower_CI > 0 | upper_CI < 0)
          rownames(avg_overall_coefs) <- NULL
          
          cat("\n--- Overall model-averaged coefficients with 95% CIs ---\n")
          print(avg_overall_coefs[, c("term", "Estimate", "Std. Error",
                                      "lower_CI", "upper_CI", "significant")])
          
          #R² from best single overall model
          best_overall    <- MuMIn::get.models(dredge_overall, subset = 1)[[1]]
          r2_overall      <- safe_r2(best_overall)
          overall_mtype   <- model_type_label(best_overall, overall_use_lm)
          overall_cond_r2 <- safe_conditional_r2(r2_overall)
          overall_marg_r2 <- suppressWarnings(as.numeric(r2_overall$R2_marginal))
          
          avg_overall_coefs$Model_type     <- overall_mtype
          avg_overall_coefs$Marginal_R2    <- overall_marg_r2
          avg_overall_coefs$Conditional_R2 <- overall_cond_r2
          
          cat("\nOverall model type:    ", overall_mtype, "\n")
          cat("Overall marginal R²:   ", round(overall_marg_r2, 4), "\n")
          if (!is.na(overall_cond_r2)) {
            cat("Overall conditional R²:", round(overall_cond_r2, 4), "\n")
          }
          
          write_csv(avg_overall_coefs, "GLMM_AIC_overall_modelavg_coefficients.csv")
          save(avg_overall, file = "GLMM_AIC_overall_averaged_model.RData")
          
          importance_overall_df <- data.frame(
            Variable   = names(importance_overall),
            Importance = as.numeric(importance_overall)
          )
          write_csv(importance_overall_df,
                    "GLMM_AIC_overall_variable_importance.csv")
        }
      }
    }
  }
}

cat("\n========================================\n")
cat("ALL MODELS COMPLETE\n")
cat("Primary inference outputs:\n")
cat("  - GLMM_AIC_modelavg_coefficients.csv\n")
cat("  - GLMM_AIC_overall_modelavg_coefficients.csv\n")
cat("  - GLMM_AIC_variable_importance.csv\n")
cat("  - GLMM_AIC_overall_variable_importance.csv\n")
cat("  - GLMM_AIC_R2_summary.csv\n")
cat("========================================\n")
