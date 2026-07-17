##TEMPORAL GORGONIAN QUERY from Avrey's Recommendations
##By Maggie Schaffzin
##Created: 02/26/2026
#Last Edited: 02/26/2026

#TEMPORAL QUERY
library(ggplot2)
library(tidyverse)

#Calculating summaries WITH species included
mean_gorgonian_by_year_species_VRG <- dat_gorgonian %>%
  dplyr::mutate(
    gorgonian_density = (Abundance / area.m2) * 100
  ) %>%
  dplyr::group_by(SampleYear, BenthicReefSpecies) %>%
  dplyr::summarise(
    mean_gorgonian_density = mean(gorgonian_density, na.rm = TRUE),
    n_surveys = n(),
    .groups = "drop"
  ) %>%
  dplyr::rename(year = SampleYear) %>%
  dplyr::mutate(Source = "VRG")

mean_gorgonian_by_year_species_PISCO <- PISCO_gorgonian %>%
  dplyr::mutate(
    gorgonian_density = (count / 60) * 100
  ) %>%
  dplyr::group_by(year, species_definition) %>%
  dplyr::summarise(
    mean_gorgonian_density = mean(gorgonian_density, na.rm = TRUE),
    n_surveys = n(),
    .groups = "drop"
  ) %>%
  dplyr::rename(BenthicReefSpecies = species_definition) %>%
  dplyr::mutate(Source = "PISCO")

# Combine
unified_species_year <- bind_rows(
  mean_gorgonian_by_year_species_VRG,
  mean_gorgonian_by_year_species_PISCO
)

#FIGURE 1: Line plot - trends over time by species, split by source
fig1 <- ggplot(unified_species_year, aes(x = year, y = mean_gorgonian_density,
                                         color = BenthicReefSpecies, group = BenthicReefSpecies)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ Source, scales = "free_x") +
  labs(
    x = "Year",
    y = expression("Mean Gorgonian Density (per 100 m"^2*")"),
    color = "Species"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )

print(fig1)
ggsave("fig1_gorgonian_trends_by_species_source.png", fig1, width = 12, height = 6, dpi = 300)

#FIGURE 2: Separate panels per species, VRG vs PISCO overlaid
fig2 <- ggplot(unified_species_year, aes(x = year, y = mean_gorgonian_density,
                                         color = Source, group = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ BenthicReefSpecies, scales = "free_y") +
  labs(
    x = "Year",
    y = expression("Mean Gorgonian Density (per 100 m"^2*")"),
    color = "Data Source"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8)
  )

print(fig2)
ggsave("fig2_gorgonian_trends_VRG_vs_PISCO_by_species.png", fig2, width = 14, height = 8, dpi = 300)

#FIGURE 3: Stacked area chart - community composition over time by source
fig3 <- ggplot(unified_species_year, aes(x = year, y = mean_gorgonian_density,
                                         fill = BenthicReefSpecies)) +
  geom_area(alpha = 0.7, position = "stack") +
  facet_wrap(~ Source, scales = "free_x") +
  labs(
    x = "Year",
    y = expression("Mean Gorgonian Density (per 100 m"^2*")"),
    fill = "Species"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )
print(fig3)
ggsave("fig3_gorgonian_stacked_area_by_source.png", fig3, width = 12, height = 6, dpi = 300)

library(readr)
library(dplyr)
library(stringr)

setwd("C:/Users/bethm/Documents/Manuscript Work/Datasets")

# ---- Load raw gorgonian data ----
dat_gorgonian <- read_csv("C:/Users/bethm/Downloads/dat_gorgonian_oct18_2024 (1).csv")

# ---- Calculate density per transect, then mean by Site x DepthZone x Species x Year ----
VRG_density_yearly <- dat_gorgonian %>%
  dplyr::select(Region, Site, BenthicReefSpecies, DepthZone, SampleYear,
                Abundance, area.m2) %>%
  dplyr::mutate(
    gorgonian_density = (Abundance / area.m2) * 100
  ) %>%
  dplyr::group_by(Region, Site, DepthZone, BenthicReefSpecies, SampleYear) %>%
  dplyr::summarise(
    mean_gorgonian_density = mean(gorgonian_density, na.rm = TRUE),
    n_transects            = n(),
    .groups = "drop"
  )

head(VRG_density_yearly)
cat("VRG yearly density: ", nrow(VRG_density_yearly), "rows\n")
cat("Years covered:", sort(unique(VRG_density_yearly$SampleYear)), "\n")


# ---- Load environmental data (same averages as before — no year in env data) ----
all_env <- read_csv("C:/Users/bethm/Downloads/all_env_lat_lon_jan15_2025.csv")

env_means <- all_env %>%
  dplyr::select(Site, mean_chl_mg_m3, mean_sst_C, DepthZone, dist_200m_bath,
                giantkelp_density_m2, Relief_index, Relief_SD,
                Substrate_index, Substrate_SD, Latitude, Longitude) %>%
  group_by(Site, DepthZone) %>%
  summarise(
    mean_giantkelp_density_m2 = mean(giantkelp_density_m2, na.rm = TRUE),
    mean_Relief_index          = mean(Relief_index,          na.rm = TRUE),
    mean_Relief_SD             = mean(Relief_SD,             na.rm = TRUE),
    mean_Substrate_index       = mean(Substrate_index,       na.rm = TRUE),
    mean_Substrate_SD          = mean(Substrate_SD,          na.rm = TRUE),
    mean_chl_mg_m3             = mean(mean_chl_mg_m3,        na.rm = TRUE),
    mean_sst_C                 = mean(mean_sst_C,            na.rm = TRUE),
    dist_200m_bath             = mean(dist_200m_bath,        na.rm = TRUE),
    mean_Latitude              = mean(Latitude,              na.rm = TRUE),
    mean_Longitude             = mean(Longitude,             na.rm = TRUE),
    .groups = "drop"
  )

# ---- Load depth data ----
depth_data <- read_csv("C:/Users/bethm/Downloads/gorgonian/Data/VRG_CRANE_site_depthzone_depth.csv")

# ---- Load CCA data ----
cca_data <- read_csv("C:/Users/bethm/Downloads/gorgonian/Data/UPC_CCA_oct18_2024.csv")
cca_means <- cca_data %>%
  group_by(Site, DepthZone) %>%
  summarise(mean_CCA_cover = mean(BRS_per_cov, na.rm = TRUE), .groups = "drop")

# ---- Merge everything — yearly density joined to site-level env averages ----
VRG_yearly_final <- VRG_density_yearly %>%
  left_join(env_means,   by = c("Site", "DepthZone")) %>%
  left_join(depth_data,  by = c("Site", "DepthZone")) %>%
  left_join(cca_means,   by = c("Site", "DepthZone")) %>%
  dplyr::mutate(
    Reef_Type = dplyr::case_when(
      stringr::str_detect(Site, "AR|PVR") ~ "Artificial Reef",
      TRUE ~ "Natural Reef"
    ),
    Source = "VRG",
    Year   = SampleYear
  ) %>%
  dplyr::select(-SampleYear)

cat("VRG yearly final: ", nrow(VRG_yearly_final), "rows\n")

PISCO_gorgonian <- read_csv("C:/Users/bethm/Downloads/Gorgonian/Data/PISCO_gorgonian_swath_16_23.csv")

# ---- Density per transect, then mean by Site x DepthZone x Species x Year ----
PISCO_density_yearly <- PISCO_gorgonian %>%
  dplyr::select(species_definition, site, zone, count, depth, year) %>%
  dplyr::mutate(
    mean_density = (count / 60) * 100
  ) %>%
  dplyr::filter(!is.na(depth)) %>%
  dplyr::group_by(site, zone, species_definition, year) %>%
  dplyr::summarise(
    mean_gorgonian_density = mean(mean_density, na.rm = TRUE),
    mean_surveydepth       = mean(depth,         na.rm = TRUE),
    n_transects            = n(),
    .groups = "drop"
  ) %>%
  dplyr::rename(
    Site               = site,
    DepthZone          = zone,
    BenthicReefSpecies = species_definition,
    Year               = year
  ) %>%
  dplyr::mutate(
    Reef_Type = dplyr::case_when(
      stringr::str_detect(Site, "AR |PVR") ~ "Artificial Reef",
      TRUE ~ "Natural Reef"
    )
  )

cat("PISCO yearly density: ", nrow(PISCO_density_yearly), "rows\n")
cat("Years covered:", sort(unique(PISCO_density_yearly$Year)), "\n")

# ---- Load PISCO environmental data ----
PISCO_all_env <- read_csv("C:/Users/bethm/Downloads/Gorgonian/Data/PISCO_all_env_lat_lon.csv")

PISCO_env_means <- PISCO_all_env %>%
  dplyr::select(Site, mean_chl_mg_m3, mean_sst_C, DepthZone, dist_200m_bath,
                giantkelp_plant_density_m2, Relief_index, Relief_SD,
                Substrate_index, Substrate_SD, Latitude, Longitude) %>%
  group_by(Site, DepthZone) %>%
  summarise(
    mean_giantkelp_density_m2 = mean(giantkelp_plant_density_m2, na.rm = TRUE),
    mean_Relief_index          = mean(Relief_index,               na.rm = TRUE),
    mean_Relief_SD             = mean(Relief_SD,                  na.rm = TRUE),
    mean_Substrate_index       = mean(Substrate_index,            na.rm = TRUE),
    mean_Substrate_SD          = mean(Substrate_SD,               na.rm = TRUE),
    mean_chl_mg_m3             = mean(mean_chl_mg_m3,             na.rm = TRUE),
    mean_sst_C                 = mean(mean_sst_C,                 na.rm = TRUE),
    dist_200m_bath             = mean(dist_200m_bath,             na.rm = TRUE),
    mean_Latitude              = mean(Latitude,                   na.rm = TRUE),
    mean_Longitude             = mean(Longitude,                  na.rm = TRUE),
    .groups = "drop"
  )

# ---- Load PISCO CCA data ----
pisco_cca <- read_csv("PISCO_CCA_UPC_16_23.csv")
pisco_cca_means <- pisco_cca %>%
  dplyr::select(species_definition, site, zone, pct_cov) %>%
  dplyr::group_by(site, zone) %>%
  dplyr::summarise(mean_CCA_cover = mean(pct_cov, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(Site = site, DepthZone = zone)

# ---- Merge everything ----
PISCO_yearly_final <- PISCO_density_yearly %>%
  left_join(PISCO_env_means,  by = c("Site", "DepthZone")) %>%
  left_join(pisco_cca_means,  by = c("Site", "DepthZone")) %>%
  mutate(Source = "PISCO")

cat("PISCO yearly final: ", nrow(PISCO_yearly_final), "rows\n")


# ==============================================================================
# PART 3 — COMBINE VRG + PISCO
# ==============================================================================

common_cols <- intersect(names(VRG_yearly_final), names(PISCO_yearly_final))
cat("\nColumns in both datasets:\n")
print(common_cols)

# Check what's missing from each — useful for debugging
cat("\nIn VRG but not PISCO:", setdiff(names(VRG_yearly_final), names(PISCO_yearly_final)), "\n")
cat("In PISCO but not VRG:", setdiff(names(PISCO_yearly_final), names(VRG_yearly_final)), "\n")

Unified_yearly <- bind_rows(
  VRG_yearly_final[,   common_cols],
  PISCO_yearly_final[, common_cols]
)

cat("\nFinal unified yearly dataset:", nrow(Unified_yearly), "rows\n")
cat("Sites:", n_distinct(Unified_yearly$Site), "\n")
cat("Years:", sort(unique(Unified_yearly$Year)), "\n")
cat("Species:", sort(unique(Unified_yearly$BenthicReefSpecies)), "\n")

glimpse(Unified_yearly)

# ---- Save ----
write_csv(Unified_yearly,
          "Unified_VRG_PISCO_gorgonian_dataset_YEARLY.csv")
cat("\nSaved: Unified_VRG_PISCO_gorgonian_dataset_YEARLY.csv\n")


# ==============================================================================
# PART 4 — QUICK SANITY CHECK PLOTS
# ==============================================================================

library(ggplot2)

# Overall density trend across all sites and species
Unified_yearly %>%
  group_by(Year, Source) %>%
  summarise(mean_dens = mean(mean_gorgonian_density, na.rm = TRUE),
            se        = sd(mean_gorgonian_density,   na.rm = TRUE) /
              sqrt(n()),
            .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_dens, color = Source, group = Source)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_dens - se, ymax = mean_dens + se, fill = Source),
              alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(title   = "Mean Gorgonian Density Over Time",
       subtitle = "Averaged across all sites and species | ±1 SE",
       x = "Year", y = "Mean density (per 100 m²)") +
  theme_bw()
ggsave("gorgonian_density_trend_overall.png", width = 9, height = 5, dpi = 300)

# Per-species trend
Unified_yearly %>%
  group_by(Year, BenthicReefSpecies, Source) %>%
  summarise(mean_dens = mean(mean_gorgonian_density, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = Year, y = mean_dens, color = Source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~BenthicReefSpecies, scales = "free_y") +
  scale_x_continuous(breaks = c(2016, 2019, 2022)) +
  labs(title = "Gorgonian Density by Species Over Time",
       x = "Year", y = "Mean density (per 100 m²)") +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom")
ggsave("gorgonian_density_trend_by_species.png", width = 12, height = 8, dpi = 300)

cat("\n=== YEARLY DATASET CREATION COMPLETE ===\n")

#Project: Delta Density GLMM — year-over-year change in gorgonian density
#By: Maggie Schaffzin
#Created: February 27th, 2026
# Response variable: delta_density = density[year] - density[year-1]
# Positive = population increased that year, negative = declined
# Year included as Year_factor (categorical) to capture discrete annual signals
# (e.g. post-Blob recovery, good recruitment years) rather than a linear trend
#
# Requires: Unified_VRG_PISCO_gorgonian_dataset_YEARLY.csv
#           (created by create_yearly_dataset.R)

library(dplyr)
library(lme4)
library(performance)
library(broom.mixed)
library(readr)
library(stringr)
library(ggplot2)

setwd("C:/Users/bethm/Documents/Manuscript Work/Datasets")


# ==============================================================================
# STEP 1 — LOAD YEARLY DATASET AND COMPUTE DELTA DENSITY
# ==============================================================================

df_yearly <- read_csv("Unified_VRG_PISCO_gorgonian_dataset_YEARLY.csv",
                      show_col_types = FALSE)

df_delta <- df_yearly %>%
  arrange(Site, DepthZone, BenthicReefSpecies, Source, Year) %>%
  group_by(Site, DepthZone, BenthicReefSpecies, Source) %>%
  mutate(
    density_lag1  = lag(mean_gorgonian_density),
    delta_density = mean_gorgonian_density - density_lag1
  ) %>%
  ungroup() %>%
  filter(!is.na(delta_density))

cat("Delta density dataset:", nrow(df_delta), "rows\n")
cat("Year transitions covered:\n")
print(sort(unique(df_delta$Year)))

cat("\nDelta density summary:\n")
print(summary(df_delta$delta_density))

cat("\nPositive (increase):", sum(df_delta$delta_density > 0, na.rm = TRUE), "\n")
cat("Negative (decline): ", sum(df_delta$delta_density < 0, na.rm = TRUE), "\n")
cat("Zero:               ", sum(df_delta$delta_density == 0, na.rm = TRUE), "\n")

df_delta <- df_delta %>%
  mutate(
    Reef_Type          = factor(Reef_Type),
    BenthicReefSpecies = factor(str_trim(as.character(BenthicReefSpecies))),
    Year               = as.integer(Year),
    Year_factor        = factor(Year)
  )


# ==============================================================================
# STEP 2 — SANITY CHECK PLOT: AVERAGE DELTA BY YEAR
# ==============================================================================

df_delta %>%
  group_by(Year, Source) %>%
  summarise(mean_delta = mean(delta_density, na.rm = TRUE),
            se         = sd(delta_density,   na.rm = TRUE) / sqrt(n()),
            .groups    = "drop") %>%
  ggplot(aes(x = Year, y = mean_delta, color = Source, group = Source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = mean_delta - se, ymax = mean_delta + se, fill = Source),
              alpha = 0.2, color = NA) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2017:2024) +
  labs(title   = "Mean Year-over-Year Change in Gorgonian Density",
       subtitle = "Averaged across all sites and species | ±1 SE\nAbove zero = net increase, below zero = net decline",
       x = "Year (transition TO this year)", y = "Delta density (per 100 m²)") +
  theme_bw()
ggsave("delta_density_by_year.png", width = 9, height = 5, dpi = 300)
message("Plot saved: delta_density_by_year.png")


# ==============================================================================
# STEP 3 — DEFINE PREDICTORS
# ==============================================================================

numeric_cols <- c(
  "mean_giantkelp_density_m2", "mean_sst_C", "mean_chl_mg_m3",
  "mean_CCA_cover", "mean_Relief_index", "mean_Relief_SD",
  "mean_Substrate_index", "mean_Substrate_SD",
  "dist_200m_bath", "mean_surveydepth",
  "density_lag1"
  # Year is NOT scaled — used as Year_factor (categorical) in models
)

species_list <- unique(df_delta$BenthicReefSpecies)


# ==============================================================================
# STEP 4 — PER-SPECIES DELTA MODELS
# ==============================================================================

model_results   <- list()
summary_results <- list()

for (sp in species_list) {
  
  cat("\nRunning delta model for species:", sp, "\n")
  
  df_sp <- df_delta %>% filter(BenthicReefSpecies == sp)
  if (nrow(df_sp) < 10) {
    cat("  Skipping — too few rows (", nrow(df_sp), ")\n")
    next
  }
  
  df_sp <- df_sp %>%
    mutate(across(all_of(numeric_cols), ~ as.numeric(scale(.))))
  
  model <- tryCatch({
    lmer(
      delta_density ~ mean_giantkelp_density_m2 +
        mean_sst_C + mean_chl_mg_m3 + mean_CCA_cover +
        mean_Relief_index + mean_Relief_SD +
        mean_Substrate_index + mean_Substrate_SD +
        dist_200m_bath + mean_surveydepth +
        Reef_Type +
        density_lag1 +
        Year_factor +
        (1 | Site),
      data = df_sp,
      REML = FALSE
    )
  }, error = function(e) {
    cat("  Model failed:", e$message, "\n")
    return(NULL)
  })
  if (is.null(model)) next
  
  if (lme4::isSingular(model, tol = 1e-4)) {
    cat("  Warning: singular fit for", sp, "\n")
  }
  
  tidy_df <- broom.mixed::tidy(model) %>%
    mutate(Species = sp,
           t_value = estimate / std.error)
  
  r2_val  <- performance::r2(model)
  tidy_df <- tidy_df %>%
    mutate(
      Marginal_R2    = r2_val$R2_marginal,
      Conditional_R2 = ifelse(r2_val$R2_conditional == 0, NA, r2_val$R2_conditional)
    )
  
  model_results[[sp]]   <- model
  summary_results[[sp]] <- tidy_df
  
  print(tidy_df[, c("term", "estimate", "std.error", "t_value")])
}

all_results <- bind_rows(summary_results)
write_csv(all_results,
          "GLMM_delta_density_results_all_species_2_27_26.csv")
save(model_results,
     file = "GLMM_delta_density_species_models_2_27_26.RData")

r2_summary_df <- bind_rows(
  lapply(model_results, function(m) {
    r <- performance::r2(m)
    data.frame(Marginal_R2    = r$R2_marginal,
               Conditional_R2 = ifelse(r$R2_conditional == 0, NA, r$R2_conditional))
  }),
  .id = "Species"
)
write_csv(r2_summary_df,
          "GLMM_delta_density_R2_summary_2_27_26.csv")


# ==============================================================================
# STEP 5 — OVERALL DELTA MODEL (ALL SPECIES)
# ==============================================================================

cat("\n========================================\n")
cat("RUNNING OVERALL DELTA MODEL (ALL SPECIES)\n")
cat("========================================\n")

# BenthicReefSpecies as fixed effect — gives a coefficient per species
# relative to the reference level (alphabetically first species)
# Site remains as the only random effect
df_overall <- df_delta %>%
  mutate(
    BenthicReefSpecies = factor(BenthicReefSpecies),
    across(all_of(numeric_cols), ~ as.numeric(scale(.)))
  )

overall_model <- tryCatch({
  lmer(
    delta_density ~ mean_giantkelp_density_m2 +
      mean_sst_C + mean_chl_mg_m3 + mean_CCA_cover +
      mean_Relief_index + mean_Relief_SD +
      mean_Substrate_index + mean_Substrate_SD +
      dist_200m_bath + mean_surveydepth +
      Reef_Type + 
      density_lag1 +
      Year_factor +
      BenthicReefSpecies +   # fixed effect: coefficient per species vs. reference
      (1 | Site),            # Site only random effect
    data = df_overall,
    REML = FALSE
  )
}, error = function(e) {
  cat("Overall model failed:", e$message, "\n")
  return(NULL)
})

if (!is.null(overall_model)) {
  
  if (lme4::isSingular(overall_model, tol = 1e-4)) {
    cat("Warning: Overall model has singular fit.\n")
  }
  
  cat("\n=== OVERALL DELTA MODEL SUMMARY ===\n")
  print(summary(overall_model))
  
  tidy_overall <- broom.mixed::tidy(overall_model) %>%
    mutate(t_value = estimate / std.error)
  
  r2_overall   <- performance::r2(overall_model)
  tidy_overall <- tidy_overall %>%
    mutate(
      Marginal_R2    = r2_overall$R2_marginal,
      Conditional_R2 = r2_overall$R2_conditional
    )
  
  cat("\n=== FIXED EFFECTS ===\n")
  print(tidy_overall %>%
          filter(effect == "fixed") %>%
          select(term, estimate, std.error, t_value))
  
  cat("\n=== RANDOM EFFECTS ===\n")
  print(tidy_overall %>%
          filter(effect == "ran_pars") %>%
          select(group, term, estimate))
  
  cat("\n=== MODEL FIT ===\n")
  cat("Marginal R²:",    round(r2_overall$R2_marginal,    4), "\n")
  cat("Conditional R²:", round(r2_overall$R2_conditional, 4), "\n")
  cat("AIC:", round(AIC(overall_model), 2), "\n")
  cat("BIC:", round(BIC(overall_model), 2), "\n")
  
  write_csv(tidy_overall,
            "GLMM_delta_density_overall_results_2_27_26.csv")
  save(overall_model,
       file = "GLMM_delta_density_overall_model_2_27_26.RData")
  
  r2_overall_summary <- data.frame(
    Model          = "Overall_Delta",
    Marginal_R2    = r2_overall$R2_marginal,
    Conditional_R2 = r2_overall$R2_conditional,
    AIC            = AIC(overall_model),
    BIC            = BIC(overall_model)
  )
  write_csv(r2_overall_summary,
            "GLMM_delta_density_overall_R2_2_27_26.csv")
  
  cat("\n========================================\n")
  cat("OVERALL DELTA MODEL COMPLETE\n")
  cat("========================================\n")
}
