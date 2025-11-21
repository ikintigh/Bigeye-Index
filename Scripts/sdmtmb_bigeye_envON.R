# Purpose: Fit the environment-ON sdmTMB model for DSLL Bigeye CPUE, adding habitat covariates (SST, MLD, O2, SLA) to the baseline env-OFF structure. This model tests the hypothesis from the proposal that changes in availability/catchability driven by ocean conditions explain part of the CPUE trend and residual structure.

# Created by: Isabella Kintigh
# Created on: 20 November 2025

################################################################################

# ------------------------- Libraries -------------------------------------------

library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sdmTMBextra)

# ----------------------------- Load in dfs -----------------------------------

DSLL_Bigeye_df <- readRDS(here("Data", "DSLL_Bigeye_df.rds"))
mesh <- readRDS(here("Data", "mesh_dsll_bigeye.rds"))
effort_poly <- readRDS(here("Data", "Effort_90pct_Bigeye_DSLL.rds"))

# ----------------------------- Data wrangling ---------------------------------

DSLL_Bigeye_df <- DSLL_Bigeye_df %>%
  mutate(
    year = as.numeric(as.character(year)),
    bait_code_val = as.factor(bait_code_val),
    ldr_mat_code_val = as.factor(ldr_mat_code_val),
    brnchln_mat_code_val = as.factor(brnchln_mat_code_val)
  ) %>%
  filter(
    !is.na(sst),
    !is.na(mld),
    !is.na(o2),
    !is.na(sla)
  )

# Standardize environmental predictors
DSLL_Bigeye_df <- DSLL_Bigeye_df %>%
  mutate(
    sst_sc       = scale(sst),
    mld_sc       = scale(mld),
    o2_sc        = scale(o2),
    sla_sc       = scale(sla)
  )


# ------------------------ Environment-ON Formula ---------------------
#  - Year + gear (bait, leader) as fixed effects
#  - Depth proxy (floatline length) as smooth
#  - Seasonal smooth (DOY, cyclic)
#  - Environmental smooths: SST, MLD, O2, SLA

formula_envON <- cpue ~ 
  year + # time trend
  bait_code_val + # bait type (gear covariate)
  ldr_mat_code_val + # leader material (gear covariate)
  fltln_len  + # depth proxy smooth
  s(DOY, bs = "cc", k = 4) + # seasonal (cyclic) smooth
  s(sst_sc,  k = 5) + # SST smooth
  s(mld_sc,  k = 5) + # MLD smooth
  s(o2_sc,   k = 5) + # O2 smooth
  s(sla_sc,  k = 5) # SLA smooth

# ---------------------- Fit Environment-ON Model ---------------------
fit_envON <- sdmTMB(
  formula = formula_envON,
  data = DSLL_Bigeye_df,
  mesh = mesh,
  family = lognormal(link = "log"),
  spatial = "on",
  spatiotemporal = "ar1",
  time = "year",
  return_tmb_object = TRUE
)


# ------------------------ Save -----------------------------------------------

saveRDS(fit_envON, here("Output", "fit_envON_bigeye.rds"))

# ------------------ Check convergence and model diagnostics ------------------

summary(fit_envON)

sanity(fit_envON)

# --------------------------- Extract indices -------------------------------

pred_envON <- predict(
  fit_envON,
  newdata = DSLL_Bigeye_df,
  return_tmb_object = TRUE
)

index_envON <- get_index(pred_envON, bias_correct = TRUE)

# -------------------------------- Save -------------------------------------

saveRDS(index_envON, here("Output", "index_envON_bigeye.rds"))





