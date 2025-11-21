# Purpose: Fit the environment-OFF (baseline) sdmTMB model for DSLL Bigeye CPUE. This model estimates standardized CPUE as a function of year and key gear covariates, with spatial (Ω) and spatiotemporal (Ε) random fields on the SPDE mesh. No environmental covariates are included, making this a baseline standardization for comparison to the later environment-ON model.

# Created by: Isabella Kintigh
# Created on: 11 November 2025

#########################################################################################

# ------------------------- Libraries -------------------------------------------

library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sdmTMBextra)

# ----------------------------- Load in dfs -----------------------------------

DSLL_Bigeye <- readRDS(here("Data", "DSLL_Bigeye_df.rds"))
mesh <- readRDS(here("Data", "mesh_dsll_bigeye.rds"))
effort_poly <- readRDS(here("Data", "Effort_90pct_Bigeye_DSLL.rds"))
fit_envOFF <- readRDS(here("Data", "fit_envOFF_bigeye.rds"))

# ----------------------------- Data wrangling ---------------------------------

DSLL_Bigeye <- DSLL_Bigeye %>%
  mutate(
    year = as.numeric(as.character(year)),
    bait_code_val = as.factor(bait_code_val),
    ldr_mat_code_val = as.factor(ldr_mat_code_val),
    brnchln_mat_code_val = as.factor(brnchln_mat_code_val)
  )



# ----------------------------- Model Formula ---------------------------------

formula_envOFF <- cpue ~
  year + # time trend
  bait_code_val + # gear covariate
  ldr_mat_code_val + # leader material
  fltln_len + # have to separate branchline and floatline because they were too correlated
  s(DOY, bs = "cc", k = 4) # smooth for seasonality # Add bs = cc 

# ------------------------ Fit Environment-OFF Model ---------------------------

fit_envOFF <- sdmTMB(
  formula = formula_envOFF,
  data = DSLL_Bigeye_df,
  mesh = mesh,
  family = lognormal(link = "log"),
  spatial = "on",
  spatiotemporal = "ar1",
  time = "year"
)


# ------------------------ Save -----------------------------------------------

saveRDS(fit_envOFF, here("Output", "fit_envOFF_bigeye.rds"))

# ------------------------ Check convergence and model diagnostics --------------

summary(fit_envOFF)

sanity(fit_envOFF)

# ---------------------------- Plot residuals ----------------------------------
DSLL_Bigeye_df$resid <- residuals(fit_envOFF, type = "response")
DSLL_Bigeye_df$fitted <- fitted(fit_envOFF)

plot(DSLL_Bigeye_df$fitted, DSLL_Bigeye_df$resid,
     pch = 16, cex = 0.3, col = "grey60",
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted (Environment-OFF Model)")
abline(h = 0, col = "red")



resid_log <- residuals(fit_envOFF, type = "deviance")

plot(fitted(fit_envOFF), resid_log,
     pch = 16, cex = 0.3, col = "grey60",
     xlab = "Fitted (log scale)",
     ylab = "Residuals (scaled log scale)",
     main = "Residuals vs Fitted (Lognormal Model)")
abline(h = 0, col = "red")

# --------------------------- Extract indices -------------------------------

pred_envOFF <- predict(
  fit_envOFF,
  newdata = DSLL_Bigeye_df,
  return_tmb_object = TRUE
)

index_envOFF <- get_index(pred_envOFF, bias_correct = TRUE)

# -------------------------------- Save -------------------------------------

saveRDS(index_envOFF, here("Output", "index_envOFF_bigeye.rds"))






