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

DSLL_Bigeye <- readRDS(here("Data", "DSLL_Bigeye_O2_SLA.rds"))
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

### Including this portion from the mesh script so that the data is the same that we built the mesh from

# Convert to sf points
DSLL_Bigeye_sf <- st_as_sf(DSLL_Bigeye,
                           coords = c("cent_lon", "cent_lat"),
                           crs = 4326)

# Crop points to inside the 90% polygon
DSLL_Bigeye_sf <- st_intersection(DSLL_Bigeye_sf, effort_poly)

# Recreate explicit x/y columns from the geometry
coords <- st_coordinates(DSLL_Bigeye_sf)

# Shift negative longitudes to 0–360
coords[, "X"] <- ifelse(coords[, "X"] < 0, coords[, "X"] + 360, coords[, "X"])

DSLL_Bigeye_sf <- DSLL_Bigeye_sf %>%
  mutate(x = coords[, "X"],
         y = coords[, "Y"])

# Make it a dataframe

DSLL_Bigeye_df <- as.data.frame(DSLL_Bigeye_sf)

# Make sure there are no explicit zeros 

DSLL_Bigeye_df$cpue <- ifelse(DSLL_Bigeye_df$cpue <= 0, 0.0001, DSLL_Bigeye_df$cpue)


# ----------------------------- Model Formula ---------------------------------

formula_envOFF <- cpue ~
  year + # time trend
  bait_code_val + # gear covariate
  ldr_mat_code_val + # leader material
  brnchln_mat_code_val + # branchline material #### Can get rid 
  brnchln_len + # depth proxy ### Include floatline len rather than branch line 
  s(DOY, k = 5) # smooth for seasonality # Add bs = cc 

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






