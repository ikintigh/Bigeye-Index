# Purpose: To add dissolved oxygen and sea level anamolies to df

# Created by: Isabella Kintigh
# Created on: 31 October 2025

###########################################################################

# --------------------------- Load libraries -----------------------------------

library(raster)
library(here)
library(tidyverse)
library(terra)
library(magrittr)
library(lubridate)

# ------------------------------ Load dfs -------------------------------------

DSLL_Bigeye <- readRDS(here("Data", "DSLL_Data_Bigeye.rds"))

SLA <- brick(here("Data", "cmems_obs-sl_glo_phy-ssh_my_allsat-l4-duacs-0.125deg_P1D_sla_170.06E-244.94E_0.06N-44.94N_2005-01-01-2025-01-01.nc"))

O2 <- brick(here("Data", "cmems_mod_glo_bgc_my_0.25deg_P1D-m_o2_170.00E-245.00E_0.00N-45.00N_300.89m_2005-01-01-2025-01-01.nc"))

# ---------------- Match each set to nearst day in SLA and O2 ------------------

read_brick_dates <- function(brk) {
  z <- raster::getZ(brk)
  if (!is.null(z)) return(as.Date(z))
  nm <- sub("^X", "", names(brk))
  suppressWarnings(as.Date(nm)) %>%
    replace(is.na(.), as.Date(nm, format = "%Y.%m.%d"))
}
sla_dates <- read_brick_dates(SLA)
o2_dates  <- read_brick_dates(O2)

# ----------------- Create date only column to make it easier ------------------

DSLL_Bigeye <- DSLL_Bigeye %>%
  mutate(date_only = as.Date(set_begin_datetime))


# ---------------------- Match set to nearest raster layer ---------------------

to_day <- function(x) as.integer(as.Date(x))
idx_nearest <- function(layer_dates, target_days) {
  ld <- to_day(layer_dates)
  vapply(target_days, function(td) which.min(abs(ld - td)), integer(1))
}
sla_idx <- idx_nearest(sla_dates, to_day(DSLL_Bigeye$date_only))
o2_idx  <- idx_nearest(o2_dates,  to_day(DSLL_Bigeye$date_only))


# --------------------------- Extract ---------------------------------------

coords <- as.matrix(DSLL_Bigeye[, c("cent_lon", "cent_lat")])

extract_by_index <- function(brick_obj, idx_vec, coords_mat) {
  out <- rep(NA_real_, nrow(coords_mat))
  for (i in unique(idx_vec)) {
    rows <- which(idx_vec == i)
    vals <- raster::extract(brick_obj[[i]], coords_mat[rows, , drop = FALSE], method = "bilinear")
    out[rows] <- as.numeric(vals)
  }
  out
}

DSLL_Bigeye$sla <- extract_by_index(SLA, sla_idx, coords)
DSLL_Bigeye$o2  <- extract_by_index(O2,  o2_idx,  coords)

# ----------------------- Remove date only column -----------------------------

DSLL_Bigeye <- DSLL_Bigeye %>%
  select(-date_only)

# -------------------------- Save -----------------------------------------

saveRDS(DSLL_Bigeye, here("Data", "DSLL_Bigeye_O2_SLA.rds"))




