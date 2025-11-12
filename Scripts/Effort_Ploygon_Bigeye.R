# Purpose: Define where the model should estimate spatial and spatiotemporal random effects. Keep the model focused only on the true fishing grounds, not the entire Pacific grid.

# Created by: Isabella Kintigh
# Created on: 10 November 2025

################################################################################

# --------------------- Libraries ------------------------------------

library(tidyverse)
library(sf)
library(here)

# ------------------------- Dfs ----------------------------------------

DSLL_Bigeye <- readRDS(here("Data", "DSLL_Bigeye_O2_SLA.rds"))

# ------------------ Create 90% Effort Polygon for Mesh ------------------------

# Aggregate effort spatially (5x5° binning)

Bigeye_bins <- DSLL_Bigeye %>%
  mutate(
    lat_bin = floor(cent_lat / 5) * 5,
    lon_bin = floor(cent_lon / 5) * 5
  ) %>%
  group_by(lat_bin, lon_bin) %>%
  summarise(n_sets = n(), .groups = "drop")

# Identify bins that make up 90% of total effort

Bigeye_bins <- Bigeye_bins %>%
  arrange(desc(n_sets)) %>%
  mutate(cumprop = cumsum(n_sets) / sum(n_sets)) %>%
  filter(cumprop <= 0.9)

# Convert to sf and make polygon

Bigeye_poly <- Bigeye_bins %>%
  st_as_sf(coords = c("lon_bin", "lat_bin"), crs = NA) %>%   # no CRS yet
  st_buffer(dist = 2.5) %>%
  st_union() %>%
  st_convex_hull() %>%
  st_set_crs(4326) # Make sure it is in 0-360 coordinate system


# Save for reuse 

saveRDS(Bigeye_poly, here("Data", "Effort_90pct_Bigeye_DSLL.rds"))

# -------------------- Sanity Check -----------------------------------

ggplot() +
  geom_sf(data = Bigeye_poly, fill = "skyblue", alpha = 0.3) +
  geom_point(data = DSLL_Bigeye, aes(x = cent_lon, y = cent_lat),
             color = "red", size = 0.4, alpha = 0.5) +
  coord_sf(xlim = c(170, 245), ylim = c(0, 40)) +
  labs(title = "Bigeye 90% Effort Polygon (0–360°)",
       subtitle = "Polygon (blue) encloses core DSLL Bigeye sets (red)")





