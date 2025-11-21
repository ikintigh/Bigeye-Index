# Purpose: Build the SPDE mesh, which allows sdmTMB to model smooth spatial structure efficiently, using 90% polygon.

# Created by: Isabella Kintigh
# Created on: 11 November 2025

#########################################################################################

# ------------------------- Libraries -------------------------------------------

library(tidyverse)
library(here)
library(sdmTMB)
library(sf)

# ----------------------------- Load in dfs -----------------------------------

DSLL_Bigeye <- readRDS(here("Data", "DSLL_Bigeye_O2_SLA.rds"))
effort_poly <- readRDS(here("Data", "Effort_90pct_Bigeye_DSLL.rds"))

# ----------------------------- Data wrangling ---------------------------------

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

# ---------------------------------- Save ------------------------------------

saveRDS(DSLL_Bigeye_df, here("Data", "DSLL_Bigeye_df.rds"))

# ------------------------------- Make mesh -----------------------------------

mesh <- make_mesh(DSLL_Bigeye_df,
                  xy_cols = c("x", "y"),

# This is the minimum allowed spacing between mesh vertices and controls how dense the mesh is, smaller cutoff means more triangles which means finer detail but a slower model, start with about 50 km
                  cutoff = 0.60, # started at 5 but changing to 60 km

# This sets the largest allowed triangle side lengths with the first value being the typical edge length inside the data-dense area, and the second being the maximum edge length around the outer boundary. The outer triangles can be bigger because we don't need fine detail in empty ocean around polygon.
                  mesh_args = list(max_edge = c(1.2,6))) # inner, outer triangles in degrees

# ------------------------ Check number of vertices ---------------------------

## Maybe look into making mesh more coarse

mesh$mesh$n    # number of mesh vertices

# 905**

# ----------------------- Sanity check ----------------------------------

plot(mesh)
points(DSLL_Bigeye_df$x, DSLL_Bigeye_df$y, pch = 19, cex = 0.2, col = "red")

# --------------------------------- Save --------------------------------------

saveRDS(mesh, here("Data", "mesh_dsll_bigeye.rds"))





