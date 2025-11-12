# Purpose: Create a correlation matrix to check which (if any) environmental and gear variables are correlated.

# Created by: Isabella Kintigh
# Created on: 27 October 2025

##############################################################################################

# --------------------------- Libraries --------------------------------

library(tidyverse)
library(here)
library(stringr)

# ------------------------------ Dfs -----------------------------------

DSLL_Data_Bigeye <- readRDS(here("Data", "DSLL_Data_Bigeye.rds"))

# ---------------------- Pick wanted variables -------------------------

vars_for_corr <- c("sss", 
                   "sst", 
                   "ssh", 
                   "mld", 
                   "v", 
                   "rugosity", 
                   "sss_sd", 
                   "sst_sd", 
                   "u", 
                   "lunar_rad", 
                   "chl_cop",
                   "ssh_sd",
                   "current_speed")

dat_num <- DSLL_Data_Bigeye %>%
  select(all_of(vars_for_corr)) %>%
  mutate(across(everything(), as.numeric))  # in case any are factors


# ----------------- Correlation matrix (pairwise complete obs) ------------------

Corr_matrix <- cor(dat_num, use = "pairwise.complete.obs", method = "pearson")

# --------------------- Prep long data for ggplot ------------------------------

var_levels <- vars_for_corr

df <- as.data.frame(Corr_matrix) %>%
  tibble::rownames_to_column("Var1") %>%
  tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "r") %>%
  mutate(
    Var1 = factor(Var1, levels = var_levels),
    Var2 = factor(Var2, levels = var_levels),
    lab  = sprintf("%.2f", r)
  )

# -------------------------- Plot -------------------------------------

Matrix <- ggplot(df, aes(x = Var2, y = Var1, fill = r)) +
  geom_tile(color = NA) +
  geom_text(aes(label = lab), size = 3) +
  scale_fill_gradient2(
    name = "Pearson\nCorrelation",
    limits = c(-1, 1),
    midpoint = 0,
    low = "#3b4cc0", mid = "white", high = "#b40426"
  ) +
  coord_fixed() +
  labs(title = "Correlation Matrix for Bigeye", x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

Matrix

# ------------------------------- Save ---------------------------------

ggsave(here("Output", "Bigeye_Correlation_Matrix.png"),
       Matrix,
       width = 8, 
       height = 8, 
       dpi = 600)









