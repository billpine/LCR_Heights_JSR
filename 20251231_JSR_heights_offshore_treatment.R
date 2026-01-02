############################################################
# Example offshore restoration fit (Site = O)
# Fits: height_live ~ treatment + harvest
############################################################

library(brms)
library(dplyr)
library(tidyr)
library(readr)

set.seed(123)
options(brms.backend = "cmdstanr", mc.cores = 4)

# ---- Data ----
oys_dat <- read.csv("oys_heights_data.csv") |>
  filter(!is.na(height_live), height_live > 0) |>
  mutate(
    treatment = factor(treatment, levels = c("control","rocks")),
    harvest   = factor(
      ifelse(strata %in% c("N_NA","N_LG","N_PILOT","N_SM"), "unharv","harv"),
      levels = c("unharv","harv")
    ),
    site = factor(site)  # I / N / O
  )

# ---- Offshore subset (Site O) ----
dat_off <- oys_dat |>
  filter(site == "O") |>
  droplevels()

# ---- Fit offshore model (restoration + harvest) ----
mod_off_treat_harv <- brm(
  height_live ~ treatment + harvest,
  data   = dat_off,
  family = Gamma(link = "log"),
  prior  = c(
    prior(normal(0,1), class = "b"),
    prior(gamma(1,1),  class = "shape")
  ),
  chains  = 4,
  iter    = 4000,
  warmup  = 1000,
  control = list(adapt_delta = 0.95),
  seed    = 123
)

# save if you want
saveRDS(mod_off_treat_harv, "mod_offshore_treatment_harvest.rds")

# ============================================================
# Output example posterior mean height (mm) by treatment × harvest
# these are population-average fitted values; 95% credible intervals
# ============================================================
nd <- expand_grid(
  treatment = factor(levels(dat_off$treatment), levels = levels(dat_off$treatment)),
  harvest   = factor(levels(dat_off$harvest),   levels = levels(dat_off$harvest))
)

draws <- fitted(mod_off_treat_harv, newdata = nd, re_formula = NA, summary = FALSE)

out_tbl <- as_tibble(draws) |>
  mutate(draw = row_number()) |>
  pivot_longer(-draw, names_to = "row_id", values_to = "height_mm") |>
  mutate(row_id = as.integer(gsub("V", "", row_id))) |>
  left_join(mutate(nd, row_id = row_number()), by = "row_id") |>
  group_by(treatment, harvest) |>
  summarise(
    mean_mm = mean(height_mm),
    lwr_95  = quantile(height_mm, 0.025),
    upr_95  = quantile(height_mm, 0.975),
    .groups = "drop"
  ) |>
  mutate(
    treatment_lab = if_else(treatment == "control", "Control", "Restored"),
    harvest_lab   = if_else(harvest == "unharv", "Closed", "Open"),
    Group         = paste(treatment_lab, harvest_lab, sep = " / ")
  ) |>
  select(Group, treatment, harvest, mean_mm, lwr_95, upr_95)

write_csv(out_tbl, "posterior_means_offshore_treatment_harvest.csv")
