#example code for control sites

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
    site      = factor(site)   # I / N / O
  )

# ---- Q 1 control reefs only ----
dat_ctrl <- oys_dat |> filter(treatment == "control") |> droplevels()

# ---- Fit control reefs site + harvest ----
mod_site_harvest <- brm(
  height_live ~ site + harvest,
  data   = dat_ctrl,
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

# ============================================================
# Output example posterior mean height (mm) by site × harvest
# ============================================================

nd <- expand_grid(
  site    = factor(levels(dat_ctrl$site),    levels = levels(dat_ctrl$site)),
  harvest = factor(levels(dat_ctrl$harvest), levels = levels(dat_ctrl$harvest))
)

# Draw-level population-average fitted values (mm)
draws <- fitted(mod_site_harvest, newdata = nd, re_formula = NA, summary = FALSE)

out_tbl <- as_tibble(draws) |>
  mutate(draw = row_number()) |>
  pivot_longer(-draw, names_to = "row_id", values_to = "height_mm") |>
  mutate(row_id = as.integer(gsub("V", "", row_id))) |>
  left_join(mutate(nd, row_id = row_number()), by = "row_id") |>
  group_by(site, harvest) |>
  summarise(
    mean_mm = mean(height_mm),
    lwr_95  = quantile(height_mm, 0.025),
    upr_95  = quantile(height_mm, 0.975),
    .groups = "drop"
  )

write_csv(out_tbl, "posterior_means_site_harvest_control.csv")
