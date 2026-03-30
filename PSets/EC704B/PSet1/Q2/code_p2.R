########################################################################
# File: code_p2.R
# Author: Atsuki Kotani
# Date: 2026-03-21
# Description: PS1 Problem 2 - Heterogenous Labor Market Flows
########################################################################

### PATHS ---------------------------------------------------------------------
# setwd("/home/atsuki013/projects/Spring2026/task/704_PS7/code")

r_lib <- Sys.getenv("R_LIBS_USER", NA_character_)
if (!is.na(r_lib) && nzchar(r_lib) && dir.exists(r_lib)) {
  .libPaths(c(r_lib, .libPaths()))
}

### LIBRARIES -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ipumsr)
  library(lubridate)
  library(tidyr)
})

source("../../common_scripts/code/figure_setting.R")
source("latex_tables.R")

### DATA ----------------------------------------------------------------------
ddi <- read_ipums_ddi("../input_ps/cps_00003.xml")
cps_data <- read_ipums_micro(ddi)

# variable names
print(names(cps_data))

# sample year
print(unique(cps_data$YEAR))

# sample month
print(unique(cps_data$MONTH))

# employment status
print(unique(cps_data$EMPSTAT))

# educational attainment
print(unique(cps_data$EDUC))

### PREPROCESSING -------------------------------------------------------------
# IPUMS CPS codes (see DDI): EDUC and EMPSTAT are 3- and 2-digit codes stored as
# integers without leading zeros (e.g., 073 -> 73).

cps_data <- cps_data |>
  mutate(
    EDUC_i = as.integer(EDUC),
    EMPSTAT_i = as.integer(EMPSTAT),
    # Five-level attainment (NIU / missing -> NA)
    educ_cat = case_when(
      EDUC_i %in% c(0, 1) ~ NA_character_,
      EDUC_i == 999 ~ NA_character_,
      EDUC_i >= 2 & EDUC_i <= 71 ~ "Less than HS",
      EDUC_i %in% c(72, 73) ~ "HS diploma or GED",
      EDUC_i %in% c(80, 81, 90, 91, 92, 100) ~ "Some college or Associate",
      EDUC_i %in% c(110, 111) ~ "Bachelor's degree",
      EDUC_i %in% c(120, 121, 122, 123, 124, 125) ~ "Graduate degree",
      TRUE ~ NA_character_
    ),
    educ_cat = factor(
      educ_cat,
      levels = c(
        "Less than HS",
        "HS diploma or GED",
        "Some college or Associate",
        "Bachelor's degree",
        "Graduate degree"
      )
    ),
    # Compact four levels (BA+)
    educ_cat4 = case_when(
      educ_cat %in% c("Less than HS", "HS diploma or GED") ~ as.character(educ_cat),
      educ_cat == "Some college or Associate" ~ "Some college or Associate",
      educ_cat %in% c("Bachelor's degree", "Graduate degree") ~ "Bachelor's or more",
      TRUE ~ NA_character_
    ),
    educ_cat4 = factor(
      educ_cat4,
      levels = c(
        "Less than HS",
        "HS diploma or GED",
        "Some college or Associate",
        "Bachelor's or more"
      )
    ),
    # Detailed employment status (civilian-focused analyses often drop Armed Forces)
    empstat_cat = case_when(
      EMPSTAT_i == 0 ~ NA_character_,
      EMPSTAT_i == 1 ~ "Armed Forces",
      EMPSTAT_i %in% c(10, 12) ~ "Employed",
      EMPSTAT_i %in% c(20, 21, 22) ~ "Unemployed",
      EMPSTAT_i %in% c(30, 31, 32, 33, 34, 35, 36) ~ "Not in labor force",
      TRUE ~ NA_character_
    ),
    empstat_cat = factor(
      empstat_cat,
      levels = c(
        "Employed",
        "Unemployed",
        "Not in labor force",
        "Armed Forces"
      )
    ),
    # Civilian labor force vs not (NIU / Armed Forces -> NA)
    in_labor_force = case_when(
      EMPSTAT_i %in% c(10, 12, 20, 21, 22) ~ TRUE,
      EMPSTAT_i %in% c(30, 31, 32, 33, 34, 35, 36) ~ FALSE,
      TRUE ~ NA
    )
  )

# Optional: frequency tables
print(table(cps_data$educ_cat, useNA = "ifany"))
print(table(cps_data$empstat_cat, useNA = "ifany"))

# Education group colors (order matches educ_cat levels)
blues5 <- c("#08306b", "#08519c", "#2171b5", "#6baed6", "#9ecae1")

### 1.1 Unemployment rate trend by group ----------------------------------------
# Civilian labor force only; unweighted share unemployed within each month × group.
analysis <- cps_data |>
  filter(in_labor_force == TRUE, !is.na(educ_cat))

monthly_u <- analysis |>
  group_by(YEAR, MONTH, educ_cat) |>
  summarise(
    unemp_rate = mean(EMPSTAT_i %in% c(20L, 21L, 22L)),
    .groups = "drop"
  ) |>
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", YEAR, MONTH))
  )

p_u <- ggplot(
  monthly_u, aes(x = date, y = unemp_rate * 100, color = educ_cat)
) +
  geom_line(linewidth = 0.35) +
  labs(
    x = NULL,
    y = "Unemployment rate (%)",
    color = "Education"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_color_manual(values = blues5) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
  theme_custom_fig +
  theme(
    aspect.ratio = 0.6,
    legend.position = "bottom",
    legend.justification = c(1, 1),
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.35
    ),
    legend.margin = margin(5, 5, 5, 5),
    legend.key.width = unit(0.9, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 22, color = "black")
  )

fig_path <- "../output/fig_unemployment_by_education.png"
dir.create(dirname(fig_path), showWarnings = FALSE, recursive = TRUE)
ggsave(fig_path, p_u, width = 12, height = 6)

### 1.2 Summary table: average, Great Recession, COVID -------------------------
# Great Recession: Dec 2007 -- Jun 2009 (change = Jun 2009 minus Dec 2007).
# COVID recession: Feb 2020 -- Apr 2020 (change = Apr 2020 minus Feb 2020).

rate_at <- function(df, group_chr, yr, mo) {
  v <- df |>
    filter(
      as.character(.data$educ_cat) == group_chr,
      .data$YEAR == yr,
      .data$MONTH == mo
    ) |>
    pull(.data$unemp_rate)
  if (length(v) == 0L) NA_real_ else as.numeric(v[1L])
}

grp_levels <- levels(droplevels(analysis$educ_cat))

summary_tbl <- lapply(grp_levels, function(g) {
  d <- monthly_u |> filter(as.character(educ_cat) == g)
  tibble::tibble(
    group = g,
    avg_unemp_pct = mean(d$unemp_rate, na.rm = TRUE) * 100,
    gr_change_pp = (rate_at(monthly_u, g, 2009L, 6L) - rate_at(monthly_u, g, 2007L, 12L)) * 100,
    covid_change_pp = (rate_at(monthly_u, g, 2020L, 4L) - rate_at(monthly_u, g, 2020L, 2L)) * 100
  )
}) |>
  bind_rows()

hdr <- paste(
  "Education group",
  "Avg.\\ unemployment (\\%)",
  "$\\Delta$ Great Recession (p.p.)",
  "$\\Delta$ COVID (p.p.)",
  sep = " & "
)
hdr <- paste0(hdr, " \\\\")

body <- sprintf(
  "%s & %s & %s & %s \\\\",
  summary_tbl$group,
  f_num(summary_tbl$avg_unemp_pct),
  f_num(summary_tbl$gr_change_pp),
  f_num(summary_tbl$covid_change_pp)
)

notes <- c(
  paste0(
    "\\multicolumn{4}{p{0.92\\linewidth}}{",
    "\\footnotesize ",
    "Unemployment rates are unweighted CPS monthly sample proportions by education ",
    "(civilian labor force). ",
    "Average is the mean of monthly group rates over the sample. ",
    "Great Recession change is June 2009 minus December 2007. ",
    "COVID change is April 2020 minus February 2020. ",
    "}"
  )
)

tab_lines <- latex_table(
  caption = "Unemployment by education: sample average and recession changes",
  label = "tab:unemployment_education_summary",
  tabular_spec = "lccc",
  body_rows = c(hdr, "\\midrule", body),
  notes = notes
)

tab_path <- "../output/tab_unemployment_summary.tex"
write_latex_table(tab_lines, tab_path)

print(summary_tbl)

### 2. Monthly job finding and job-losing rates (CPSIDP panel) ------------------
# Consecutive calendar months for the same CPSIDP; civilian labor force states
# only at t. Job finding: U -> E; job losing (separation to unemployment): E -> U.
# CPSIDP == 0 (e.g. ASEC oversample) cannot be linked — excluded.

panel_flow <- cps_data |>
  filter(as.numeric(CPSIDP) > 0, !is.na(educ_cat)) |>
  mutate(
    date = as.Date(sprintf("%04d-%02d-01", YEAR, MONTH)),
    lf = case_when(
      EMPSTAT_i %in% c(10L, 12L) ~ "E",
      EMPSTAT_i %in% c(20L, 21L, 22L) ~ "U",
      TRUE ~ NA_character_
    )
  ) |>
  arrange(CPSIDP, date) |>
  group_by(CPSIDP) |>
  mutate(
    next_date = lead(date),
    next_lf = lead(lf)
  ) |>
  ungroup() |>
  filter(
    !is.na(lf),
    !is.na(next_date),
    next_date == date %m+% months(1L)
  )

# keep only columns we need
panel_flow <- panel_flow |>
  select(CPSIDP, date, lf, next_date, next_lf, educ_cat, YEAR, MONTH)

#. head(panel_flow)

monthly_f <- panel_flow |>
  filter(lf == "U") |>
  group_by(YEAR, MONTH, educ_cat) |>
  summarise(
    job_find_rate = sum(next_lf == "E", na.rm = TRUE) / n(),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", YEAR, MONTH)))

#. head(monthly_f)

monthly_s <- panel_flow |>
  filter(lf == "E") |>
  group_by(YEAR, MONTH, educ_cat) |>
  summarise(
    job_loss_rate = sum(next_lf == "U", na.rm = TRUE) / n(),
    .groups = "drop"
  ) |>
  mutate(date = as.Date(sprintf("%04d-%02d-01", YEAR, MONTH)))

#. head(monthly_s)

monthly_flows <- monthly_f |>
  full_join(monthly_s, by = c("YEAR", "MONTH", "educ_cat", "date")) |>
  pivot_longer(
    cols = c(job_find_rate, job_loss_rate),
    names_to = "flow",
    values_to = "rate"
  ) |>
  mutate(
    flow = factor(
      flow,
      levels = c("job_find_rate", "job_loss_rate"),
      labels = c("Job-finding rate (U to E)", "Job-losing rate (E to U)")
    )
  )

#. head(monthly_flows)

p_flow <- ggplot(
  monthly_flows,
  aes(x = date, y = rate * 100, color = educ_cat)
) +
  geom_line(linewidth = 0.35) +
  facet_wrap(~flow, ncol = 1L, scales = "free_y") +
  labs(
    x = NULL,
    y = "Probability (%)",
    color = "Education"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_color_manual(values = blues5) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
  theme_custom_fig +
  theme(
    aspect.ratio = NULL,
    legend.position = "bottom",
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.35
    ),
    legend.margin = margin(5, 5, 5, 5),
    legend.key.width = unit(0.9, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 18, color = "black"),
    strip.text = element_text(size = 20)
  )

fig_flow_path <- "../output/fig_job_flows_by_education.png"
ggsave(fig_flow_path, p_flow, width = 12, height = 10)

### 3. Recover underlying f_m, s_m; time aggregation bias (Problem 1.3) -------
# Observed monthly flows F_m, S_m match the "aggregated" rates in Problem 1.
# Recover continuous-time rates (ps/ps.pdf, Problem 2.3):
#   f_m = -(F_m/(F_m+S_m)) * log(1 - F_m - S_m),
#   s_m = -(S_m/(F_m+S_m)) * log(1 - F_m - S_m),
# defined when 0 < F_m + S_m < 1.
# Bias: |F_m - f_m|, |S_m - s_m|.

rates_agg <- monthly_f |>
  left_join(
    monthly_s |> select(YEAR, MONTH, educ_cat, job_loss_rate),
    by = c("YEAR", "MONTH", "educ_cat")
  ) |>
  mutate(
    F_m = job_find_rate,
    S_m = job_loss_rate,
    sum_fs = F_m + S_m,
    f_m = if_else(
      sum_fs > 0 & sum_fs < 1,
      -(F_m / sum_fs) * log(1 - sum_fs),
      NA_real_
    ),
    s_m = if_else(
      sum_fs > 0 & sum_fs < 1,
      -(S_m / sum_fs) * log(1 - sum_fs),
      NA_real_
    ),
    abs_bias_F = abs(F_m - f_m),
    abs_bias_S = abs(S_m - s_m)
  )

rates_full <- rates_agg |>
  left_join(
    monthly_u |> select(YEAR, MONTH, educ_cat, unemp_rate),
    by = c("YEAR", "MONTH", "educ_cat")
  ) |>
  mutate(
    # Steady-state unemployment from flow rates: u = s / (s + f)
    u_eq = if_else(
      !is.na(s_m) & !is.na(f_m) & (s_m + f_m) > 0,
      s_m / (s_m + f_m),
      NA_real_
    ),
    y_eq = if_else(
      !is.na(u_eq) & u_eq > 0 & u_eq < 1,
      log(u_eq / (1 - u_eq)),
      NA_real_
    )
  )

underlying_long <- rates_agg |>
  select(YEAR, MONTH, educ_cat, date, f_m, s_m) |>
  pivot_longer(
    cols = c(f_m, s_m),
    names_to = "series",
    values_to = "rate"
  ) |>
  mutate(
    series = factor(
      series,
      levels = c("f_m", "s_m"),
      labels = c(
        "Underlying job-finding rate (f_m)",
        "Underlying separation rate (s_m)"
      )
    )
  )

p_under <- ggplot(
  underlying_long,
  aes(x = date, y = rate * 100, color = educ_cat)
) +
  geom_line(linewidth = 0.35) +
  facet_wrap(~series, ncol = 1L, scales = "free_y") +
  labs(
    x = NULL,
    y = "Rate (%)",
    color = "Education"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_color_manual(values = blues5) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
  theme_custom_fig +
  theme(
    aspect.ratio = NULL,
    legend.position = "bottom",
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.35
    ),
    legend.margin = margin(5, 5, 5, 5),
    legend.key.width = unit(0.9, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 18, color = "black"),
    strip.text = element_text(size = 16)
  )

fig_under_path <- "../output/fig_underlying_fs_rates.png"
ggsave(fig_under_path, p_under, width = 12, height = 10)

bias_long <- rates_agg |>
  select(YEAR, MONTH, educ_cat, date, abs_bias_F, abs_bias_S) |>
  pivot_longer(
    cols = c(abs_bias_F, abs_bias_S),
    names_to = "series",
    values_to = "bias"
  ) |>
  mutate(
    series = factor(
      series,
      levels = c("abs_bias_F", "abs_bias_S"),
      labels = c(
        "Time aggregation bias: |F_m - f_m|",
        "Time aggregation bias: |S_m - s_m|"
      )
    )
  )

p_bias <- ggplot(
  bias_long,
  aes(x = date, y = bias * 100, color = educ_cat)
) +
  geom_line(linewidth = 0.35) +
  facet_wrap(~series, ncol = 1L, scales = "free_y") +
  labs(
    x = NULL,
    y = "Bias (p.p.)",
    color = "Education"
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  scale_color_manual(values = blues5) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE)) +
  theme_custom_fig +
  theme(
    aspect.ratio = NULL,
    legend.position = "bottom",
    legend.background = element_rect(
      fill = "white",
      color = "black",
      linewidth = 0.35
    ),
    legend.margin = margin(5, 5, 5, 5),
    legend.key.width = unit(0.9, "cm"),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 18, color = "black"),
    strip.text = element_text(size = 14)
  )

fig_bias_path <- "../output/fig_time_aggregation_bias.png"
ggsave(fig_bias_path, p_bias, width = 12, height = 10)

print(
  rates_agg |>
    group_by(educ_cat) |>
    summarise(
      mean_abs_bias_F = mean(abs_bias_F, na.rm = TRUE),
      mean_abs_bias_S = mean(abs_bias_S, na.rm = TRUE),
      max_abs_bias_F = max(abs_bias_F, na.rm = TRUE),
      max_abs_bias_S = max(abs_bias_S, na.rm = TRUE),
      .groups = "drop"
    )
)

### 4. Cross-sectional variance decomposition (Problem 2.4) ----------------------
# Varg(X) = (1/G) sum_g (X_g - Xbar)^2 (same-time cross section).
# Equilibrium unemployment u_g^eq = s_m / (s_m + f_m); Y_g = log(u^eq/(1-u^eq)) = log s - log f.
# From ps/ps.pdf: Var_g(Y) = Cov_g(Y, log s_g) + Cov_g(Y, -log f_g). Shares: Cov / Var when Var > 0.

cross_sectional_decomp <- function(y, log_s, log_f) {
  ok <- is.finite(y) & is.finite(log_s) & is.finite(log_f)
  y <- y[ok]
  log_s <- log_s[ok]
  log_f <- log_f[ok]
  G <- length(y)
  if (G < 2L) {
    return(list(
      G = G, v_y = NA_real_, cov_log_s = NA_real_, cov_neg_log_f = NA_real_,
      share_s = NA_real_, share_f = NA_real_
    ))
  }
  y_bar <- mean(y)
  ls_bar <- mean(log_s)
  lf_bar <- mean(log_f)
  v_y <- mean((y - y_bar)^2)
  cov_log_s <- mean((y - y_bar) * (log_s - ls_bar))
  cov_neg_log_f <- mean((y - y_bar) * (-(log_f - lf_bar)))
  list(
    G = G,
    v_y = v_y,
    cov_log_s = cov_log_s,
    cov_neg_log_f = cov_neg_log_f,
    share_s = if (v_y > 0) cov_log_s / v_y else NA_real_,
    share_f = if (v_y > 0) cov_neg_log_f / v_y else NA_real_
  )
}

fn4 <- function(x) {
  sprintf("%.4f", round(x, 4L))
}

# (a) Monthly cross sections: average Var and Cov terms over time
monthly_decomp <- rates_full |>
  filter(
    !is.na(y_eq),
    !is.na(f_m), !is.na(s_m), f_m > 0, s_m > 0
  ) |>
  mutate(
    y = y_eq,
    log_s = log(s_m),
    log_f = log(f_m)
  ) |>
  group_by(YEAR, MONTH) |>
  group_modify(~ {
    r <- cross_sectional_decomp(.x$y, .x$log_s, .x$log_f)
    tibble::tibble(
      v_y = r$v_y,
      cov_s = r$cov_log_s,
      cov_f = r$cov_neg_log_f,
      share_s = r$share_s,
      share_f = r$share_f
    )
  }) |>
  filter(!is.na(v_y))

decomp_a <- monthly_decomp |>
  ungroup() |>
  summarise(
    v_y = mean(v_y, na.rm = TRUE),
    cov_log_s = mean(cov_s, na.rm = TRUE),
    cov_neg_log_f = mean(cov_f, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    share_s = cov_log_s / v_y,
    share_f = cov_neg_log_f / v_y
  )

# (b) Great Recession: Delta log odds; Delta log s, Delta log f across groups
snap_fs <- function(yr, mo) {
  rates_full |>
    filter(.data$YEAR == yr, .data$MONTH == mo) |>
    select("educ_cat", "unemp_rate", "u_eq", "f_m", "s_m")
}

delta_gr <- snap_fs(2007L, 12L) |>
  inner_join(snap_fs(2009L, 6L), by = "educ_cat", suffix = c("_0", "_1")) |>
  filter(
    u_eq_0 > 0, u_eq_0 < 1,
    u_eq_1 > 0, u_eq_1 < 1,
    !is.na(f_m_0), !is.na(s_m_0), f_m_0 > 0, s_m_0 > 0,
    !is.na(f_m_1), !is.na(s_m_1), f_m_1 > 0, s_m_1 > 0
  ) |>
  mutate(
    Dy = log(u_eq_1 / (1 - u_eq_1)) - log(u_eq_0 / (1 - u_eq_0)),
    Dlog_s = log(s_m_1) - log(s_m_0),
    Dlog_f = log(f_m_1) - log(f_m_0)
  )

r_b <- cross_sectional_decomp(delta_gr$Dy, delta_gr$Dlog_s, delta_gr$Dlog_f)
decomp_b <- tibble::tibble(
  v_y = r_b$v_y,
  cov_log_s = r_b$cov_log_s,
  cov_neg_log_f = r_b$cov_neg_log_f,
  share_s = r_b$share_s,
  share_f = r_b$share_f
)

# (c) COVID window
delta_cov <- snap_fs(2020L, 2L) |>
  inner_join(snap_fs(2020L, 4L), by = "educ_cat", suffix = c("_0", "_1")) |>
  filter(
    u_eq_0 > 0, u_eq_0 < 1,
    u_eq_1 > 0, u_eq_1 < 1,
    !is.na(f_m_0), !is.na(s_m_0), f_m_0 > 0, s_m_0 > 0,
    !is.na(f_m_1), !is.na(s_m_1), f_m_1 > 0, s_m_1 > 0
  ) |>
  mutate(
    Dy = log(u_eq_1 / (1 - u_eq_1)) - log(u_eq_0 / (1 - u_eq_0)),
    Dlog_s = log(s_m_1) - log(s_m_0),
    Dlog_f = log(f_m_1) - log(f_m_0)
  )

r_c <- cross_sectional_decomp(delta_cov$Dy, delta_cov$Dlog_s, delta_cov$Dlog_f)
decomp_c <- tibble::tibble(
  v_y = r_c$v_y,
  cov_log_s = r_c$cov_log_s,
  cov_neg_log_f = r_c$cov_neg_log_f,
  share_s = r_c$share_s,
  share_f = r_c$share_f
)

decomp_tbl <- bind_rows(
  decomp_a |> mutate(case = "(a) Avg.\\ monthly cross section"),
  decomp_b |> mutate(case = "(b) $\\Delta$ Great Recession"),
  decomp_c |> mutate(case = "(c) $\\Delta$ COVID (Feb--Apr 2020)")
) |>
  select(case, v_y, cov_log_s, cov_neg_log_f, share_s, share_f)

hdr_dec <- paste(
  "Case",
  "$\\mathrm{Var}_g$",
  "$\\mathrm{Cov}_g(\\cdot,\\log s)$",
  "$\\mathrm{Cov}_g(\\cdot,-\\log f)$",
  "Share $\\log s$",
  "Share $-\\log f$",
  sep = " & "
)
hdr_dec <- paste0(hdr_dec, " \\\\")

body_dec <- apply(decomp_tbl, 1L, function(row) {
  paste0(
    row[["case"]], " & ",
    fn4(as.numeric(row[["v_y"]])), " & ",
    fn4(as.numeric(row[["cov_log_s"]])), " & ",
    fn4(as.numeric(row[["cov_neg_log_f"]])), " & ",
    fn4(as.numeric(row[["share_s"]])), " & ",
    fn4(as.numeric(row[["share_f"]])), " \\\\"
  )
})

notes_dec <- c(
  paste0(
    "\\multicolumn{6}{p{0.95\\linewidth}}{",
    "\\footnotesize ",
    "$G=5$ education groups. ",
    "Moments use $\\mathrm{Var}_g(X)=G^{-1}\\sum_g (X_g-\\bar{X})^2$ and ",
    "$\\mathrm{Cov}_g(X,Z)=G^{-1}\\sum_g (X_g-\\bar{X})(Z_g-\\bar{Z})$. ",
    "Equilibrium unemployment $u_g^{\\mathrm{eq}}=s_m/(s_m+f_m)$; ",
    "$Y_g=\\log(u_g^{\\mathrm{eq}}/(1-u_g^{\\mathrm{eq}}))=\\log s_g-\\log f_g$. ",
    "$\\log s_g$, $\\log f_g$ are underlying rates from Section~3. ",
    "Shares are $\\mathrm{Cov}_g/\\mathrm{Var}_g$. ",
    "For (b)--(c), outcomes are $\\Delta\\log(u^{\\mathrm{eq}}/(1-u^{\\mathrm{eq}}))$ and ",
    "covariates $\\Delta\\log s_g$, $\\Delta\\log f_g$ between the two dates. ",
    "}"
  )
)

tab_dec_lines <- latex_table(
  caption = paste(
    "Decomposition of cross-sectional variance of log odds of equilibrium unemployment",
    "($u=s/(s+f)$)"
  ),
  label = "tab:var_decomp_cross_section",
  tabular_spec = "lccccc",
  body_rows = c(hdr_dec, "\\midrule", body_dec),
  notes = notes_dec
)

write_latex_table(tab_dec_lines, "../output/tab_variance_decomposition.tex")

print(decomp_tbl)