  ################################################################################
  # PS1 Q2: Heterogeneous Labor Market Flows by Education
  # Dimension: Education (5 groups)
  #   1. HS dropout        (EDUC < 073)
  #   2. HS graduate       (EDUC 073)
  #   3. Some college      (EDUC 080-100)
  #   4. College completed (EDUC 110-111)
  #   5. Post graduate     (EDUC 120-125)
  #
  # Data: IPUMS CPS monthly, 2000-2025(ish), fixed-width .dat file
  # Linking: CPSIDV for consecutive-month pairs only
  ################################################################################

  library(data.table)
  library(ggplot2)
  library(ipumsr)


  # ==============================================================================
  # 0. READ DATA
  # ==============================================================================

  ddi <- read_ipums_ddi("Data/cps_00007.xml")
  dt <- read_ipums_micro(ddi)
  dt <- as.data.table(dt)


  cat("Raw rows read:", nrow(dt), "\n")

  table(dt$YEAR)

  # Restrict to civilian non-institutional population, age 16+
  # EMPSTAT: 0 = NIU (children/institutions), 1 = Armed Forces
  dt <- dt[AGE >= 16 & !(EMPSTAT %in% c(0, 1))]

  # Create integer year-month key (fast for grouping/merging)
  dt[, ym := YEAR * 100L + MONTH]

  # Helper: convert ym integer to Date (for plotting only)
  ym_to_date <- function(ym) as.IDate(sprintf("%04d-%02d-01", ym %/% 100L, ym %% 100L))

  cat("Rows after filtering:", nrow(dt), "\n")

  sample(dt$ym, 100)
  # ==============================================================================
  # 1. DEFINE EDUCATION GROUPS
  # ==============================================================================
  # EDUC codes (from codebook):
  #   000-002: NIU / none / preschool
  #   010-060: Grades 1-11
  #   070-072: Grade 12 no diploma / unclear
  #   073: HS diploma or equivalent
  #   080-081: Some college no degree
  #   090-092: Associate's degree
  #   100: 3 years college
  #   110-111: 4 years / Bachelor's
  #   120-125: 5+ years / Master's / Professional / Doctorate
  #   999: Missing

  dt[, educ_group := fcase(
    EDUC <= 72,                    "HS dropout",
    EDUC == 73,                    "HS graduate",
    EDUC >= 80 & EDUC <= 100,     "Some college",
    EDUC >= 110 & EDUC <= 111,    "College completed",
    EDUC >= 120 & EDUC <= 125,    "Post graduate",
    default = NA_character_
  )]

  # Drop missing education and EDUC == 0/999
  dt <- dt[!is.na(educ_group)]

  # Ordered factor for plotting
  dt[, educ_group := factor(educ_group,
    levels = c("HS dropout", "HS graduate", "Some college", "College completed", "Post graduate"))]

  # ==============================================================================
  # 2. LABOR FORCE STATUS
  # ==============================================================================
  # EMPSTAT: 10, 12 = Employed; 20, 21, 22 = Unemployed; 30+ = NILF; 0,1 = NIU/Armed Forces
  # LABFORCE: 0 = NIU, 1 = NILF, 2 = In LF
  dt[, emp_status := fcase(
    EMPSTAT %in% c(10, 12),       "E",
    EMPSTAT %in% c(20, 21, 22),   "U",
    LABFORCE == 1,                 "N",   # NILF
    default = NA_character_
  )]

  # Drop armed forces / NIU
  dt <- dt[!is.na(emp_status)]

  cat("Final analysis rows:", nrow(dt), "\n")

  # ==============================================================================
  # DIAGNOSTIC PLOT 1: Total sample size per month
  # ==============================================================================
  diag_sample <- dt[, .(n_total = .N), by = ym]

  p_sample <- ggplot(diag_sample, aes(x = ym_to_date(ym), y = n_total)) +
    geom_col(alpha = 0.6) +
    labs(title = "Total Sample Size per Month (age 16+, non-Armed Forces)",
        x = "Month", y = "Count") +
    theme_minimal()
  ggsave("Figures/plot_diag_sample_size.pdf", p_sample, width = 10, height = 5)

  # ==============================================================================
  # DIAGNOSTIC PLOT 2: Total LFP count per month
  # ==============================================================================
  diag_lfp <- dt[emp_status %in% c("E", "U"), .(n_lf = .N), by = ym]

  p_lfp <- ggplot(diag_lfp, aes(x = ym_to_date(ym), y = n_lf)) +
    geom_col(alpha = 0.6) +
    labs(title = "Total Labor Force Participants per Month",
        x = "Month", y = "Count") +
    theme_minimal()
  ggsave("Figures/plot_diag_lfp_count.pdf", p_lfp, width = 10, height = 5)

  # ==============================================================================
  # Q2.1: UNEMPLOYMENT RATE BY EDUCATION GROUP OVER TIME
  # ==============================================================================
ur_group <- dt[emp_status %in% c("E", "U"),
  .(urate = weighted.mean(emp_status == "U", w = WTFINL), n_indiv = .N),
  by = .(ym, educ_group)]



  # Also compute aggregate
  ur_agg <- dt[emp_status %in% c("E", "U"),
    .(urate = weighted.mean(emp_status == "U", w = WTFINL)),
    by = ym]

  # DIAGNOSTIC PLOT 3: Aggregate unemployment rate
  p_ur_agg <- ggplot(ur_agg, aes(x = ym_to_date(ym), y = urate)) +
    geom_col(alpha = 0.6) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Aggregate Unemployment Rate Over Time",
        x = "Month", y = "Unemployment Rate") +
    theme_minimal()
  ggsave("Figures/plot_diag_urate_aggregate.pdf", p_ur_agg, width = 10, height = 5)

  # Main plot: by group
  p_ur <- ggplot(ur_group, aes(x = ym_to_date(ym), y = urate, color = educ_group)) +
    geom_line(linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Unemployment Rate by Education Group",
        x = "Month", y = "Unemployment Rate", color = "Education") +
    theme_minimal() +
    theme(legend.position = "bottom")
  ggsave("Figures/plot_q2_1_urate_by_educ.pdf", p_ur, width = 10, height = 5)

  # ---------- Summary statistics ----------
  # (a) Average unemployment rate over sample period
  avg_ur <- ur_group[, .(avg_urate = mean(urate), avg_n_indiv = mean(n_indiv)), by = educ_group]
  cat("\n=== (a) Average unemployment rate ===\n")
  print(avg_ur)

  # (b) Change during Great Recession (Dec 2007 to Jun 2009)
  gr_start <- 200712L
  gr_end   <- 200906L
  ur_gr <- ur_group[ym %in% c(gr_start, gr_end)]
  ur_gr_wide <- dcast(ur_gr, educ_group ~ ym, value.var = "urate")
  names(ur_gr_wide)[2:3] <- c("ur_start", "ur_end")
  ur_gr_n <- dcast(ur_gr, educ_group ~ ym, value.var = "n_indiv")
  names(ur_gr_n)[2:3] <- c("n_start", "n_end")
  ur_gr_wide <- merge(ur_gr_wide, ur_gr_n, by = "educ_group")
  ur_gr_wide[, delta_gr := ur_end - ur_start]
  cat("\n=== (b) Change during Great Recession ===\n")
  print(ur_gr_wide)

  # (c) Change during COVID recession (Feb 2020 to Apr 2020)
  covid_start <- 202002L
  covid_end   <- 202004L
  ur_covid <- ur_group[ym %in% c(covid_start, covid_end)]
  ur_covid_wide <- dcast(ur_covid, educ_group ~ ym, value.var = "urate")
  names(ur_covid_wide)[2:3] <- c("ur_start", "ur_end")
  ur_covid_n <- dcast(ur_covid, educ_group ~ ym, value.var = "n_indiv")
  names(ur_covid_n)[2:3] <- c("n_start", "n_end")
  ur_covid_wide <- merge(ur_covid_wide, ur_covid_n, by = "educ_group")
  ur_covid_wide[, delta_covid := ur_end - ur_start]
  cat("\n=== (c) Change during COVID Recession ===\n")
  print(ur_covid_wide)

  # ==============================================================================
  # Q2.2: MONTHLY JOB-FINDING AND JOB-LOSING PROBABILITIES (F_m, S_m)
  #        Link individuals in CONSECUTIVE months only using CPSIDV
  # ==============================================================================
  # CPSIDV == "000000000000000" means unlinkable (oversample or failed validation)
  # We only link month m to month m+1.

  cat("\nBuilding consecutive-month linked pairs...\n")

  # Create a "next month" integer ym for merging (vectorised, no per-row paste)
  dt[, ym_next := (YEAR + (MONTH == 12L)) * 100L + (MONTH %% 12L + 1L)]

  # Keep only rows with valid CPSIDV (nonzero)
  dt_link <- dt[CPSIDV != "000000000000000" & trimws(CPSIDV) != "0"]

  # Self-merge: match person in month m with same person in month m+1
  # Key: CPSIDV, ym
  setkey(dt_link, CPSIDV, ym)

  # For each row, find the same CPSIDV in the next month
  # Efficient approach: merge dt_link with itself shifted
  pairs <- merge(
    dt_link[, .(CPSIDV, ym, educ_group, emp_status, WTFINL, ym_next)],
    dt_link[, .(CPSIDV, ym, educ_group_next = educ_group,
                emp_status_next = emp_status, WTFINL_next = WTFINL)],
    by.x = c("CPSIDV", "ym_next"),
    by.y = c("CPSIDV", "ym"),
    allow.cartesian = FALSE
  )

  cat("Linked pairs:", nrow(pairs), "\n")

  # DIAGNOSTIC PLOT 4: Number of linked individuals per consecutive-month pair
  diag_links <- pairs[, .(n_linked = .N), by = ym]

  p_links <- ggplot(diag_links, aes(x = ym_to_date(ym), y = n_linked)) +
    geom_col(alpha = 0.6) +
    labs(title = "Number of Linked Individuals per Consecutive Month-Pair",
        x = "Month (t)", y = "Count of linked persons") +
    theme_minimal()
  ggsave("Figures/plot_diag_linked_count.pdf", p_links, width = 10, height = 5)

  # ---------- Compute F_m and S_m by group ----------
  # F_m = P(employed at t+1 | unemployed at t) among LF participants
  # S_m = P(unemployed at t+1 | employed at t) among LF participants
  # Restrict to people in LF in both periods
  pairs_lf <- pairs[emp_status %in% c("E", "U") & emp_status_next %in% c("E", "U")]

  # Job-finding: U -> E
  Fm <- pairs_lf[emp_status == "U",
    .(Fm = weighted.mean(emp_status_next == "E", w = WTFINL)),
    by = .(ym, educ_group)]

  # Job-losing: E -> U
  Sm <- pairs_lf[emp_status == "E",
    .(Sm = weighted.mean(emp_status_next == "U", w = WTFINL)),
    by = .(ym, educ_group)]

  # Plot F_m
  p_Fm <- ggplot(Fm, aes(x = ym_to_date(ym), y = Fm, color = educ_group)) +
    geom_line(linewidth = 0.4) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Monthly Job-Finding Probability (F_m) by Education",
        x = "Month", y = "F_m", color = "Education") +
    theme_minimal() + theme(legend.position = "bottom")

  print(p_Fm)

  ggsave("Figures/plot_q2_2_Fm_by_educ.pdf", p_Fm, width = 10, height = 5)

  # Plot S_m
  p_Sm <- ggplot(Sm, aes(x = ym_to_date(ym), y = Sm, color = educ_group)) +
    geom_line(linewidth = 0.4) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Monthly Job-Losing Probability (S_m) by Education",
        x = "Month", y = "S_m", color = "Education") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave("Figures/plot_q2_2_Sm_by_educ.pdf", p_Sm, width = 10, height = 5)

  # ==============================================================================
  # Q2.3: RECOVER UNDERLYING RATES f_m, s_m (TIME AGGREGATION CORRECTION)
  # ==============================================================================
  # From Q1.2, the solution to the ODE gives:
  #   F_m = 1 - [ s_m + f_m * exp(-(f_m + s_m)) ] / (f_m + s_m)
  #   S_m = 1 - [ f_m + s_m * exp(-(f_m + s_m)) ] / (f_m + s_m)
  #
  # Equivalently (Shimer 2012 approach):
  #   Let lambda = f_m + s_m.  Then:
  #   F_m = (f_m / lambda) * (1 - exp(-lambda))
  #   S_m = (s_m / lambda) * (1 - exp(-lambda))
  #
  # From these: F_m + S_m = 1 - exp(-lambda), so
  #   lambda = -log(1 - F_m - S_m)
  #   f_m = lambda * F_m / (F_m + S_m)
  #   s_m = lambda * S_m / (F_m + S_m)

  rates <- merge(Fm, Sm, by = c("ym", "educ_group"), all = TRUE)

  # Some months may have tiny samples => NA. Drop those.
  rates <- rates[!is.na(Fm) & !is.na(Sm)]

  # Guard against F_m + S_m >= 1 (would make log undefined)
  rates[, Fm_plus_Sm := Fm + Sm]
  rates[Fm_plus_Sm >= 0.999, Fm_plus_Sm := 0.999]

  rates[, lambda := -log(1 - Fm_plus_Sm)]
  rates[, fm := lambda * Fm / Fm_plus_Sm]
  rates[, sm := lambda * Sm / Fm_plus_Sm]

  # Time aggregation bias (signed: continuous - discrete)
  rates[, bias_f := fm - Fm]
  rates[, bias_s := sm - Sm]

  cat("\n=== Time aggregation bias summary ===\n")
  cat("Mean (f_m - F_m):", mean(rates$bias_f, na.rm = TRUE), "\n")
  cat("Mean (s_m - S_m):", mean(rates$bias_s, na.rm = TRUE), "\n")
  cat("Mean F_m:", mean(rates$Fm, na.rm = TRUE), "\n")
  cat("Mean S_m:", mean(rates$Sm, na.rm = TRUE), "\n")
  cat("Mean f_m:", mean(rates$fm, na.rm = TRUE), "\n")
  cat("Mean s_m:", mean(rates$sm, na.rm = TRUE), "\n")

  # Plot f_m
  p_fm <- ggplot(rates, aes(x = ym_to_date(ym), y = fm, color = educ_group)) +
    geom_line(linewidth = 0.4) +
    labs(title = "Continuous-Time Job-Finding Rate (f_m) by Education",
        x = "Month", y = "f_m", color = "Education") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave("Figures/plot_q2_3_fm_by_educ.pdf", p_fm, width = 10, height = 5)

  # Plot s_m
  p_sm <- ggplot(rates, aes(x = ym_to_date(ym), y = sm, color = educ_group)) +
    geom_line(linewidth = 0.4) +
    labs(title = "Continuous-Time Job-Losing Rate (s_m) by Education",
        x = "Month", y = "s_m", color = "Education") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave("Figures/plot_q2_3_sm_by_educ.pdf", p_sm, width = 10, height = 5)

  # Bias plot: f_m - F_m by education group
  p_bias_f <- ggplot(rates, aes(x = ym_to_date(ym), y = bias_f, color = educ_group)) +
    geom_line(linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = "Time Aggregation Bias in Job-Finding Rate (f_m - F_m)",
        x = "Month", y = "f_m - F_m", color = "Education") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave("Figures/plot_q2_3_bias_f.pdf", p_bias_f, width = 10, height = 5)

  # Bias plot: s_m - S_m by education group
  p_bias_s <- ggplot(rates, aes(x = ym_to_date(ym), y = bias_s, color = educ_group)) +
    geom_line(linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    labs(title = "Time Aggregation Bias in Job-Losing Rate (s_m - S_m)",
        x = "Month", y = "s_m - S_m", color = "Education") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave("Figures/plot_q2_3_bias_s.pdf", p_bias_s, width = 10, height = 5)

  cat("\nDone. All plots saved as PDF.\n")

