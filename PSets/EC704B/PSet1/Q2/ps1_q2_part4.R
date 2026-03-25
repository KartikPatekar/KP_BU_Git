################################################################################
# PS1 Q2.4: Decomposition of Steady-State Unemployment Fluctuations
# Requires: dt, rates, ym_to_date (from ps1_q2.R)
# Variance/covariance taken ACROSS 5 education groups at each time t,
# weighted by group population (sum of person weights).
# Uses discrete-time probabilities F_m, S_m.
################################################################################

# ---------- 1. Compute u_hat and v at each (t, educ_group) using F_m, S_m ----------
rates[, u_hat := Sm / (Sm + Fm)]
rates[, v     := u_hat / (1 - u_hat)]   # equivalently Sm / Fm
rates[, ln_v  := log(v)]
rates[, ln_S  := log(Sm)]
rates[, ln_F  := log(Fm)]

# ---------- 2. Group population weights (sum of WTFINL per group-month) ----------
grp_wt <- dt[emp_status %in% c("E", "U"),
  .(pop_wt = sum(WTFINL)),
  by = .(ym, educ_group)]

rates <- merge(rates, grp_wt, by = c("ym", "educ_group"), all.x = TRUE)

# ---------- Weighted covariance helper ----------
wcov <- function(x, y, w) {
  w <- w / sum(w)
  sum(w * (x - sum(w * x)) * (y - sum(w * y)))
}

# =============================================================================
# A. LEVELS DECOMPOSITION
# =============================================================================
decomp_lev <- rates[, {
  w  <- pop_wt
  vv <- wcov(ln_v, ln_v, w)
  if (vv > 0) {
    share_s <- wcov(ln_v, ln_S, w) / vv
    share_f <- wcov(ln_v, -ln_F, w) / vv
  } else {
    share_s <- NA_real_
    share_f <- NA_real_
  }
  .(share_fluc_s = share_s, share_fluc_f = share_f)
}, by = ym]

decomp_lev <- decomp_lev[!is.na(share_fluc_s)]

cat("\n=== Q2.4A: Levels decomposition (mean shares) ===\n")
cat("Mean share_fluc_S (job-losing):", mean(decomp_lev$share_fluc_s), "\n")
cat("Mean share_fluc_F (job-finding):", mean(decomp_lev$share_fluc_f), "\n")
cat("Sum:", mean(decomp_lev$share_fluc_s) + mean(decomp_lev$share_fluc_f), "\n")

# Plot levels decomposition (both S and F on same figure)
decomp_lev_long <- melt(decomp_lev, id.vars = "ym",
  measure.vars = c("share_fluc_s", "share_fluc_f"),
  variable.name = "component", value.name = "share")
decomp_lev_long[, component := fifelse(component == "share_fluc_s",
  "Job-losing (S)", "Job-finding (F)")]

p_lev <- ggplot(decomp_lev_long, aes(x = ym_to_date(ym), y = share, color = component)) +
  geom_line(linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Decomposition of Cross-Sectional Unemployment Fluctuations (Levels)",
       x = "Month", y = "Share of Var(ln v)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("Figures/plot_q2_4_decomp_levels.pdf", p_lev, width = 10, height = 5)

# =============================================================================
# B. CHANGES DECOMPOSITION (month-over-month differences)
# =============================================================================
# Compute Δln_v, Δln_S, Δln_F for each education group
setorder(rates, educ_group, ym)
rates[, dln_v := ln_v - shift(ln_v, 1L), by = educ_group]
rates[, dln_S := ln_S - shift(ln_S, 1L), by = educ_group]
rates[, dln_F := ln_F - shift(ln_F, 1L), by = educ_group]

# Use lagged population weight (weight from month t-1)
rates[, pop_wt_lag := shift(pop_wt, 1L), by = educ_group]

rates_d <- rates[!is.na(dln_v) & !is.na(pop_wt_lag)]

decomp_chg <- rates_d[, {
  w  <- pop_wt_lag
  vv <- wcov(dln_v, dln_v, w)
  if (vv > 0) {
    share_s <- wcov(dln_v, dln_S, w) / vv
    share_f <- wcov(dln_v, -dln_F, w) / vv
  } else {
    share_s <- NA_real_
    share_f <- NA_real_
  }
  .(share_fluc_s = share_s, share_fluc_f = share_f)
}, by = ym]

decomp_chg <- decomp_chg[!is.na(share_fluc_s)]

cat("\n=== Q2.4B: Changes decomposition (mean shares) ===\n")
cat("Mean share_fluc_S (job-losing):", mean(decomp_chg$share_fluc_s), "\n")
cat("Mean share_fluc_F (job-finding):", mean(decomp_chg$share_fluc_f), "\n")
cat("Sum:", mean(decomp_chg$share_fluc_s) + mean(decomp_chg$share_fluc_f), "\n")

# Plot changes decomposition (both S and F on same figure)
decomp_chg_long <- melt(decomp_chg, id.vars = "ym",
  measure.vars = c("share_fluc_s", "share_fluc_f"),
  variable.name = "component", value.name = "share")
decomp_chg_long[, component := fifelse(component == "share_fluc_s",
  "Job-losing (S)", "Job-finding (F)")]

p_chg <- ggplot(decomp_chg_long, aes(x = ym_to_date(ym), y = share, color = component)) +
  geom_line(linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Decomposition of Cross-Sectional Unemployment Fluctuations (Changes)",
       x = "Month", y = "Share of Var(Δln v)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("Figures/plot_q2_4_decomp_changes.pdf", p_chg, width = 10, height = 5)

cat("\nDone. Q2.4 plots saved.\n")
