library(nleqslv)

# ── Parameters ──
beta  <- 0.96^(1/12)
s     <- 0.02
b_bar     <- 0.95
gamma <- 0.5
iota  <- 1.6
cc    <- 0.101        # vacancy posting cost (avoid masking base c())
pi_z  <- 0.985         # persistence of Markov chain
z_h   <- exp(0.01)
z_l   <- exp(-0.01)

# ── Solve for theta(z_h), theta(z_l), J(z_h), J(z_l) ──
equations <- function(x) {
  th_h <- x[1]; th_l <- x[2]; Jh <- x[3]; Jl <- x[4]
  eq1 <- (cc / beta) * (1 + th_h^iota)^(1/iota) - (pi_z * Jh + (1 - pi_z) * Jl)
  eq2 <- (cc / beta) * (1 + th_l^iota)^(1/iota) - (pi_z * Jl + (1 - pi_z) * Jh)
  eq3 <- Jh - ((1 - gamma) * (1 - b_bar)*z_h - gamma * cc * th_h + beta * (1 - s) * (pi_z * Jh + (1 - pi_z) * Jl))
  eq4 <- Jl - ((1 - gamma) * (1 - b_bar)*z_l - gamma * cc * th_l + beta * (1 - s) * (pi_z * Jl + (1 - pi_z) * Jh))
  c(eq1, eq2, eq3, eq4)
}

sol <- nleqslv(c(0.5, 0.3, 10, 8), equations)
theta_h <- sol$x[1]; theta_l <- sol$x[2]

cat(sprintf("theta(z_h) = %.6f\n", theta_h))
cat(sprintf("theta(z_l) = %.6f\n", theta_l))

# ── Matching function helpers ──
# f(theta) = theta / (1 + theta^iota)^(1/iota)
f_theta <- function(theta) {
  theta / (1 + theta^iota)^(1/iota)
}

# ── Simulate ──
set.seed(42)
T_sim   <- 2000
T_burn  <- 1000          # burn-in to remove dependence on initial conditions
T_total <- T_burn + T_sim

# Simulate Markov chain for z: states 1 = z_h, 2 = z_l
z_idx    <- integer(T_total)
z_idx[1] <- 1           # start in high state
for (t in 2:T_total) {
  if (runif(1) < pi_z) {
    z_idx[t] <- z_idx[t - 1]           # stay
  } else {
    z_idx[t] <- 3 - z_idx[t - 1]       # switch
  }
}

# Map z index to theta values
theta_vec <- ifelse(z_idx == 1, theta_h, theta_l)
z_vec     <- ifelse(z_idx == 1, z_h, z_l)

# Unemployment dynamics: u_{t+1} = u_t + s(1-u_t) - f(theta_t)*u_t
u_vec    <- numeric(T_total)
u_vec[1] <- 0.05        # steady-state initial value
for (t in 1:(T_total - 1)) {
  u_vec[t + 1] <- u_vec[t] + s * (1 - u_vec[t]) - f_theta(theta_vec[t]) * u_vec[t]
}

# Vacancies: v_t = theta_t * u_t
v_vec <- theta_vec * u_vec

# ── Drop burn-in ──
idx       <- (T_burn + 1):T_total
log_z     <- log(z_vec[idx])
log_u     <- log(u_vec[idx])
log_v     <- log(v_vec[idx])
log_theta <- log(theta_vec[idx])

# ── Standard deviations ──
cat(sprintf("\nStd dev of log(z_t)     = %.6f\n", sd(log_z)))
cat(sprintf("Std dev of log(u_t)     = %.6f\n", sd(log_u)))
cat(sprintf("Std dev of log(v_t)     = %.6f\n", sd(log_v)))
cat(sprintf("Std dev of log(theta_t) = %.6f\n", sd(log_theta)))

# ── Plot: 4 panels (2x2) ──
tt <- 1:T_sim


figname <- "Q3/Figures/plot_q3_part5_sim.pdf"
pdf(figname, width = 9, height = 7)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

plot(tt, log_z, type = "l", col = "orange3",
     xlab = "Month", ylab = expression(ln~z[t]), main = "Productivity")

plot(tt, log_u, type = "l", col = "steelblue",
     xlab = "Month", ylab = expression(ln~u[t]), main = "Unemployment")

plot(tt, log_v, type = "l", col = "firebrick",
     xlab = "Month", ylab = expression(ln~v[t]), main = "Vacancies")

plot(tt, log_theta, type = "l", col = "darkgreen",
     xlab = "Month", ylab = expression(ln~theta[t]), main = "Market Tightness")

dev.off()
cat("\nPlot saved to Q3/Figures/plot_q3_part5_sim.pdf\n")
