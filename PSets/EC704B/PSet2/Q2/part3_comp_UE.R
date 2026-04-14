library(nleqslv)

# ── Parameters ──
b1     <- 1.5 * 0.07   # = 0.105
b2     <- 0.07
gamma  <- 0.129122
A      <- 1
eta    <- 0.5
r      <- 0.05 / 12
s      <- 0.02
z_min  <- 0.1
alpha  <- 5
c_cost <- 0.1

# ── Pareto helpers ──
one_minus_G <- function(zR) (z_min / zR)^alpha
surp_int    <- function(zR) (z_min / zR)^alpha * zR / (alpha - 1)

# ── f(theta), q(theta) ──
f_th <- function(th) th^(1 - eta)
q_th <- function(th) th^(-eta)

# ── System: 5 equations in (zR1, zR2, theta, u1, u2) ──
system_eq <- function(x) {
  zR1 <- x[1]; zR2 <- x[2]; th <- x[3]; u1 <- x[4]; u2 <- x[5]

  ft <- f_th(th); qt <- q_th(th)
  I1 <- surp_int(zR1); I2 <- surp_int(zR2)
  oG1 <- one_minus_G(zR1); oG2 <- one_minus_G(zR2)

  phi1 <- u1 / (u1 + u2)

  eq <- numeric(5)
  eq[1] <- zR1 - b1 - gamma * A * ft * I1 / (r + s)
  eq[2] <- zR2 - b2 - gamma * A * ft * I2 / (r + s)
  eq[3] <- c_cost - (1 - gamma) * A * qt * (phi1 * I1 + (1 - phi1) * I2) / (r + s)
  eq[4] <- (1 - u1) / u1 - A * ft * oG1 / s
  eq[5] <- (1 - u2) / u2 - A * ft * oG2 / s
  eq
}

# ── Solve ──
x0  <- c(0.13, 0.11, 1.0, 0.06, 0.04)
sol <- nleqslv(x0, system_eq, control = list(maxit = 5000))

cat("Convergence code:", sol$termcd, "\n\n")
cat(sprintf("  zR1   = %.6f\n", sol$x[1]))
cat(sprintf("  zR2   = %.6f\n", sol$x[2]))
cat(sprintf("  theta = %.6f\n", sol$x[3]))
cat(sprintf("  u1    = %.6f\n", sol$x[4]))
cat(sprintf("  u2    = %.6f\n", sol$x[5]))
cat(sprintf("  u     = %.6f  (pop-weighted)\n", 0.5 * sol$x[4] + 0.5 * sol$x[5]))

# UE rates
th <- sol$x[3]
UE1 <- A * f_th(th) * one_minus_G(sol$x[1])
UE2 <- A * f_th(th) * one_minus_G(sol$x[2])
cat(sprintf("  UE1   = %.6f\n", UE1))
cat(sprintf("  UE2   = %.6f\n", UE2))
cat(sprintf("  Max residual = %.2e\n", max(abs(sol$fvec))))