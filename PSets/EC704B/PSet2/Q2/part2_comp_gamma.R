library(nleqslv)

# ── Parameters ──
b1    <- 0.07
r     <- 0.05 / 12
s     <- 0.02
z_min <- 0.1
alpha <- 5
c_cost <- 0.1

# ── Pareto helpers ──
one_minus_G <- function(zR) (z_min / zR)^alpha
surp_int    <- function(zR) (z_min / zR)^alpha * zR / (alpha - 1)

# ── System: 2 equations in (zR, gamma) ──
#   (1)  (r+s)(zR - b1)(1 - G(zR)) = 19 * gamma * s * integral
#   (2)  19 * c * s * (r+s)        = (1 - gamma) * (1 - G(zR)) * integral
system_eq <- function(x) {
  zR  <- x[1]
  gam <- x[2]

  oG <- one_minus_G(zR)
  SI <- surp_int(zR)

  eq1 <- (r + s) * (zR - b1) * oG - 19 * gam * s * SI
  eq2 <- 19 * c_cost * s * (r + s) - (1 - gam) * oG * SI
  c(eq1, eq2)
}

# ── Solve ──
x0  <- c(z_min * 1.3, 0.5)
sol <- nleqslv(x0, system_eq, control = list(maxit = 2000))

cat("Convergence code:", sol$termcd, "\n\n")

zR  <- sol$x[1]
gam <- sol$x[2]

# ── Recover theta from steady-state flow ──
#   (1 - u)/u = A f(theta)(1 - G(zR)) / s = 19
#   f(theta) = theta^(1 - eta),  eta = 0.5
A   <- 1
eta <- 0.5
oG  <- one_minus_G(zR)
# A * theta^(1-eta) * oG = 19 * s
theta <- (19 * s / (A * oG))^(1 / (1 - eta))

cat(sprintf("  zR    = %.6f\n", zR))
cat(sprintf("  gamma = %.6f\n", gam))
cat(sprintf("  theta = %.6f\n", theta))
UE <- A * theta^(1 - eta) * oG
cat(sprintf("  UE    = %.6f\n", UE))
cat(sprintf("  Max residual = %.2e\n", max(abs(sol$fvec))))