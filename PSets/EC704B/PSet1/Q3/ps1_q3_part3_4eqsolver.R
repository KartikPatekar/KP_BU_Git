library(nleqslv)

# Parameters
beta  <- 0.96^(1/12)
s     <- 0.02
b     <- 0.95
gamma <- 0.5
iota  <- 1.6
cc     <- 0.101
pi    <- 0.985
z_h   <- exp(0.01)
z_l   <- exp(-0.01)

equations <- function(x) {
  theta_h <- x[1]
  theta_l <- x[2]
  Jh      <- x[3]
  Jl      <- x[4]

  eq1 <- (cc / beta) * (1 + theta_h^iota)^(1 / iota) - (pi * Jh + (1 - pi) * Jl)
  eq2 <- (cc / beta) * (1 + theta_l^iota)^(1 / iota) - (pi * Jl + (1 - pi) * Jh)
  eq3 <- Jh - ((1 - gamma) * (z_h - b) - gamma * cc * theta_h + beta * (1 - s) * (pi * Jh + (1 - pi) * Jl))
  eq4 <- Jl - ((1 - gamma) * (z_l - b) - gamma * cc * theta_l + beta * (1 - s) * (pi * Jl + (1 - pi) * Jh))

  return(c(eq1, eq2, eq3, eq4))
}

x0  <- c(0.5, 0.3, 10, 8)
sol <- nleqslv(x0, equations)

theta_h <- sol$x[1]
theta_l <- sol$x[2]
Jh      <- sol$x[3]
Jl      <- sol$x[4]

cat(sprintf("theta_h = %.6f\n", theta_h))
cat(sprintf("theta_l = %.6f\n", theta_l))
cat(sprintf("J(z_h)  = %.6f\n", Jh))
cat(sprintf("J(z_l)  = %.6f\n", Jl))

residuals <- equations(sol$x)
cat(sprintf("\nResiduals: %s\n", paste(sprintf("%.2e", residuals), collapse = ", ")))
