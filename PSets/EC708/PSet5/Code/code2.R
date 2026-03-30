library(ggplot2)

################
# Q4: Anderson Rubin test and inference
################

beta_grid = seq(-10, 10, by = 0.01)
AR_stat_vals = rep(NA, length(beta_grid))



get_f_stat <- function(beta, data){

    data$y_aux <- data$loggdp - beta * data$risk

    reg <- feols(y_aux ~ logmort0 + latitude + asia + africa + other,
                 data = data, vcov = "hetero")

    # Wald test on logmort0 coefficient
    alpha_hat <- coef(reg)["logmort0"]
    se_robust <- se(reg)["logmort0"]

    AR_robust <- (alpha_hat / se_robust)^2
    return(AR_robust)

}


# Compute AR statistic for each beta in the grid

for (i in 1:length(beta_grid)) {
    beta <- beta_grid[i]
    AR_stat_vals[i] <- get_f_stat(beta, data)
}

# Critical value for staitisic chi^2_1
c_crit <- qchisq(1 - 0.05, df = 1)


# Plot the AR statistic and the critical value as dashed horizontal line using ggplot

ggplot(data.frame(beta_grid, AR_stat_vals), aes(x = beta_grid, y = AR_stat_vals)) +
    geom_line() + 
    geom_hline(yintercept = c_crit, linetype = "dashed", color = "red") +
    xlab("beta") +
    ylab("AR statistic") + 
    ggtitle("AR statistic for different values of beta") + theme_minimal()


ggsave(output_loc %+% "Figures/AR_statistic_plot.png", width = 8, height = 6)

# Report results
left_ind <- max(which((AR_stat_vals <= c_crit) & (beta_grid <= -3)))
left_int_boundary <- beta_grid[left_ind] 

right_ind <- min(which((AR_stat_vals <= c_crit) & (beta_grid >= -3)))
right_int_boundary <- beta_grid[right_ind]

print("Left boundary is " %+% left_int_boundary)
print("Right boundary is " %+% right_int_boundary)