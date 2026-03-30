# Load libraries
library(haven)
library(tidyverse)
library(fixest)
library(stargazer)



# Define string concat operator
`%+%` <- function(x, y) {
    paste(x, y, sep = "")
}


# Define parmas
data_file <- "data/AJR2001.dta"
output_loc <- "output/"

# Load data

data <- read_dta(data_file) 


################
# Q1: Replicating columns 2,5, 6 of basemodel
################


ols_col_2 <- feols(loggdp ~ risk, data = data, vcov = "hetero")

ols_col_5 <- feols(loggdp ~ risk + latitude, data = data, vcov = "hetero")

ols_col_6 <- feols(loggdp ~ risk + latitude + asia + africa + other , data = data, vcov = "hetero")

# Write to tex file using etable
etable(ols_col_2, ols_col_5, ols_col_6,
    headers = c("Column 2", "Column 5", "Column 6"),
    title = "OLS Regressions - Replication of AJR (2001)",
    drop = "Constant",
    fitstat = c("n", "r2", "f"),
    tex = TRUE,
    file = output_loc %+% "Tables/q1.tex")


################
# Q2: Using IV regressions instead
################

IV_col_2 <- feols(loggdp ~ 1 | risk ~ logmort0, data = data, vcov = "hetero")

IV_col_5 <- feols(loggdp ~ latitude | risk ~ logmort0, data = data, vcov = "hetero")

IV_col_6 <- feols(loggdp ~ latitude + asia + africa + other | risk ~ logmort0, data = data, vcov = "hetero")

# Write IV results to tex file (second stage + first stage as panels)
etable(IV_col_2, IV_col_5, IV_col_6,
    headers = c("Column 2", "Column 5", "Column 6"),
    title = "IV Regressions - Second Stage",
    drop = "Constant",
    fitstat = c("n", "r2", "f", "ivwald"),
    tex = TRUE,
    file = output_loc %+% "Tables/q2_second_stage.tex")

etable(summary(IV_col_2, stage = 1), summary(IV_col_5, stage = 1), summary(IV_col_6, stage = 1),
    headers = c("Column 2", "Column 5", "Column 6"),
    title = "IV Regressions - First Stage",
    drop = "Constant",
    fitstat = c("n", "r2", "f"),
    tex = TRUE,
    file = output_loc %+% "Tables/q2_first_stage.tex")


