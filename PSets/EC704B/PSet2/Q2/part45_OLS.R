
######################
#   part 4
######################


# ── Data: 2 types x 2 periods = 4 observations ──
b  <- c(0.07, 0.07, 1.5*0.07, 0.07)
UE <- c(0.38, 0.38, 0.234053, 0.372227)
 
reg <- lm(log(UE) ~ log(b))
summary(reg)
 

######################
#   part 5
######################

g  <- factor(c(1, 2, 1, 2))
 
reg <- lm(log(UE) ~ g + log(b))
summary(reg)