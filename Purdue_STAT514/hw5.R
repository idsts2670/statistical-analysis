library("dplyr")

#Q1
## ANOVA
df <- data.frame(
            agents = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5)),
            bolts = rep(c(1, 2, 3, 4, 5), times = 4)
)
df
