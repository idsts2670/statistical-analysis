library("dplyr")
# Q3
# data frame
df <- data.frame(
        strength = c(3129, 3000, 2865, 2890, 3200, 3300, 2975, 3150, 2800, 2900, 2985, 3050, 2600, 2700, 2600, 2765),
        level = as.factor(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)))
    )
df

lm <- lm(data = df, df$strength ~ df$level)
anova(lm)

library(dplyr)

# get the 
df %>% 
    group_by(level) %>%
        summarise_at(vars(strength), list(name = mean))
