library("car")
library("ggplot2")
library("dplyr")

df1 <- c(1.186, 1.151, 1.322, 1.339, 1.20, 1.402, 1.365, 1.537, 1.559)
df2 <- c(1.061, 0.992, 1.063, 1.062, 1.065, 1.178, 1.037, 1.086, 1.052)
df <- data.frame(Karlsruh = df1, Lehigh = df2)
class(df)
typeof(df)
print(df)

# check normality assumption for both samples
qqnorm(df$Karlsruh, pch = 1, main = "QQ-plot for Karlsruhe Method")
qqline(df$Karlsruh)
hist(df$Karlsruh, col = "steelblue", main = "Normal")
qqnorm(df$Lehigh, pch = 1, main = "QQ-plot for Lehigh Method")
qqline(df$Lehigh)
hist(df$Lehigh, col = "steelblue", main = "Normal")

# check normality assumption for mean difference
df <- df %>%
    mutate(diff = Karlsruh - Lehigh)
qqnorm(df$diff, pch = 1, main = "QQ-plot for difference between two samples")
qqline(df$diff)
hist(df$diff, col = "steelblue", main = "Normal")