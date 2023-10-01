# library
library("tidyverse")
library("car")
library("ggplot2")
library("dplyr")
library("tidyr")
library("MASS")
library("zoom")
library("MVN")
library("fs")
library("moments")
library("RVAideMemoire")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# problem 4.39
## prep
col_names <- c("Indep", "Supp", "Benev", "Conform", "Leader", "Gender", "Socio")
df <-  read.table(file.path(main_path, "hw3/T4-6.txt"), header=FALSE, col.names=col_names)
var <- df[, c("Indep", "Supp", "Benev", "Conform", "Leader")]

# (a)
## marginal normality test
v1 <- qqnorm(df$Indep, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
v2 <- qqnorm(df$Supp, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
v3 <- qqnorm(df$Benev, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
v4 <- qqnorm(df$Conform, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
v5 <- qqnorm(df$Leader, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")

## correlation coefficient test for normality (checking the treathhold in the internet)
cor(v1$x, v1$y)
cor(v2$x, v2$y)
cor(v3$x, v3$y)
cor(v4$x, v4$y)
cor(v5$x, v5$y)

# (b)
## multivariate normality test
### assess multivariate normality through multivariate normality tests
mvn(var, mvnTest = "mardia")

## multi-normal Q-Q plot
mqqnorm(var, main = "Multi-normal Q-Q Plot")

## computes the optimal value of lambda by maximizing the log-likelihood function to transform the data to normality
### find the optimal lambda and log-likelihood values for each value of lambda
### "Indep" and "Leader" can be transformed to normality simply by taking the square root, while "Supp" can be transformed by box-cox transformation
boxcox <- boxcox(df$Supp ~ 1)
l <- boxcox$x[which.max(boxcox$y)]
v1 <- qqnorm(sqrt(df$Indep))
v2 <- qqnorm(df$Supp^l)
v5 <- qqnorm(sqrt(df$Leader))

cor(v1$x, v1$y)
cor(v2$x, v2$y)
cor(v5$x, v5$y)
