# library
library("tidyverse")
library("dplyr")
library("car")
library("ggplot2")
library("reshape2")
library("caTools")
library("Rcmdr")
library("MASS")
library("ggExtra")
library("ggpubr")
library("GGally")
library("RVAideMemoire")

# problem 4-39
col_names <- c("Indep", "Supp", "Benev", "Conform", "Leader", "Gender", "Socio")
df <-  read.table("/Users/satoshiido/Documents/statistical-analysis/524/hw/hw2/T4-6.txt", header=FALSE, col.names=col_names)
df

v1 <- qqnorm(df$Indep, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
cor(v1$x, v1$y)

v2 <- qqnorm(df$Supp, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
cor(v2$x, v2$y)

v3 <- qqnorm(df$Benev, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
cor(v3$x, v3$y)

v4 <- qqnorm(df$Conform, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
cor(v4$x, v4$y)

v5 <- qqnorm(df$Leader, main="Normal Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")
cor(v5$x, v5$y)

ggpairs(df[, 1:5], diag = list(continuous = "density"), title = "Scatterplot Matrix")

mqqnorm(df[, 1:5], main = "Multi-normal Q-Q Plot")
install.packages("mqqnorm")

install.packages("RVAideMemoire")
Yeslibrary()
