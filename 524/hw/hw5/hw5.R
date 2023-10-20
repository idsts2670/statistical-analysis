# library
library("tidyverse")
library("car")
library("ggplot2")
library("MASS")
# library("zoom")
# library("MVN")
library("fs")
library("moments")
# library("RVAideMemoire")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 7.2
# data setup
# Given data
z1 <- c(10, 5, 7, 19, 11, 18)
z2 <- c(2, 3, 3, 6, 7, 9)
y <- c(15, 9, 3, 25, 7, 13)
# Standardize the data
z1_star <- scale(z1, center=TRUE, scale=TRUE)
z2_star <- scale(z2, center=TRUE, scale=TRUE)
y_star <- scale(y, center=TRUE, scale=TRUE)
Z_star <- cbind(z1_star, z2_star)
beta_star <- solve(t(Z_star) %*% Z_star) %*% t(Z_star) %*% y_star

solve((t(Z) %*% Z))
Z_star <- matrix(data = c(-0.292, -1.166, -0.816, 1.283, -0.117, 1.108, -1.088, -0.725, -0.725, 0.363, 0.725, 1.451), nrow=6, ncol=2)
y_star <- matrix(data = c(0.391, -0.391, -1.174, 1.695, -0.652, 0.130), nrow=6, ncol=1)

beta <- solve(t(Z_star) %*% Z_star) %*% t(Z_star) %*% y_star
