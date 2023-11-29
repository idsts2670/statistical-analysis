# library
library("tidyverse")
library("ggplot2")
library("MASS")
library("car")
library("fs")
library("moments")
library("pracma")
require("umx")
library("CCA")
library("CCP")
library("multiUS")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 10.5 (a)
## taking sequence of elements
sigma11 <- matrix(
  c(1.0, 0.4, 0.4, 1.0), nrow = 2, ncol = 2, byrow = TRUE
)
sigma12 <- matrix(
  c(0.5, 0.6, 0.3, 0.4), nrow = 2, ncol = 2, byrow = TRUE
)
sigma21 <- matrix(
  c(0.5, 0.3, 0.6, 0.4), nrow = 2, ncol = 2, byrow = TRUE
)
sigma22 <- matrix(
  c(1.0, 0.2, 0.2, 1.0), nrow = 2, ncol = 2, byrow = TRUE
)
sigma11_inv <- solve(sigma11)
sigma22_inv <- solve(sigma22)

# calculate the special structure of the matrix
sp1 <- sigma11_inv %*% sigma12 %*% sigma22_inv %*% sigma21

# eigenvalues and eigenvectors of it
eigen_sp1 <- eigen(sp1)

# 10.5 (b)
standardized_vec <- matrix(
  c(0.4371, 0.2178, 0.2178, 0.1096), nrow = 2, ncol = 2, byrow = TRUE
)
eigen_standardized_vec <- eigen(standardized_vec)
# extract the elements [1,2] and [2,2] from the matrix
e2 <- eigen_standardized_vec$vectors[, 2]
# calculate the inverse squared root of the matrix
rho11_inv <- solve(sigma11)
rho11_squared_root_inv <- sqrtm(rho11_inv)
rho22_inv <- solve(sigma22)

# calculate the `a2`
a2 <- rho11_squared_root_inv$B %*% e2
# calculate the `b2`
b2 <- rho22_inv %*% sigma21 %*% a2
var_b2 <- t(b2) %*% sigma22 %*% b2
scaled_b2 <- b2 / sqrt(var_b2[1, 1])

# calculate var_U2 and var_V2
var_U2 <- t(a2) %*% sigma11 %*% a2
var_V2 <- t(b2) %*% sigma22 %*% b2
cov_U2_V2 <- t(a2) %*% sigma12 %*% b2
corr_U2_V2 <- cov_U2_V2 / sqrt(var_U2 * var_V2)



# 10.13
# read the data
R <- umx_read_lower(file.path(main_path, "hw7/P10-13.txt"), diag = TRUE)
write.csv(R, file.path(main_path, "hw7/P10-13.csv"))

# add the column names
wheat <- c("kernel_texture", "test_weight", "damaged_kernels", "foreign_material", "crude_protein_in_the_wheat")
flour <- c("wheat_per_barrel_of_flour", "ash_in_flour", "crude_protein_in_flour", "gluten_quality_index")
combined_col_names <- c(wheat, flour)
dimnames(R) <- list(combined_col_names, combined_col_names) # manually add

# 10.13 (a)
# calculate the special structure of the matrix
## extract the submatrices
r11 <- R[1:5, 1:5]
r12 <- R[1:5, 6:9]
r21 <- R[6:9, 1:5]
r22 <- R[6:9, 6:9]
r11_inv <- solve(r11)
r11_inv_sqrt <- sqrtm(r11_inv)
r22_inv <- solve(r22)
## apply the formula
sp2 <- r11_inv_sqrt$B %*% r12 %*% r22_inv %*% r21 %*% r11_inv_sqrt$B
# eigenvalues
eigen_sp2 <- eigen(sp2)
# rho (= squared root of eigenvalues)
rhos <- sqrt(eigen_sp2$values)[1:4]

# Liklihood ratio test (LRT using Wilks' Lambda)
## calculate Wilks' Lambda
wilks_lambda <- prod(1 - rhos^2)

## sample size and dimensions
n <- 138 # (sample size)
p <- 5  # number of variables in set 1
q <- 4  # number of variables in set 2
k <- min(p, q)  # number of canonical correlations
df1 <- p * q  # degrees of freedom for the first LRT
df2 <- (p - 2) * (q - 2)

## calculate chi-square statistic
chi_square <- -((n - 1) - (p + q + 1) / 2) * log(wilks_lambda)

## chi-square critical value for two LRTs
critical_value1 <- qchisq(0.99, df1)
critical_value2 <- qchisq(0.99, df2)

# calculate the sample canonical variables
e1 <- eigen_sp2$vectors[, 1] # extract the first eigenvector of the matrix
e2 <- eigen_sp2$vectors[, 2] # extract the second eigenvector of the matrix
a1 <- r11_inv_sqrt$B %*% e1 # calculate the `a1`
a2 <- r11_inv_sqrt$B %*% e2 # calculate the `a2`
b1 <- r22_inv %*% r21 %*% a1 # calculate the `b1`
b2 <- r22_inv %*% r21 %*% a2 # calculate the `b2`
## scale `b1` and `b2`
var_b1 <- t(b1) %*% r22 %*% b1 # calculate the variance of `b1`
scaled_b1 <- b1 / sqrt(var_b1[1, 1]) # sqrt(var_b1) is the eigenvalue of the matrix
var_b2 <- t(b2) %*% r22 %*% b2 # calculate the variance of `b2`
scaled_b2 <- b2 / sqrt(var_b2[1, 1])

# 10.13 (c)
# matrix of sample correlations between U1 and Z1
R_U1Z1 <- t(a1) %*% r11
# proportion of sample variance of Z1 explained by the first canonical variates, U1
R_Z1_U1 <- sum(R_U1Z1^2) / length(a1)

# matrix of the sample correlations between V1 (scaled) and Z2
R_V1Z2 <- t(scaled_b1) %*% r22
R_Z2_V1 <- sum(R_V1Z2^2) / length(scaled_b1)




# 10.18

col_names <- c("BL", "EM", "SF", "BS", "AFL", "LFF", "FFF", "ZST")
df <- read.table(file.path(main_path, "hw7/T7-7.txt"), header=FALSE, col.names=col_names)
# assign set
paper <- df[, 1:4]
pulp <- df[, 5:8]

# regular correlation matrix
matcor(paper, pulp)

# canonical correlation analysis
cca <- cancor(paper, pulp)

# canonical correlations
cc <- cca$cor; cc

# rho_sq (= squared of canonical correlations)
rho_sq <- cc^2
rho_sq[1:2]

# Liklihood ratio test (LRT using Wilks' Lambda)
## calculate Wilks' Lambda
wilks_lambda1 <- prod(1 - rho_sq)
wilks_lambda2 <- prod(1 - rho_sq[3:4])

## sample size and dimensions
n <- 62 # sample size
p <- 4  # number of variables in set 1
q <- 4  # number of variables in set 2
k <- min(p, q)  # number of canonical correlations
df1 <- p * q  # degrees of freedom for the first LRT
df2 <- (p - 2) * (q - 2) # degrees of freedom for the first LRT

## calculate chi-square statistic
chi_square_statistic1 <- -((n - 1) - (p + q + 1) / 2) * log(wilks_lambda1);chi_square_statistic1
chi_square_statistic2 <- -((n - 1) - (p + q + 1) / 2) * log(wilks_lambda2);chi_square_statistic2

## chi-square critical value for two LRTs
critical_value1 <- qchisq(0.95, df1); critical_value1
critical_value2 <- qchisq(0.99, df2); critical_value2


# canonical variates
cpaper <- cca$xcoef # canonical variates for paper
cpulp <- cca$ycoef # canonical variates for pulp


cc <- cc(paper, pulp)
# display the canonical correlations
cc$cor

# standardized psych canonical coefficients diagonal matrix of psych sd's
s1 <- diag(sqrt(diag(cov(paper))))
s1 %*% cc$xcoef

s2 <- diag(sqrt(diag(cov(pulp))))
s2 %*% cc$ycoef


cancorPlus(paper, pulp, xcenter = TRUE, ycenter = TRUE, useCCApackage = TRUE)
