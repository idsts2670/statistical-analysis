# library
library("tidyverse")
library("ggplot2")
library("MASS")
library("car")
library("fs")
library("moments")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 8.12
# data setup
col_names <- c("Wind", "Solar_radiation", "CO", "NO", "NO_2", "O3", "HC")
df <- read.table(file.path(main_path, "hw6/T1-5.txt"), header=FALSE, col.names=col_names)


# PCA using prcomp function
## PCA using the covariance matrix S
pca_cov <- prcomp(df, scale = FALSE)

## PCA using the correlation matrix R (data is scaled to have unit variance)
pca_cor <- prcomp(df, scale = TRUE)

## Compare the results
summary(pca_cov)
summary(pca_cor)
pca_cov$rotation # For the covariance matrix S
pca_cor$rotation # For the correlation matrix R

## create a scree plot
plot(pca_cov)
plot(pca_cor)
## create a biplot
biplot(pca_cov)
biplot(pca_cor)


# PCA manually calculated
## covariance matrix and correlation matrix
S <- cov(df)
R <- cor(df)

## eigenvalues and eigenvectors with S
eigen_S <- eigen(S)
eigen_S$values
## eigenvalues and eigenvectors with R
eigen_R <- eigen(R)
eigen_R$values

## calculate loadings for the covariance matrix S
### loadings are the eigenvectors scaled by the square root of the eigenvalues (=standard deviations)
loadings_S <- eigen_S$vectors %*% diag(sqrt(eigen_S$values))
loadings_R <- eigen_R$vectors %*% diag(sqrt(eigen_R$values))

## PCA scores with S
scores_S <- as.matrix(df) %*% eigen_S$vectors
## PCA scores with R
df_standardized <- scale(df)
scores_R <- as.matrix(df_standardized) %*% eigen_R$vectors

## calculate standard deviation of each PC
std_dev_S <- sqrt(eigen_S$values)
std_dev_R <- sqrt(eigen_R$values)


## calculate proportion of variance explained by each PC
prop_var_S <- eigen_S$values / sum(eigen_S$values)
prop_var_R <- eigen_R$values / sum(eigen_R$values)

## calculate cumulative proportion of variance explained
cum_prop_var_S <- cumsum(prop_var_S)
cum_prop_var_R <- cumsum(prop_var_R)

## create a summary list for the PCA based on S
summary_S <- list(
  std_dev = std_dev_S,
  prop_var = prop_var_S,
  cum_prop_var = cum_prop_var_S,
  loadings = loadings_S
)
## create a summary list for the PCA based on R
summary_R <- list(
  std_dev = std_dev_R,
  prop_var = prop_var_R,
  cum_prop_var = cum_prop_var_R,
  loadings = loadings_R
)

## results
list(S = summary_S, R = summary_R)


# 9.10
L <- matrix(c(0.602,0.467,0.926,1,0.874,0.894,
    0.2,0.154,0.143,0,0.476,0.327),6,2
    )
R <- matrix(c(1,0.505,0.569,0.602,0.621,0.603,0.505,1,0.422,0.467,0.482,0.450,
    0.569,0.422,1,0.926,0.877,0.878,0.602,0.467,0.926,1,0.874,0.894,0.621,0.482,
    0.877,0.874,1,0.937,0.603,0.45,0.878,0.894,0.937,1),6,6
    )

P <- R - L %*% t(L)
P[lower.tri(P)] <- 0
P[upper.tri(P)] <- 0
# (a) specific variances
diag(P)

# (b) communalities
L[1,1]^2+L[1,2]^2
L[2,1]^2+L[2,2]^2
L[3,1]^2+L[3,2]^2
L[4,1]^2+L[4,2]^2
L[5,1]^2+L[5,2]^2
L[6,1]^2+L[6,2]^2


# (c) proportion of variance explained by factors
sum(L[,1]^2) * (1/6)
sum(L[,2]^2) * (1/6)


# (d) residual matrix
R - L %*% t(L) - P
