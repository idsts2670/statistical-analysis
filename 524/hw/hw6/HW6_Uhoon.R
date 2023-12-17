library(factoextra)

# Read the file using read.table
t1_5 <- read.table("D:/Fall 2023/STAT 524/Stat524Data/Data/T1-5.txt", header = F)
# Rename the columns
colnames(t1_5) <- c("wind","solar","C0","N0","N0_2","0_3","HC")

# covariance matrix
S <- cov(t1_5)
eigen(S)
cov.pca <- princomp(S)
summary(cov.pca)
fviz_eig(cov.pca, addlabels = TRUE)


# correlation matrix
R <- cor(t1_5)
eigen(R)
cor.pca <- princomp(R)
summary(cor.pca)
fviz_eig(cor.pca, addlabels = TRUE)


# after normalization
data_normalized <- scale(t1_5)
head(data_normalized)

# covariance matrix
norm_S <- cov(data_normalized)
eigen(norm_S)
norm_cov.pca <- princomp(norm_S)
summary(norm_cov.pca)
fviz_eig(norm_cov.pca, addlabels = TRUE)

# correlation matrix
norm_R <- cor(data_normalized)
eigen(norm_R)
norm_cor.pca <- princomp(norm_R)
summary(norm_cor.pca)
fviz_eig(norm_cor.pca, addlabels = TRUE)


# Exercise 9.10
L <- matrix(c(0.602,0.467,0.926,1,0.874,0.894,0.2,0.154,0.143,0,0.476,0.327),6,2);L
R <- matrix(c(1,0.505,0.569,0.602,0.621,0.603,0.505,1,0.422,0.467,0.482,0.450,0.569,0.422,1,0.926,0.877,0.878,0.602,0.467,0.926,1,0.874,0.894,0.621,0.482,0.877,0.874,1,0.937,0.603,0.45,0.878,0.894,0.937,1),6,6); R
P <- R - L %*% t(L)
P[lower.tri(P)] <- 0
P[upper.tri(P)] <- 0; P

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
