# library
library("stats")
library("tidyverse")
library("ggplot2")
library("MASS")
library("cluster")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 12.7
(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2

# define the lower triangular part of the sample correlation matrix
cor_matrix <- matrix(0, nrow = 5, ncol = 5)
lower_tri_values <- c(1, 0.63, 0.51, 0.12, 0.16, 1, 0.57, 0.32, 0.21, 1, 0.18, 0.15, 1.00, 0.68)
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- lower_tri_values

# labels for the stocks
labels <- c("JP Morgan", "Citibank", "Wells Fargo", "Royal DutchShell", "ExxonMobil")

# convert to distance matrix using the formula sqrt(2 * (1 - correlation))
dist_matrix <- sqrt(2 * (1 - cor_matrix))

# convert the matrix to a 'dist' object
dist_object <- as.dist(dist_matrix)

# perform single linkage clustering
single_linkage <- hclust(dist_object, method = "single")

# perform complete linkage clustering
complete_linkage <- hclust(dist_object, method = "complete")

# plot the dendrograms
par(mfrow = c(1, 2)) # Set the plotting area to display two plots side by side

# plot single linkage dendrogram
plot(single_linkage, main = "Single Linkage Dendrogram", xlab = "Distance Object", sub = "", labels = labels)
# plot complete linkage dendrogram
plot(complete_linkage, main = "Complete Linkage Dendrogram", xlab = "Distance Object", sub = "", labels = labels)



# 12.10
# (a) (b)
# function to calculate the Error Sum of Squares (ESS)
calculate_ESS <- function(items) {
  mean_value <- mean(items)
  ess <- sum((items - mean_value)^2)
  return(ess)
}

# initial measurements
measurements <- c(2, 1, 5, 8)

# scenarios to calculate ESS
scenarios <- list(
  c("13" = calculate_ESS(c(measurements[1], measurements[3]))),
  c("14" = calculate_ESS(c(measurements[1], measurements[4]))),
  c("23" = calculate_ESS(c(measurements[2], measurements[3]))),
  c("24" = calculate_ESS(c(measurements[2], measurements[4]))),
  c("34" = calculate_ESS(c(measurements[3], measurements[4])))
)

# display the increase in ESS for each scenario
scenarios

# (c)
# define the measurements for items
measurements <- c(2, 1, 5, 8)

# Function to calculate ESS
calculate_ESS <- function(items) {
  mean_value <- mean(items)
  ess <- sum((items - mean_value)^2)
  return(ess)
}

# mean and ESS calculation for each potential merge
# merge {12} and {3}
measurements_123 <- c(measurements[1], measurements[2], measurements[3])
ess_123 <- calculate_ESS(measurements_123)
# merge {12} and {4}
measurements_124 <- c(measurements[1], measurements[2], measurements[4])
ess_124 <- calculate_ESS(measurements_124)
# merge {3} and {4}
measurements_34 <- c(measurements[3], measurements[4])
ess_34 <- calculate_ESS(measurements_34)
# output the ESS values
cat("ESS for merge {12} and {3}:", ess_123, "\n")
cat("ESS for merge {12} and {4}:", ess_124, "\n")
cat("ESS for merge {3} and {4}:", ess_34, "\n")


# function to calculate ESS
calculate_ESS <- function(items) {
  mean_value <- mean(items)
  ess <- sum((items - mean_value)^2)
  return(ess)
}

# mean and ESS calculation for the final merge {12} and {34}
ess_1234 <- calculate_ESS(measurements)

# output the ESS value
cat("ESS for the final merge {12} and {34}:", ess_1234, "\n")


# perform hierarchical clustering using Ward's method
hc <- hclust(dist(measurements), method = "ward.D")
# plot the dendrogram
plot(hc, main = "Hierarchical Clustering with Ward's Method", xlab = "Distance Object", sub = "")
# manually calculated ESS values at each step
ess_values <- c(0.5, 4.5, 30.0) # Replace with your actual ESS values

# get the number of initial observations (items)
n <- length(measurements)

# annotate the dendrogram with ESS values
for (i in 1:length(ess_values)) {
  # Find the height at which the merge occurs
  y_coord <- hc$height[i]
  # use the number of initial observations to adjust the x-coordinate
  x_coord <- n - i + 0.5
  # add the ESS text annotation
  text(x = x_coord, y = y_coord, labels = paste("ESS:", ess_values[i]), pos = 3)
}


# 12.28
# data setup
col_names <- c("Family", "DistRD", "Cotton", "Maize", "Sorg", "Millet", "Bull", "Cattle", "Goats")
df <- read.table(file.path(main_path, "hw9/T8-7.txt"), header=FALSE, col.names=col_names)
df <- df[-c(25, 34, 69, 72),]
# check if the data is numeric or not
sapply(df, is.numeric)

# perform K-means clustering with K = 4, 5, 6
kmeans_3 <- kmeans(df, centers = 3)
kmeans_4 <- kmeans(df, centers = 4)
kmeans_5 <- kmeans(df, centers = 5)
kmeans_6 <- kmeans(df, centers = 6)

# display the cluster centers
clusplot(df, kmeans_3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="K-means Clustering with K = 3")
clusplot(df, kmeans_4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="K-means Clustering with K = 4")
clusplot(df, kmeans_5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="K-means Clustering with K = 5")
clusplot(df, kmeans_6$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="K-means Clustering with K = 6")


# 12.26
# calculate the Euclidean distance between pairs of farms
dist_matrix <- dist(df)
# perform hierarchical clustering using average linkage
hc_average <- hclust(dist_matrix, method = "average")
# perform hierarchical clustering using Ward's method
hc_ward <- hclust(dist_matrix, method = "ward.D2")
# plot the dendrograms
par(mfrow = c(1, 2)) # set the plotting area to display two plots side by side
plot(hc_average, main = "Average Linkage", xlab = "Farms", sub = "")
plot(hc_ward, main = "Ward's Method", xlab = "Farms", sub = "")
