# library
library("stats")
library("tidyverse")
library("ggplot2")
library("MASS")

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
# Define the measurements for items
measurements <- c(2, 1, 5, 8)

# Function to calculate ESS
calculate_ESS <- function(items) {
  mean_value <- mean(items)
  ess <- sum((items - mean_value)^2)
  return(ess)
}

# Mean and ESS calculation for each potential merge

# Merge {12} and {3}
measurements_123 <- c(measurements[1], measurements[2], measurements[3])
ess_123 <- calculate_ESS(measurements_123)

# Merge {12} and {4}
measurements_124 <- c(measurements[1], measurements[2], measurements[4])
ess_124 <- calculate_ESS(measurements_124)

# Merge {3} and {4}
measurements_34 <- c(measurements[3], measurements[4])
ess_34 <- calculate_ESS(measurements_34)

# Output the ESS values
cat("ESS for merge {12} and {3}:", ess_123, "\n")
cat("ESS for merge {12} and {4}:", ess_124, "\n")
cat("ESS for merge {3} and {4}:", ess_34, "\n")


# Function to calculate ESS
calculate_ESS <- function(items) {
  mean_value <- mean(items)
  ess <- sum((items - mean_value)^2)
  return(ess)
}

# Mean and ESS calculation for the final merge {12} and {34}
ess_1234 <- calculate_ESS(measurements)

# Output the ESS value
cat("ESS for the final merge {12} and {34}:", ess_1234, "\n")


# Perform hierarchical clustering using Ward's method
hc <- hclust(dist(measurements), method = "ward.D")
# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering with Ward's Method", xlab = "Distance Object", sub = "", labels = labels)
# Manually calculated ESS values at each step
ess_values <- c(0.5, 4.5, 30.0) # Replace with your actual ESS values

# Get the number of initial observations (items)
n <- length(measurements)

# Annotate the dendrogram with ESS values
for (i in 1:length(ess_values)) {
  # Find the height at which the merge occurs
  y_coord <- hc$height[i]

  # Use the number of initial observations to adjust the x-coordinate
  x_coord <- n - i + 0.5

  # Add the ESS text annotation
  text(x = x_coord, y = y_coord, labels = paste("ESS:", ess_values[i]), pos = 3)
}
