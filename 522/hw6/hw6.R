library("tidyverse")
library("car")
library("ggplot2")


df <- data.frame(
        student = c(1, 2, 3, 4, 5, 6),
        score = c(66, 59, 70, 83, 82, 71)
        )

# prob1(a)
# Calculate the mean
y_U <- mean(df$score)

# Calculate the variance
S_squared <- var(df$score)

# prob1(c)
# Generate all possible SRS samples of size 4
samples <- combn(df$score, 4)

# Calculate the sample mean for each
sample_means <- apply(samples, 2, mean)

# Calculate and print the sample variance of the sample means
n <- 4
N <- nrow(df)
V_y_bar <- var(sample_means) / n * (1 - n/N)


# prob1(d)
# Calculate the number of stratified random samples of size 4
n1 <- nrow(df[df$student <= 3, ]) # size of stratum 1
n2 <- nrow(df[df$student > 3, ]) # size of stratum 2
k <- 2 # number of students to select from each stratum

num_samples_stratum1 <- choose(n1, k)
num_samples_stratum2 <- choose(n2, k)
total_samples <- num_samples_stratum1 * num_samples_stratum2

# prob1(e)
# Generate all possible stratified random samples of size 2 from each stratum
samples_stratum1 <- combn(df$score[df$student <= 3], 2)
samples_stratum2 <- combn(df$score[df$student > 3], 2)

# Generate all possible stratified random samples of size 4
stratified_samples <- expand.grid(1:ncol(samples_stratum1), 1:ncol(samples_stratum2))

# Create a matrix to store the samples
samples_of_size_4 <- matrix(nrow = 4, ncol = nrow(stratified_samples))

# Fill the matrix with the samples
for (i in 1:nrow(stratified_samples)) {
  samples_of_size_4[,i] <- c(samples_stratum1[, stratified_samples[i, 1]], samples_stratum2[, stratified_samples[i, 2]])
}

# prob 1(f)
# Calculate the mean for each stratum
## Stratum 1
y_str_bar1 <- apply(samples_of_size_4[1:2, ], 2, mean)
## Stratum 2
y_str_bar2 <- apply(samples_of_size_4[3:4, ], 2, mean)

# Calculate S_h^2 for each stratum using the provided formula
S_squared_stratum1 <- sum((y_str_bar1 - mean(y_str_bar1))^2) / (length(y_str_bar1) - 1)
S_squared_stratum2 <- sum((y_str_bar2 - mean(y_str_bar2))^2) / (length(y_str_bar2) - 1)
