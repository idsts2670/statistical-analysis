# library
library("tidyverse")
library("car")
library("ggplot2")
library("tidyr")
library("MASS")
# library("zoom")
# library("MVN")
library("fs")
library("moments")
# library("RVAideMemoire")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")


# problem 5.9
## draw the ellipse
critical_value <- qchisq(0.95, df = 6)
# Given means
mu1 <- 95.52
mu4 <- 93.39

# Given eigenvalues and eigenvectors
lambda1 <- 3695.52
lambda2 <- 45.92
e1 <- c(0.94, 0.34)
e4 <- c(-0.34, 0.94)

# Constructing the covariance matrix
angle <- atan2(e1[2], e1[1]) * (180 / pi)

# Initialize the plot
plot(1, type="n", xlab="mu1", ylab="mu4",
     xaxp=c(60, 120, 12), yaxp=c(60, 120, 12),
     xlim = c(mu1 - sqrt(lambda1) * sqrt(0.206), mu1 + sqrt(lambda1) * sqrt(0.206)),
     ylim = c(mu4 - 10, mu4 + 10))

# Add grid to the plot
grid(col = "gray", lty = "dotted")

# Draw the ellipse
draw.ellipse(x=mu1, y=mu4, a=sqrt(lambda1)*sqrt(0.206), b=sqrt(lambda2)*sqrt(0.206), angle=angle, border="black", lwd=2)

# Mark the center of the ellipse
points(mu1, mu4, pch = 19, col = "blue")


# Intervals for mu1 and mu2
mu1_interval <- c(75.56, 115.49)
mu2_interval <- c(85.73, 101.00)

# Add a rectangular region to the existing plot
rect(
    xleft=mu1_interval[1], ybottom=mu2_interval[1],
    xright=mu1_interval[2], ytop=mu2_interval[2],
    col=rgb(0,0,0.4,0.2), border="white"
    )


