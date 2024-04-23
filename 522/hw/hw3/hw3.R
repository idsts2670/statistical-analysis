# Load the survey package
library("survey")
library("sampling")

# [reference](https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/)

#Importing the data
current_dir <- getwd()
shapespop <- read.csv(file.path(current_dir, "/hw/hw3/shapespop.csv"), header = TRUE)

#check the actual population total and mean as we will use samples to estimate these values
sum(shapespop$area)
mean(shapespop$area)

# population quantity of gray objects or circles, which we use for comparison with the sample estimates
table(shapespop$color)
table(shapespop$shape)



##################################Using R Packages#################################


# Set seed for reproducibility
set.seed(49)

# Part (a):
# Select an SRS of size 200
N <- nrow(shapespop)
index <- sample(1:N, 200, replace = FALSE)

samp <- shapespop[index, ]
samp

n <- nrow(samp)
samp$probability <-rep(n/20000,n)
# sampling weight
samp$sampwt <- rep(20000/n,n)

# str provides a summary of the sample, similar to PROC CONTENTS 
str(samp)

# Save the sample for later exercises
file.path(current_dir, "/hw/hw3/shapespop.csv")
write.table(file=file.path(current_dir, "/hw/hw3/samp.csv"), samp, sep = ",", row.names = F)


# Part (b)
# Draw a histogram of the areas for the objects in the sample
hist_plot <- hist(samp$area, 
                  weights = samp$sampwt, 
                  breaks = 40, 
                  col = "lightblue", 
                  main = "Analyze object area from samp; specify 40 bins for the histogram", 
                  xlab = "Area", 
                  ylab = "Percent",
                  xlim = c(-16, 86), ylim = c(0, 25))

# Add kernel density lines
density_values <- density(samp$area, weights = samp$sampwt)

# Scale the density values to make the peak reach 25
scaled_density <- density_values$y / 29

# Add the scaled kernel density line to the plot
lines(density_values$x, scaled_density, col = "red")

# Add a legend
legend("topright", legend = c("Histogram", "Kernel Density"), fill = c("lightblue", "red"))




# Part(c)
# Create survey design object for area
# Since cluster sampling was not used here, we just have to put ~1 in as a place-holder
# The fpc argument gives the function the necessary information to use a finite population correction in estimation
sample_count <- svydesign(ids = ~1, 
                          weights = ~samp$sampwt,
                          fpc = rep(20000,200), 
                          data = samp)

# Calculate mean and total estimates with confidence intervals
sample_total <- svytotal(~area, design = sample_count)
sample_mean <- svymean(~area, design = sample_count)

# Display the results
print(sample_total)
print(sample_mean)

confint(sample_total, df=199)
confint(sample_mean, df=199)




# Part(d)
# Create survey design object for color
color_count <- svydesign(ids = ~1, weights = ~samp$sampwt, fpc = rep(20000,200), data = samp)

# Calculate the total with confidence intervals
gray_total <- svytotal(~color, design = color_count)

# Display the results
print(gray_total)
confint(gray_total, df=199)

# population quantity of gray objects
table(shapespop$color)


# Part(e)
# Create survey design object for shape
shape_count <- svydesign(ids = ~1, weights = ~samp$sampwt,fpc = rep(20000,200), data = samp)

# Calculate the total with confidence intervals
circle_total <- svytotal(~shape, design = shape_count)

# Display the results
print(circle_total)
confint(circle_total, df=199)

# Check the population
table(shapespop$shape)


##################################Manual Computation#################################

#Importing the data
samp2 <-read.csv("samp.csv", header=T)


# Part(c): area

# Getting the sample variance 
var(samp2$area)

# Getting the sample mean 
mean(samp2$area)


# Estimating population mean 
# Computing the estimated variance using the sample variance 
(1-200/20000)*(216.7099/200)

# Computing the standard error
sqrt(1.072714)

# Getting the t score based on alpha=0.05, two tails
qt(p = .025, df = 199)

# Computing the lower limit of 95% CI
27.94-1.971957*1.0357

# Computing the Upper limit of 95% CI
27.94+1.971957*1.0357


# Estimating population total
# Estimating the total using the sample mean
20000*27.94

# Estimating the SE using the sample SE
20000*1.035719

# Computing the lower limit of 95% CI
558800-1.971957*20714.38

# Computing the Upper limit of 95% CI
558800+1.971957*20714.38


# Part(d): gray 
table (samp2$color)

# Computing the proportion 
77/200

# Estimating the variance of the population using the sample proportion
(1-200/20000)*0.385*(1-0.385)/(200-1)

# Estimating the SE using the sample proportion
sqrt(0.001177926)

# Estimating the SE for the total 
20000*0.03432093

# Estimating the total of gray object using the sample proportion
20000*0.385

# Computing the lower limit of 95% CI
7700-1.971957*686.4186
# Computing the Upper limit of 95% CI
7700+1.971957*686.4186



# Part(e): circles 
table (samp2$shape)

# Computing the proportion 
52/200

# Estimating the variance of the population using the sample proportion
(1-200/20000)*0.26*(1-0.26)/(200-1)

# Estimating the SE using the sample proportion
sqrt(0.0009571658)

# Estimating the SE for the total 
20000*0.0309381

# Estimating the total of gray object using the sample proportion
20000*0.26

# Computing the lower limit of 95% CI
5200-1.971957* 618.762
# Computing the Upper limit of 95% CI
5200+1.971957* 618.762
