# Loading necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(lmtest)
library(nlme)

# Load the dataset
data <- read.csv("/Users/satoshiido/Documents/programming/statistical-analysis/582/SCS project/Oxygen.csv")

# Combine 'handwarmer' and 'bag' into a single factor
data$handwarmer_bag <- interaction(data$handwarmer, data$bag)
# Convert the combined factor to a factor data type
data$handwarmer_bag <- as.factor(data$handwarmer_bag)

## Drop hour=24 and 48
data2 <- subset(data, !(hours %in% c(24, 48)))
View(data2)

# Assuming your dataframe is named 'data'
# data$TimeBefore = pmin(data$hours, 12)  # Time until the changepoint (minimizes at 12)
# data$TimeAfter = pmax(data$hours - 12, 0)  # Time after the changepoint (starts at 0)

# Adding indicator variables for the changepoint at 8.5 hours
data2$before_change <- ifelse(data2$hours <= 8.5, 1, 0)
data2$after_change <- ifelse(data2$hours > 8.5, 1, 0)

# plot Y vs time(hour) or plot log(Y) vs time(hour)
# R code
plot(data2$hours, data2$oxygen, main="Scatterplot of hours vs oxygen", xlab="hours", ylab="oxygen")
plot(data2$hours, data2$log_oxygen, main="Scatterplot of hours vs log_oxygen", xlab="hours", ylab="log_oxygen")
plot(data2$log_hours, data2$log_oxygen, main="Scatterplot of log_hours vs log_oxygen", xlab="log_hours", ylab="log_oxygen")
plot(data2$log_hours, data2$oxygen, main="Scatterplot of log_hours vs oxygen", xlab="log_hours", ylab="oxygen")
plot(data2$squared_hours, data2$log_oxygen, main="Scatterplot of squared_hours vs log_oxygen", xlab="squared_hours", ylab="log_oxygen")
plot(data2$squared_hours, data2$oxygen, main="Scatterplot of squared_hours vs oxygen", xlab="squared_hours", ylab="oxygen")

cor(data2[c("hours", "squared_hours", "before_change", "after_change")])


# Fit the generalized linear model
# logoxygen=handwarmer|bag+hours+hours^2+repeated(rep)
model1 <- glm(log_oxygen ~ log_hours * handwarmer_bag, data = data, family = gaussian(link = "identity"))
model1 <- lm(log_oxygen ~ log_hours * handwarmer_bag, data = data)
summary(model1)

model2 <- lm(log_oxygen ~ hours * handwarmer_bag, data = data)
summary(model2)

# repeated measure as random effect 
model3 <- lme(log_oxygen ~ handwarmer_bag + hours + squared_hours + before_change, 
               random = ~1 | rep,
               weights = varIdent(form = ~ 1 | handwarmer_bag),
               data = data2)
summary(model3)
plot(model3)
qqnorm(residuals(model3))
qqline(residuals(model3))
# Perform the Breusch-Pagan test
bptest(residuals(model3) ~ fitted(model3))

# Create a list of models
models <- list(model1, model2, model3)  # replace with your actual models

# Initialize an empty list to store the test results
bp_tests <- list()

# Loop over the models
for (i in seq_along(models)) {
  # Check if the model is of class 'lme'
  if (class(models[[i]]) == "lme") {
    # Perform the Breusch-Pagan test using residuals and fitted values
    bp_tests[[i]] <- bptest(residuals(models[[i]]) ~ fitted(models[[i]]))
  } else {
    # Perform the Breusch-Pagan test as before
    bp_tests[[i]] <- bptest(models[[i]], ~ fitted(models[[i]]), data = data)
  }
  
  # Display the test results
  print(bp_tests[[i]])
}


# Stepwise Regression
intercept_only <- lm(log_oxygen ~ 1, data=data)
# Define model with all predictors and their interactions
all <- lm(log_oxygen ~ log_hours * handwarmer + log_hours * bag + log_hours * handwarmer * bag, data=data)
summary(all)

#perform backward stepwise regression
both <- step(intercept_only, direction='both', scope = list(lower = intercept_only, upper = formula(all)))

# Create a residual vs fitted values plot
plot(model1$fitted.values, residuals(model1),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted values")

# Add a horizontal line at y = 0
abline(h = 0, col = "red")


# Weighted Least Squares Regression
## Fit the model
model3 <- lm(log_oxygen ~ I(hours^2) * handwarmer_bag, data = data2)
summary(model3)

model4 <- aov(log_oxygen ~ I(hours^2) * handwarmer_bag + Error(handwarmer_bag / (I(hours^2) * handwarmer_bag)), data = data2)
summary(model4)

# Perform the Breusch-Pagan test
bp_test <- bptest(model3, ~ fitted(model3), data = data2)
# Display the test results
print(bp_test)

## Define weights to use
wt <- 1 / lm(abs(model3$residuals) ~ model3$fitted.values)$fitted.values^2

# Perform weighted least squares regression
wls_model <- lm(log_oxygen ~ I(hours^2) * handwarmer_bag, data = data2, weights = wt)
summary(wls_model)


data.frame(y = rstandard(wls_model),
           x = wls_model$fitted.values) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(title = "Standardized Residuals vs Fitted Values Plot")


# Create a residual vs fitted values plot
plot(wls_model$fitted.values, residuals(wls_model),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted values")

# Add a horizontal line at y = 0
abline(h = 0, col = "red")


# Exponential Decay Model
# Adding indicator variables for the changepoint
data2$before_change <- ifelse(data$hours <= 12, 1, 0)
data2$after_change <- ifelse(data$hours > 12, 1, 0)

# Linear model with changepoint
modelA <- lm(log_oxygen ~ before_change + after_change * hours, data = data)
summary(modelA)