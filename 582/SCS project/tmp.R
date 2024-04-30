# Loading necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(lmtest)
library(nlme)

# Load the dataset
current_path <- getwd()
data <- read.csv(file.path(current_path, "/582/SCS project/Oxygen.csv"))

# Combine 'handwarmer' and 'bag' into a single factor
data$handwarmer_bag <- interaction(data$handwarmer, data$bag)
# Convert the combined factor to a factor data type
data$handwarmer_bag <- as.factor(data$handwarmer_bag)

## Drop hour=24 and 48
data2 <- subset(data, !(hours %in% c(24, 48)))
View(data2)

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

# Correlation Matrix
cor(data2[c("hours", "squared_hours", "before_change", "after_change")])


# Fit the Linear Mixed Model 
## log_oxygen = handwarmer|bag + hours + hours^2 + repeated(rep) + weights
## The weights argument is used to model the variance of the residuals so that you can handle cases where the variance differs by groups or across values of a predictor (this is known as heteroscedasticity).
## The weights is set on different variances per level of handwarmer_bag factor
## Repeated measure as random effect 
## before_change is fixed effects
model <- lme(log_oxygen ~ handwarmer_bag + hours + squared_hours + before_change, 
               random = ~1 | rep,
               weights = varIdent(form = ~ 1 | handwarmer_bag),
               data = data2)
summary(model)
plot(model)
qqnorm(residuals(model))
qqline(residuals(model))
# Perform the Breusch-Pagan test
bptest(residuals(model) ~ fitted(model))




###############################################
# Unfinished
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