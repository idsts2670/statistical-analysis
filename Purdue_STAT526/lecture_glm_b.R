# set the working directory
setwd("/Users/satoshiido/Documents/statistical-analysis/")

# Dose Response Models

x <- (-150:150) / 25

# logistic function curve
plot(x, 1 - 1 / (1 + exp(x)), type = "l", ylab = "p")

# cumulative probability
lines(x, pnorm(x), col = 2)
# C log-log link function Extreme value family
lines(x, 1 - exp(-exp(x)), col = 3)

lines(c(-6, 6), c(.5, .5), lty = 2, col = 1)

# cumulative probability with sd = 1.7
lines(x, pnorm(x, sd = 1.7), col = 6)


# Binomial Family in R

"budworm" <-
structure(list(numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 
12, 16), ldose = c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5), sex = structure(c(2, 
2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1), .Label = c("F", "M"), class = "factor"), 
    SF = structure(c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16, 
    19, 16, 11, 7, 2, 0, 20, 18, 14, 10, 8, 4), .Dim = c(12, 
    2))), .Names = c("numdead", "ldose", "sex", "SF"), row.names = c("1", 
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), class = "data.frame")

# budworm data input from MASS book
budworm2 <- read.csv("./Purdue_STAT526/budworm2.csv")

# https://bit.ly/3kgkiID
# https://bit.ly/3IeQdBq
# https://bit.ly/3EkTe22
# https://bit.ly/3KjJ5Gm

# training glm
## these two below are the same

budwm.lgt0 <- glm(SF ~ ldose * sex, family = binomial, data = budworm)
budwm.lgt0

# weight = 重み`weights`を使った重みつき最小自乗法 (つまり sum(w*e^2) を最小化する) が使われる
budwm.lgt1 <- update(budwm.lgt0, numdead / 20 ~ ., weight = rep(20, 12))
budwm.lgt1

# same results
summary(budwm.lgt0); summary(budwm.lgt1)

# ldose and sex parameters have contributed a lot to the model
anova(budwm.lgt0)
drop1(budwm.lgt0)

# plotting multiple graph
plot(budwm.lgt0)

# Stepwise
# seems like there is no need of interaction terms of ldose * sex
step(budwm.lgt0)

# model renew and re-fitting
# get rid of interaction
# AIC gets better
budwm.lgt <- update(budwm.lgt0, . ~ . - ldose:sex)
budwm.lgt

# prediction with glm method
# new = the name of the new data frame to make predictions for
predict(budwm.lgt, new = data.frame(ldose = 2, sex = "M"))

# type = "response" gives the predicted probabilities
predict(budwm.lgt, type = "response")

# residuals
residuals(budwm.lgt)
residuals(budwm.lgt, "pearson")


# 2 * 2 Table, Deviances
test <- data.frame(y = I(rbind(c(10, 26), c(40, 24))), x = factor(1:2))
test
fit <- glm(y ~ x, family = binomial, data = test)
# Deviance Residuals = 0 meaning the perfect fit
summary(fit)

# same fit with Bernoulli data
test1 <- data.frame(y = c(rep(1, 10), rep(0, 26), rep(1, 40), rep(0,24)), x = factor(c(rep(1, 36), rep(0, 64))))
fit1 <- glm(y ~ x, family = binomial, data = test1)
summary(fit1)

# check the deviances of separate constant fits
summary(glm(y ~ 1, binomial, test1[1:36, ]))
summary(glm(y ~ 1, binomial, test1[-(1:36), ]))


# restrspective logistic Model: Example
x <- runif(100000)
# so rare
y <- rbinom(x, 1, plogis(-10 + 4 * x))
sum(y)
# sampling
ind <- c(sample(100000, 60), (1:100000)[y == 1])
smpl <- data.frame(x = x[ind], y = y[ind])

# retrospective logistic modeling
summary(glm(y ~ x, family = binomial, data = smpl))


# With Logit = -2 + 4x
x <- runif(120)
y <- rbinom(x, 1, plogis(-2 + 4 * x))
sum(y)
## result is similar to the retrospective
summary(glm(y ~ x, family = binomial))

# With Logit = -6 + 4x
x <- runif(2000)
y <- rbinom(x, 1, plogis(-6 + 4 * x))
sum(y)
## sampling
ind <- (1:2000)[y == 1]
ind <- c(ind, sample((1:2000)[-ind], 60))
smpl <- data.frame(x = x[ind], y = y[ind])

## result is similar to the retrospective as well
summary(glm(y ~ x, family = binomial, data = smpl))


# Over-Dispersion: Example
boys <- read.csv("./Purdue_STAT526/boys.csv")
# modeling
boys.fit <- glm(cbind(boy, girl) ~ 1, binomial, weight = fr, data = boys)
# prediction
predict(boys.fit, type = "res")
# boys ratio
p <- sum(boys$fr * (0:6)) / sum(boys$fr) / 6

# Pearson's X^2 statistics over 
sum(resid(boys.fit, type = "pear")^ 2) / (sum(boys$fr) - 1)
