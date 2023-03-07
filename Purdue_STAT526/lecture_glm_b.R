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


# Binomial Family
# budworm data input from MASS book
budworm <- read.csv("./Purdue_STAT526/budworm.csv")
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

summary(budwm.lgt0); summary(budwm.lgt1)
anova(budwm.lgt0)
drop1(budwm.lgt0)
# plotting multiple graph
plot(budwm.lgt0)
# increased or decreased variables at each step, and the model is selected based on AIC criteria.
# seems like there is no need of interaction terms of ldose * sex
step(budwm.lgt0)

# model renew and re-fitting
# get rid of interaction
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
