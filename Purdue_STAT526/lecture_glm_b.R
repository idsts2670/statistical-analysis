# Binomial Family
# budworm data input from MASS book
"budworm" <-
structure(list(
        numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16),
        ldose = c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5),
        sex = structure(c(2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1), .Label = c("F", "M"), class = "factor"),
        SF = structure(c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16, 19, 16, 11, 7, 2, 0, 20, 18, 14, 10, 8, 4), 
        .Dim = c(12, 2))), 
        .Names = c("numdead", "ldose", "sex", "SF"),
        row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), 
        class = "data.frame")

"budworm2" <-
structure(list(ldose = c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 
0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5), sex = structure(c(2, 2, 
2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 
1), .Label = c("F", "M"), class = "factor"), Fr = c(1, 4, 9, 
13, 18, 20, 0, 2, 6, 10, 12, 16, 19, 16, 11, 7, 2, 0, 20, 18, 
14, 10, 8, 4), dead = structure(c(2, 2, 2, 2, 2, 2, 2, 2, 2, 
2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), .Label = c("0", 
"1"), class = "factor"), id = structure(c(1, 2, 3, 4, 5, 6, 7, 
8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), .Label = c("1", 
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), class = "factor")), .Names = c("ldose", 
"sex", "Fr", "dead", "id"), row.names = c("1", "2", "3", "4", 
"5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
"16", "17", "18", "19", "20", "21", "22", "23", "24"), class = "data.frame")

budworm
budworm2
# https://bit.ly/3kgkiID
# https://bit.ly/3IeQdBq
# https://bit.ly/3EkTe22
# https://bit.ly/3KjJ5Gm

# training glm
## these two below are the same

budwm.lgt0 <- glm(SF ~ ldose * sex, family = binomial, data = budworm)
budwm.lgt0

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
