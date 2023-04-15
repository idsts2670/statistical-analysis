library("stats")
library("foreign")
library("MASS")
library("nnet")

# To fit a logistic model, one may use multinom in nnet. To fit a proportional odds model, one may use polr in MASS.
data(housing)
housing

# multinomial regression model
## low level is the base level
## Frequency treated as weights <----------------------------------------------------------????
stepAIC(house.mult0 <- multinom(Sat ~ Infl * Type * Cont, data = housing, weights = Freq))
ouse.mult0

# Poisson regression model
options(contrasts = c("contr.treatment", "contr.treatment"))
house.log0 <- glm(Freq ~ Infl * Type * Cont * Sat, poisson, housing)
house.log0

# All the variables including satisfaction does matter everything else is "alpha" in the model
house.mult <- update(house.mult0, ~ Infl + Type + Cont)
summary(house.mult)

3470.084 - 38.66
head(housing)

# prediction, probability 3 data are the same because they are the same "x" <-why---------------------------------------------------------????
house.pr <- predict(house.mult, type = "prob")
house.pr

house.cpr <- apply(house.pr, 1, cumsum)
house.cpr
# q-logistic
logit <- function(x) log(x / (1 - x))
# relative I should go ahead to
plot(tmp1 <- qlogis(house.cpr[2, ]) - qlogis(house.cpr[1, ]))

house.mult0
# proportional odds fit, saturated model (nested)
3431.422 - 3479.149

house.polr <- polr(Sat ~ Infl + Type + Cont, housing, Freq)
# 40 df,
house.polr
