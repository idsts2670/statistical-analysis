# Poisson Regression Example
library("stats")
library("foreign")
library("MASS")

data(ships, package = "MASS")
ships$year <- factor(ships$year)
class(ships$year)
class(ships$period)

# modeling
ships.log0 <- glm(
                incidents ~ type + year + period + log(service),
                poisson, ships, subset = service > 0
                )
# set the offset term
ships.log <- update(ships.log0, . ~ . - log(service),
                    offset = log(service)
                    )

summary(ships.log)
plot(ships.log)

# Stepwise regression, with AIC = −2l(βˆ) + 2p, adds type:year
# why ^2?? <----------------------------------------------------------????
ships.log1 <- step(ships.log, . ~ .^2)
drop1(ships.log1, test= "F")

# dispersion parameter = (Pearson's X^2 statistics) / (n-p = 13)
# Understanding Deviance Residuals https://bit.ly/41F8jW0
disp <- sum(resid(ships.log1, type = "pear")^2) / ships.log1$df.residual

# pchisq() computes cumulative chi square density for a vector of elements
1 - pchisq((38.695 - 14.587) / disp, 12)
ships.log1$df.residual

# Negative Binomial Examples
# What is the positive points of this model over logistic model? <----------------------------------------------------------????
ships.nb <- update(ships.log, family = neg.bin(20))
summary(ships.nb)

# are these two below the exactly same? <----------------------------------------------------------????
ships.nb1 <- glm.nb(incidents ~ type + year + period + log(service), data = ships, subset = service > 0)
ships.nb2 <- glm.nb(ships.log0)
## the correlation matrix of the estimated parameters is returned and printed
summary(ships.nb1, cor = TRUE)
summary(ships.nb2)

# Contingency Table: Example

## create the data
eye <- cbind(expand.grid(Left = c("A", "B"), Right = c("A", "B")), Fr = c(45, 12, 13, 54))

## saturated model
glm(Fr ~ Left * Right, poisson, eye)
## coef of each beta
log(45); log(12) - log(45); log(13) - log(45)

## additive model
add.fit <- glm(Fr ~ Left + Right, poisson, eye)
fitted(add.fit, type = "res")
## estimated value
log((45 + 12) * (45 + 13) / sum(45, 12, 13, 54))
add.fit
log((12 + 54) / (45 + 13))


# Contingency Table: Example

data(HairEyeColor)
lab <- dimnames(HairEyeColor)
# combine the multiple datasets
HairEye <- cbind(expand.grid(Hair = lab$Hair, Eye = lab$Eye, Sex = lab$Sex),
                Fr = as.vector(HairEyeColor))
# modeling
HairEye.fit <- glm(Fr ~ . ^2, poisson, HairEye)
# we get the pretty low residual deviance
HairEye.fit
# stepwise to select the parameters
drop1(HairEye.fit)
sum(resid(HairEye.fit, "pear")^2)


# Mover-Stayer Model

# read csv files
Migration <- read.csv("./Purdue_STAT526/Migration.csv")

# return diagonal of 1:4
diag(1:4)
# convert a vector object to a factor
Migration$stay <- as.factor(diag(1:4))

matrix(Migration$Fr, 4, 4)
matrix(Migration$stay, 4, 4)

M.S.fit <- glm(Fr ~ stay + move66 + move71, poisson, Migration)

# get the predicted number of movers on the diagnol
predict(M.S.fit, type = "res", data.frame(stay = as.factor(0), move66 = "CC", move71 = "CC"))
jk9 <- predict(M.S.fit, 
            data.frame(
                move66 = Migration$move66,
                move71 = Migration$move71,
                stay = as.factor(0),
                type = "res"
                )
            )

jk8 <- matrix(jk9, 4, 4)
jk8[, 1] / jk8[, 2]

# what does this percentage tell us?  <----------------------------------------------------------????
sum(jk9) / sum(Migration$Fr)

matrix(fitted(M.S.fit, "res"), 4, 4)
matrix(resid(M.S.fit, "res"), 4, 4)


# Square Table: Symmetry

# what does these numbers? are these just random numbers <----------------------------------------------------------????
Migration$symm <- as.factor(c(0, 12, 13, 14, 12, 0, 23, 24, 13, 23, 0,34, 14, 24, 34, 0))
matrix(Migration$symm, 4, 4)

# 6 residual (16 - 10) over parametized <----------------------------------------------------------????
glm(Fr ~ stay + symm, poisson, Migration)
matrix(Migration$stay, 4, 4)
# what does this percentage tell us?  <----------------------------------------------------------????
1 - pchisq(9.128, 6)

# macnemar test determines if there is a statistically significant difference in proportions between paired data
# if two off-diagnoal is the same (looking at the two cell at the same time), expected
mcnemar.test(matrix(Migration$Fr, 4, 4))

# is this Pearson X^2 statistics?? <----------------------------------------------------------????
sum(resid(glm(Fr ~ stay + symm, poisson, Migration), type = "pear")^2)

# Surrogate Log Linear Model: Example (Binomial Family)

# 12 different x values
budworm <- read.csv("./Purdue_STAT526/budworm.csv")

# Poisson y
budworm2 <- read.csv("./Purdue_STAT526/budworm2.csv")

is.factor(budworm2$dead)

# additive model
budwm.lgt <- glm(SF ~ ldose + sex, family = binomial, data = budworm)

# new method
## coefficients are just alpha for checking the a expected total and observed total are equal
## coefficent of dead1:ldose = that of ldose in budwm.lgt.
## dead1 = (Intercept) in budwm.lgt
budwm.log <- glm(Fr ~ id + dead * (ldose + sex) - 1, poisson, budworm2)
summary(budwm.log)
summary(budwm.lgt)


# Surrogate Log Linear Model: Example
detg <- read.csv("./Purdue_STAT526/detg.csv")
detg1 <- read.csv("./Purdue_STAT526/detg1.csv")

# Soft = softness of the water
# Temp = temperature of water
# M.user = whether the user uses the M brands or not
# all covariates are categorical
detg
is.ordered(detg$Soft)

# Set default contrasts
# what does this do?<----------------------------------------------------------????
# http://faculty.nps.edu/sebuttre/home/r/contrasts.html
options(contrasts = c("contr.treatment", "contr.treatment"))

# what does this mean? why are they meaningless here?<----------------------------------------------------------????
# full interaction of X margins gives us the alpha and only y for additive. Everything else are α (meaningless here)
detg.m0 <- glm(Fr ~ M.user * Temp * Soft + Brand, poisson, detg)

# modeling with only a constant
# 2 * 2 * 3 = 12 combinations
# what does this model tell us?? <----------------------------------------------------------????
detg1.m0 <- glm(cbind(X, M) ~ 1, family = binomial, data = detg1)

# we are interested in only aything related to y = BrandM
# all x are discrete. all interaction of x in the model achieve
# why these two different models' residual deviance are same?? <----------------------------------------------------------????
detg.m0
detg1.m0

# stepwise with AIC
detg.step <- step(detg.m0, scope = list(lower = ~., upper = ~.^2))
detg.step

# anything without Brands is useless here
# Brand depends on Temperature and User but not on Softness
## why AIC between detg.step and detg1.step are different?
detg1.step <- step(detg1.m0, trace = F, scope = list(upper = ~ M.user * Temp * Soft))
detg1.step

# surrogate model?? <----------------------------------------------------------????
# what is the purpose of this model??  <----------------------------------------------------------????
detg.log <- glm(terms(Fr ~ M.user * Temp * Soft + Brand * M.user * Temp, keep.order = T), family = poisson, data = detg)
detg.log

# should be the same as detg1.step
## why they are the same??  <----------------------------------------------------------????
detg.lgt <- update(detg1.m0, ~ + M.user * Temp)

# messy here but what we need to do is not fitting the model but 
# this approach allows us to find the multivariate discrete Y
summary(detg.log)
# why detg.lgt has better AIC?? meaning detg.log has too many unncessary parameters? <----------------------------------------------------------????
summary(detg.lgt)

# change the environment 
# what happens?? <----------------------------------------------------------????
options(contrasts = c("contr.sum", "contr.sum"))
det.log <- update(detg.log)


# Multivariate Responses: Examples

# i = 1~6, j = 1~4
# surrogate model?? <----------------------------------------------------------????
detg.mm0 <- glm(Fr ~ Temp * Soft + Brand + M.user, poisson, detg)
# stepwise
step(detg.mm0, list(lower = ~., upper = ~.^3))


# outcome is the same as stepwise why?? <----------------------------------------------------------????
detg.mm <- glm(terms(Fr ~ Temp * Soft + Brand * M.user + Temp:Brand, keep.order = TRUE), poisson, detg)

summary(detg.mm)
drop1(detg.mm)
anova(detg.mm)

ee <- fitted(detg.mm, "res")
# expected Poisson Count = 24 counts
ee

# store data in two dimensions
ee <- array(ee, c(2, 2, 2, 3))
ee

oo <- array(detg$Fr, c(2, 2, 2, 3))
oo
# swap 2nd and 3rd
ee <- aperm(ee, c(1, 3, 2, 4))
oo <- aperm(oo, c(1, 3, 2, 4))

# sum
apply(ee, c(3, 4), sum)
jk <- apply(oo, c(3, 4), sum)
jk

# keeping the temp, changing the softness. Probability
# divide observed count by observed total
ee[, , 1, 1] / jk[1, 1]
ee[, , 1, 2] / jk[1, 2]
ee[, , 1, 3] / jk[1, 3]
ee[, , 2, 3] / jk[2, 3]
ee[, , 2, 1] / jk[2, 1]

# calculate the log odds ratio
# if this is 0, it means independent, but 0.57 so its dependent
log(ee[1, 1, 1, 1] * ee[2, 2, 1, 1] / ee[1, 2, 1, 1] / ee[2, 1, 1, 1])
# change the temp level but the log-odds ratio is still same
log(ee[1, 1, 2, 1] * ee[2, 2, 2, 1] / ee[1, 2, 2, 1] / ee[2, 1, 2, 1])


# Multivariate Responses: Examples

miners <- read.csv("./Purdue_STAT526/miners.csv")
miners0 <- read.csv("./Purdue_STAT526/miners0.csv")

x <- miners0
plot(log((x[, 2] + x[, 3] + .5) / (x[, 4] + x[, 5] + .5)))
plot(log((x[, 2] + x[, 4] + .5) / (x[, 3] + x[, 5] + .5)))
plot(log((x[, 2] + .5) * (x[, 5] + .5) / (x[, 3] + .5) / (x[, 4] + .5)))

options(contrast = c("contr.treatment", "contr.treatment"))
# why -1?? <-----------------------------------------------------------------------------------------------------????
fit <- glm(Fr ~ Group + (Breath * Wheeze) * Age - 1, poisson, miners)
summary(fit)

# Fitting Distributions via Log Linear Models
boys <- read.csv("./Purdue_STAT526/boys.csv")

# modeling
# https://bit.ly/3yeBfXj
## lchoose() function computes the natural logarithm of combination function
boys.log <- glm(fr ~ boy, poisson, boys, offset = lchoose(6, boy))
boys.log
## set the prior weights on Frequency
boys.lgt <- glm(cbind(boy, girl) ~ 1, binomial, boys, weights = fr)
boys.lgt

# these two below are same
predict(boys.log, type = "res") / 72069
## plogis = ロジスティック分布の累積分布関数, dbinom = 二項分布の確率質量関数
dbinom(0:6, 6, plogis(boys.lgt$coef))
sum(resid(boys.log, "pear")^2) / 72069

# toward the end, it gets positive, but the middle it becomes negative. so we may need quadratic form.
plot(0:6, resid(boys.log, "work"))
boys.log2 <- glm(fr ~ boy + I(boy^2), poisson, boys, off = lchoose(6, boy))
boys.log2
# this one is more civilized
sum(resid(boys.log2, "pear")^2)

prob<-predict(boys.log2,type="res")/72069
mu <- sum((0:6)*prob)
mu
#p
#6*p
# 
sigmasq<-sum((0:6)^2*prob)-mu^2
sigmasq

# with only 6 parameters, this overdispersion is ignorable but with a large number of sample size, we cannot 
disp <- sigmasq/ (6*mu/6*(1-mu/6))
disp
sum(resid(boys.lgt,type="pear")^2)/72068




# Poisson Distribution vs Surrogate Model
y <- rpois(1000, 5)
fr <- as.vector(table(y))
fr
# table of 14 cells
table(y)
# n = 14
n <- length(fr)
# add 7 0s
fr <- c(fr, rep(0, 21 - n))

dat <- data.frame(y = 0:20, fr = fr)
# Coefficients values are same in below
glm(y ~ 1, poisson, dat, wei = fr)
log(sum((0:20) * fr) / 1000)

# using surrogate poisson model to one way table
# The first 14 cells following Poisson, but the latter 7 cells not following Poisson
glm(fr ~ y, poisson, dat, off = -lgamma(y + 1))

# truncated poisson
glm(fr ~ y, poisson, dat[2:21, ], off = -lgamma(y + 1))