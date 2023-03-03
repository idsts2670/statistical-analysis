# Poisson Regression Example
library("stats")
library("foreign")
library("MASS")

data(ships, package = "MASS")
ships$year <- factor(ships$year)
class(ships$year)
class(ships$period)
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
# why ^2?? where 13 come from?? Is it variance λ = 13?? <----------------------------------------------------------????
# Understanding Deviance Residuals https://bit.ly/41F8jW0
# Pearson's chi^2 statistics??
disp <- sum(resid(ships.log1, type = "pear")^2) / 13
# pchisq() computes cumulative chi square density for a vector of elements
1 - pchisq((38.695 - 14.587) / disp, 12)


# Negative Binomial Examples
# What is the positive points of this model over logistic model? <----------------------------------------------------------????
ships.nb <- update(ships.log, family = neg.bin(20))
summary(ships.nb)

# why the two results below are the same even though one is using offset and the other dont? <----------------------------------------------------------????
ships.nb1 <- glm.nb(incidents ~ type + year + period + log(service), data = ships, subset = service > 0)
ships.nb2 <- glm.nb(ships.log0)
# the correlation matrix of the estimated parameters is returned and printed
summary(ships.nb1, cor = TRUE)




"Migration" <-
structure(list(move66 = structure(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 
2, 3, 4, 1, 2, 3, 4), .Label = c("CC", "ULY", "WM", "GL"), class = "factor"), 
    move71 = structure(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 
    4, 4, 4, 4), .Label = c("CC", "ULY", "WM", "GL"), class = "factor"), 
    Fr = c(118, 14, 8, 12, 12, 2127, 69, 110, 7, 86, 2548, 88, 
    23, 130, 107, 7712)), .Names = c("move66", "move71", "Fr"
), row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", 
"10", "11", "12", "13", "14", "15", "16"), class = "data.frame")

Migration
Migration$stay <- as.factor(diag(1:4))

matrix(Migration$Fr, 4, 4)
matrix(Migration$stay, 4, 4)

M.S.fit <- glm(Fr ~ stay + move66 + move71, poisson, Migration)
jk9 <- predict(M.S.fit, data.frame(move66 = Migration$move66, move71 = Migration$move71, stay = as.factor(0), type = "res")) # nolint

jk8 <- matrix(jk9, 4, 4)
jk8[,1]/jk8[,2]

sum(jk9)/sum(Migration$Fr)

matrix(fitted(M.S.fit, "res"), 4, 4)

matrix(resid(M.S.fit, "res"), 4, 4)

matrix(resid(M.S.fit, "res"), 4, 4)

Migration$symm <- as.factor(c(0, 12, 13, 14, 12, 0, 23, 24, 13, 23, 0,34, 14, 24, 34,0)) # nolint
matrix(Migration$symm, 4, 4)
# 6 residual (16 - 10) over parametized
glm(Fr~stay+symm,poisson,Migration)
matrix(Migration$stay, 4, 4)
1 - pchisq(9.128, 6)

# macnemar test. it only look at the 
# if two off-diagnoal is the same (looking at the two cell at the same time), expected  # nolint
mcnemar.test(matrix(Migration$Fr,4,4))

# what is this???
sum(resid(glm(Fr ~ stay + symm, poisson, Migration), "pear")^2)


"boys" <-
structure(list(boy = c(0, 1, 2, 3, 4, 5, 6), girl = c(6, 5, 4, 
3, 2, 1, 0), fr = c(1096, 6233, 15700, 22221, 17332, 7908, 1579
)), .Names = c("boy", "girl", "fr"), row.names = c("1", "2", 
"3", "4", "5", "6", "7"), class = "data.frame")
boys
boys.log<-glm(fr~boy,poisson,boys,off=lchoose(6,boy))
boys.log
boys.lgt<-glm(cbind(boy,girl)~1,binomial,boys,wei=fr)
boys.lgt
# these two below are same
predict(boys.log,type="res")/72069
dbinom(0:6,6,plogis(boys.lgt$coef))
sum(resid(boys.log, "pear")^2)

# toward the end, it gets positive, but the middle it becomes negative. so we may need quadratic form.
plot(0:6, resid(boys.log, "work"))
boys.log2<-glm(fr~boy+I(boy^2),poisson, boys,off=lchoose(6,boy))
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


# Binomial Family
# budworm data input from MASS book
# 12 different x values
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

# Poisson y
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

is.factor(budworm2$dead)

budworm
budworm2

# previous model
budwm.lgt <- glm(SF ~ ldose + sex, family = binomial, data = budworm)
budwm.lgt

# new method
## coefficients are just alpha for checking the a expected total and observed total are equal
## coefficent of dead1:ldose = that of ldose in budwm.lgt.
## dead1 = (Intercept) in budwm.lgt
budwm.log <- glm(Fr ~ id + dead * (ldose + sex) - 1, poisson, budworm2)
budwm.log


"detg" <-
structure(list(Brand = structure(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 
2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2), .Label = c("X", 
"M"), class = "factor"), Temp = structure(c(1, 1, 2, 2, 1, 1, 
2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2), .Label = c("Low", 
"High"), class = "factor"), M.user = structure(c(1, 1, 1, 1, 
2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2), .Label = c("N", 
"Y"), class = "factor"), Soft = structure(c(3, 3, 3, 3, 3, 3, 
3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1), .Label = c("Soft", 
"Medium", "Hard"), class = c("ordered", "factor")), Fr = c(68, 
42, 42, 30, 37, 52, 24, 43, 66, 50, 33, 23, 47, 55, 23, 47, 63, 
53, 29, 27, 57, 49, 19, 29)), .Names = c("Brand", "Temp", "M.user", 
"Soft", "Fr"), row.names = c("1", "2", "3", "4", "5", "6", "7", 
"8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", 
"19", "20", "21", "22", "23", "24"), class = "data.frame")
"detg1" <-
structure(list(Temp = structure(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 
2, 1, 2), class = "factor", .Label = c("Low", "High")), M.user = structure(c(1, 
1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2), class = "factor", .Label = c("N", 
"Y")), Soft = structure(c(3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1), class = c("ordered", 
"factor"), .Label = c("Soft", "Medium", "Hard")), M = c(42, 30, 
52, 43, 50, 23, 55, 47, 53, 27, 49, 29), X = c(68, 42, 37, 24, 
66, 33, 47, 23, 63, 29, 57, 19)), .Names = c("Temp", "M.user", 
"Soft", "M", "X"), row.names = c("1", "3", "5", "7", "9", "11", 
"13", "15", "17", "19", "21", "23"), class = "data.frame")

# Soft = softness of the water`
# Temp = temperature of water`
# M.user = whether the user uses the M brands or not
# all covariates are categorical
detg
is.ordered(detg$Soft)

options(contrasts = c("contr.treatment", "contr.treatment"))

# full interaction of X margins gives us the alpha and only y for additive. Everything else are α (meaningless here)
detg.m0 <- glm(Fr ~ M.user * Temp * Soft + Brand, poisson, detg)
# 2 * 2 * 3 = 12 combinations
detg1.m0 <- glm(cbind(X, M) ~ 1, family = binomial, data = detg1)

# we are interested in only aything related to y = BrandM
# all x are discrete. all interaction of x in the model achieve
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

detg.log <- glm(terms(Fr ~ M.user * Temp * Soft + Brand * M.user * Temp, keep.order = T),family = poisson, data = detg) # nolint
detg.log

# should be the same as detg.
detg.lgt <- update(detg1.m0, ~ + M.user * Temp)

# messy here but what we need to do is not fitting the model but 
# this approach allows us to find the multivariate discrete Y
summary(detg.log)
summary(detg.lgt)

# change the environment
options(contrasts = c("contr.treatmment", "contr.treatmment"))
det.log = update(det.log)

# change everything
options(contrasts = c("contr.sum", "contr.sum"))

# i = 1~6, j = 1~4
detg.mm0 <- glm(Fr ~ Temp * Soft + Brand + M.user, poisson, detg)
# stepwise
step(detg.mm0, list(lower = ~., upper = ~.^3))

# outcome is the same as stepwise but we 
detg.mm<-glm(terms(Fr ~ Temp * Soft + Brand * M.user + Temp:Brand, keep.order = TRUE), poisson, detg)

ee <- fitted(detg.mm, "res")
# expected Poisson Count
ee

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
