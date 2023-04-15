library("stats")
library("foreign")
library("MASS")

# Linear Models

# Hwt = heart weight, Bwt = body weight
data(cats)

# modeling
catsF <- lm(Hwt ~ Bwt, data = cats, subset = Sex == "F")
catsM <- update(catsF, subset = Sex == "M")

print(catsF)
summary(catsF)
plot(catsF)

predict(catsF, new = list(Bwt = 3), se.fit = TRUE)
# these below are the same
coef(catsF); catsF$coef

deviance(catsF)
# these below are the same
fitted(catsF); catsF$fit
# these below are the same
resid(catsF); catsF$resid

# provides the basic quantities which are used in forming a wide variety of diagnostics for checking the quality of regression fits.
catsF.diag <- lm.influence(catsF)
catsF.diag$coef; catsF.diag$hat; catsF.diag$sigma

# Model Formulas
# constant model
cats0 <- lm(log(Hwt / Bwt) ~ 1, data = cats)
# update the model
cats1 <- update(cats0, .~. + Sex)
## interaction
cats2 <- update(cats1, .~. + Sex * log(Bwt))
# no significant difference between models
anova(cats0, cats1, cats2)
# interaction terms has a significant difference
anova(cats2)
# removing the intereaction slightly descreases AIC
drop1(cats2)

# Regression Diagnostics
# tells the potentially influential observations based on multiple indicators
catsF.infl <- influence.measures(catsF)
summary(catsF.infl)

dfbetas(catsF)
dffits(catsF) 
## covratio = covariance ratio
covratio(catsF)
cooks.distance(catsF)
rstandard(catsF)
rstudent(catsF)


# ANOVA, Factor Coding
## Wt = Litter average weight gain of the litter
data(genotype)
# anova
geno.fit <- aov(Wt ~ Litter + Mother, genotype)
summary(geno.fit)
# summarize Linear Model Fits
summary.lm(geno.fit)
# computes summary tables for model fits, especially complex aov fits.
model.tables(geno.fit, se = TRUE)

se.contrast(
    geno.fit, data = genotype, coef = c(1, -2, 1),
    contrast.obj = list(Litter == "A", Litter == "B", Litter == "J")
    )
# change the contrast coding scheme
options(contrast = c("contr.sum", "contr.sum"))

# Aliasing, Model Selection
## A classical N, P, K (nitrogen, phosphate, potassium) factorial experiment on the growth of peas conducted on 6 blocks
data(npk)
npk[npk$block == 1, ]
options(contrast = c("contr.treatment", "contr.poly"))
# Find aliases (linearly dependent terms) in a linear model specified by a formula.
alias(aov(yield ~ N * K + P, data = npk, sub = block == 1))
options(contrasts = c("contr.sum", "contr.sum"))
alias(aov(yield ~ N * K + P, data = npk, sub = block == 1))

## model selection
npk0 <- aov(yield ~ block, npk)
add1(npk0, ~. + N + P + K)
npk1 <- update(npk0, ~. + N + P + K)
step(npk1, scope = list(upper = ~. + (N + P + K)^3), lower = ~ block)

# Box-Cox Transformation: Example
data(quine)
quine
boxcox(
        Days + 1 ~ Eth * Sex * Age * Lrn, 
        data = quine, lambda = seq(-0.5, 0.45, len = 20)
    )
# Another family of transformations is y^alpha = log(y + alpha) with the corresponding profile likelihood
logtrans(Days ~ Eth * Sex * Age * Lrn, data = quine)
boxcox(Days + 2.5 ~ Eth * Sex * Age * Lrn, data = quine)
