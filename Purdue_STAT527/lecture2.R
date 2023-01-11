# Linear Models
library("MASS"); data(cats)

# linear model can be fitted using the function lm
catsF <- lm(Hwt ~ Bwt, data = cats, sub = Sex == "F") # nolint

catsM <- update(catsF, subset = Sex == "M") # nolint

## subtract the detail info
print(catsF); summary(catsF); plot(catsF)
predict(catsF, new = list(Bwt = 3), se.fit = TRUE)
coef(catsF); catsF$coef; deviance(catsF)
fitted(catsF); catsF$fit; resid(catsF); catsF$resid
catsF.diag <- lm.influence(catsF)
catsF.diag$coef; catsF.diag$hat; catsF.diag$sigma


# Model Formulas
## Models can be updated and extra sum of squares obtained
cats0 <- lm(log(Hwt / Bwt)~1, data = cats)
cats1 <- update(cats0, .~. + Sex)
### Sex * log(Bwt) indicates their interaction
cats2 <- update(cats1, .~. + Sex * log(Bwt))
### RSS is type I SS
anova(cats0, cats1, cats2)
### show the familiar outputs
anova(cats2)
### show the outputs if we drop any one of variables (but if we have interactions, we are suggested to drop only interactions)
drop1(cats2)