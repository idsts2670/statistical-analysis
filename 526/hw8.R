
# Q1
diet <- read.csv("/Users/satoshiido/Documents/statistical-analysis/526/diet.csv")
head(diet)
# Q2
ovarian <- read.csv("/Users/satoshiido/Documents/statistical-analysis/526/ovarian.csv")
head(ovarian)
# Q3
veteran <- read.csv("/Users/satoshiido/Documents/statistical-analysis/526/veteran.csv")
head(veteran)

library("stats4")
library("survival")
library("survminer")


# Q1
# fit a Kaplan-Meier and plot it
km.fit <- survfit(Surv(futime, fustat) ~ fat, data = diet[31:90, ])
plot(km.fit, main = "Survival Proportions: ")
ggsurvplot(km.fit, conf.type = "log-log")

# Cox model
cox.fit <- coxph(Surv(futime, fustat) ~ fat, data = diet[31:90, ])
cox.fit

# Q1-b
# logrank test
surv_diff <- survdiff(Surv(futime, fustat) ~ fat, data = diet[31:90, ])
surv_diff

# Q1-c
# logrank test
surv_diff2 <- survdiff(Surv(futime, fustat) ~ fat, data = diet)
surv_diff2



# Q2

# Q2-a
km2.fit <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
tmp <- survreg(Surv(futime, fustat) ~ rx, data = ovarian)

ggsurvplot(km2.fit,
        conf.int = TRUE,
        conf.type = "plain",
        title = "Plot of Kaplan-Meier with CI type = 'plain'"
        )

ggsurvplot(km2.fit,
        conf.int = TRUE,
        conf.type = "log",
        title = "Plot of Kaplan-Meier with CI type = 'log'"
        )

ggsurvplot(km2.fit,
        conf.int = TRUE,
        fun = "cloglog",
        title = "Plot of Kaplan-Meier with CI type = 'log-log'"
        )

# Q2-b
wei <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
plot(wei,
    fun = "cloglog",
    main = "a log-log plot of t-lambda(t) in the two trt groupss")

# Q2-c
## "ecog.ps" should be dropped according to the output
wei2 <- survreg(
        formula = Surv(futime, fustat) ~ age + ecog.ps + strata(rx),
        data = ovarian,
        dist = "weibull"
        )
summary(wei2)

## comparing the startified model and regular model
wei3_a <- survreg(
        formula = Surv(futime, fustat) ~ age + rx,
        data = ovarian,
        scale = 0.5
        )
summary(wei3_a)

wei3_b <- survreg(
        formula = Surv(futime, fustat) ~ age + strata(rx),
        data = ovarian
        )
summary(wei3_b)


# Q3
# which pertains to Veterans' Administration Lung Cancer study
head(veteran)

# factorize the variables
veteran$trt <- as.factor(veteran$trt)
veteran$prior <- as.factor(veteran$prior)
veteran$celltype <- as.factor(veteran$celltype)
attach(veteran)
typeof(trt); typeof(celltype); typeof(prior)
class(trt); class(celltype); class(prior)

# start from constant model
km1 <- survfit(Surv(time = time, event = status) ~ 1, data = veteran)
# survival curve
ggsurvplot(km1,
        conf.int = TRUE,
        conf.type = "log",
        title = "Plot of Constant Kaplan-Meier model"
        )
survreg(Surv(time = time, event = status) ~ 1, data = veteran)

# model by trt group (1 = standard 2 = test)
km2 <- survfit(Surv(time = time, event = status) ~ trt, data = veteran)
# survival curve
ggsurvplot(km2,
        conf.int = TRUE,
        pval = TRUE,
        conf.type = "log",
        title = "Plot of Kaplan-Meier model by trt group"
        )
# Log-rank test
surv_diff2 <- survdiff(Surv(time, status) ~ trt, data = veteran)
surv_diff2


# model by prior therapy (0 = no 10 = yes)
# prior therapy (whether a patient received treatment before the recent one)
km3 <- survfit(Surv(time, status) ~ prior, data = veteran)
# survival curve
ggsurvplot(km3,
        conf.int = TRUE,
        pval = TRUE,
        conf.type = "log",
        title = "Plot of Kaplan-Meier model by prior group"
        )
# Log-rank test
surv_diff3 <- survdiff(Surv(time, status) ~ prior, data = veteran)
surv_diff3


# model by celltype
# cell types of the tumor (1=squamous, 2=smallcell, 3=adeno, 4=large)
km4 <- survfit(Surv(time, status) ~ celltype, data = veteran)
tmp <- survreg(Surv(time, status) ~ celltype, data = veteran)
# survival curve
ggsurvplot(km4,
        title = "Plot of Kaplan-Meier model by celltype group"
        )
# Log-rank test
surv_diff4 <- survdiff(Surv(time, status) ~ celltype, data = veteran)
surv_diff4


# Cox-model (complex survival)
cox1 <- coxph(
        Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior,
        data = veteran
        )
summary(cox1)

# visualize the estimated distribution of survival times
ggsurvplot(survfit(cox1, data = veteran),
        title = "Plot of Cox Proportional-Hazards Model"
        )

# assumption check part1
## PH assumption check
test.ph <- cox.zph(cox1)
### a graphical diagnostic using the function ggcoxzph()
ggcoxzph(test.ph)

ggcoxdiagnostics(cox1, type = "dfbeta",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw(),
                 main = "Index plots of dfbeta for the Cox regression of time to lung cancer on every variables")

# Log-rank test
surv_diff5 <- survdiff(
        Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior,
        data = veteran)
surv_diff5

# Remodeling using the stratification on "celltype"
cox2 <- coxph(
        Surv(time, status) ~ trt + strata(celltype) + karno + diagtime + age + prior,
        data = veteran
        )
summary(cox2)

# assumption check part2
## PH assumption check
test.ph2 <- cox.zph(cox2)
### a graphical diagnostic using the function ggcoxzph()
ggcoxzph(test.ph2)

ggcoxdiagnostics(cox2, type = "dfbeta",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw(),
                 main = "Index plots of dfbeta for the Cox regression of time to lung cancer on every variables")


# Remodeling using the stratification on "celltype" and "prior"
cox3 <- coxph(
        Surv(time, status) ~ trt + strata(celltype) + karno + diagtime + age + strata(prior),
        data = veteran
        )
summary(cox3)

# assumption check part3
## PH assumption check
test.ph3 <- cox.zph(cox3)
### a graphical diagnostic using the function ggcoxzph()
ggcoxzph(test.ph3)

ggcoxdiagnostics(cox3, type = "dfbeta",
                 linear.predictions = FALSE,
                 ggtheme = theme_bw(),
                 main = "Index plots of dfbeta for the Cox regression of time to lung cancer on every variables")


# comparison of two models
anova(cox1, cox2); anova(cox2, cox3); anova(cox1, cox3)
AIC(cox1); AIC(cox2); AIC(cox3)
logLik(cox1); logLik(cox2); logLik(cox3)
lrtest(cox1, cox2); lrtest(cox2, cox3)
