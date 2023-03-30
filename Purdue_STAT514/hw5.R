library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")

#Q1
# ANOVA
df <- data.frame(
            agents = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5)),
            bolts = rep(c(1, 2, 3, 4, 5), times = 4),
            strength = c(73, 68, 74, 71, 67, 73, 67, 75, 72, 70, 75, 68, 78, 73, 68, 73, 71, 75, 75, 69)
)
# factorize the variables
df$agents <- as.factor(df$agents)
df$bolts <- as.factor(df$bolts)

# ANOVA
model <- aov(strength ~ agents + bolts, data = df)
summary(model)

# break down the ANOVA
## grand mean
attach(df)
mean(strength)

## mean diff by each agents categories
df %>% 
    group_by(agents) %>%
        summarise_at(vars(strength), list(means = mean)) %>%
            as.data.frame() %>%
                mutate(diff_sqr = (means - mean(means))^2) %>%
                mutate(diff_sum = sum(diff_sqr)) -> df_agents
## mean diff by each bolts categories
df %>% 
    group_by(bolts) %>%
        summarise_at(vars(strength), list(means = mean)) %>%
            as.data.frame() %>%
                mutate(diff_sqr = (means - mean(means))^2) %>%
                mutate(diff_sum = sum(diff_sqr)) -> df_bolts

## diff by each cell -> SSE
## merge by agents column
tmp1 <- merge(df, df_agents[, c(1, 2)], by = "agents")
tmp2 <- merge(tmp1, df_bolts[, c(1, 2)], by = "bolts")
tmp2 %>%
    mutate(
        diff_sqr = (strength - means.x - means.y + mean(strength))^2,
        diff_sum = sum(diff_sqr)
        )



# Assumption check
## check normality assumption
qqnorm(df$strength, pch = 1, main = "Normal QQ-plot")
qqline(df$strength)

## Shapiro-Wilk normality test
shapiro.test(model$residuals)

## residual plot
### model prediction
### histogram and density plot

hist(model$residuals, col = "gray", prob = TRUE, 
    main = "Histogram with Density plot")
lines(density(model$residuals))

g_1 <- ggplot(df, aes(x = predict(model), y = model$residuals)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "Predicted Values", y = "Residuals",
        title = "Predicted values vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5))

g_2 <- ggplot(df, aes(x = agents, y = model$residuals)) +
    geom_point() +
    labs(x = "Treatments", y = "Residuals",
        title = "Treatments vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5))

g_3 <- ggplot(df, aes(x = bolts, y = model$residuals)) +
    geom_point() +
    labs(x = "Blocks", y = "Residuals",
        title = "Blocks vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5))

# multiple graph on 1 page
grid.arrange(g_1, g_2, g_3, ncol = 2, nrow = 2)

# Pairwise comparison
## Tukey
TukeyHSD(model, conf.level = .95)

## Bonferroni
pairwise.t.test(df$strength, df$agents, p.adjust.method = "bonferroni")



# Q2
# dataframe
df2 <- data.frame(
            temp = c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5)),
            oven = rep(c(1, 2, 3, 4, 5), times = 4),
            strength = c(3, 10, 7, 4, 3, 3, 8, 12, 2, 4, 9, 
                        13, 15, 3, 10, 7, 12, 9, 8, 13)
            )

# factorize the variables
df2$temp <- as.factor(df2$temp)
df2$oven <- as.factor(df2$oven)

## ANOVA
model2 <- aov(strength ~ temp + oven, data = df2)
summary(model2)

## Tukey method
TukeyHSD(model2, conf.level = .95)


# Q3
df3 <- data.frame(
            detergent = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)),
            stain = rep(c(1, 2, 3), times = 4),
            cleaness = c(45, 43, 51, 47, 46, 52, 48, 50, 55, 42, 37, 49)
        )
# factorize the variables
df3$detergent <- as.factor(df3$detergent)
df3$stain <- as.factor(df3$stain)

# ANOVA
model3 <- aov(cleaness ~ detergent + stain, data = df3)
summary(model3)

pred <- predict(lm(cleaness ~ detergent + stain), df3, se.fit = TRUE)
vec <- c()
vec <- pred$fit
vec2 <- vec^2
df3 <- data.frame(
            y = vec,
            q = vec2
        )

# break down the ANOVA
## grand mean
attach(df3)
mean(cleaness)

## mean diff by each detergent (treatment) categories
df3 %>% 
    group_by(detergent) %>%
        summarise_at(vars(cleaness), list(det_means = mean)) %>%
            as.data.frame() -> df_detergent
## mean diff by each stain (block) categories
df3 %>% 
    group_by(stain) %>%
        summarise_at(vars(cleaness), list(means = mean)) %>%
            as.data.frame() %>%
                mutate(diff_sqr = (means - mean(means))^2) %>%
                mutate(diff_sum = sum(diff_sqr)) -> df_stain

## diff by each cell -> SSE
## merge by agents column
tmp1 <- merge(df3, df_detergent[, c(1, 2)], by = "detergent")
tmp2 <- merge(tmp1, df_stain[, c(1, 2)], by = "stain")
tmp2 %>%
    mutate(
        three_multiple = (cleaness * means.x * means.y),
        ss = mean(cleaness) * (110.92 + 135.17 + (mean(cleaness)^2 / (3 * 2))),
        numerator = (three_multiple - ss)^2,
        denominator = ((3 * 2) * 110.92 * 135.17),
        SSn = sum(numerator / denominator)
        )

tmp2 %>%
    mutate(
        three_multiple = (cleaness * means.x * means.y),
        ss = mean(cleaness),
        s =  (110.92 + 135.17 + (mean(cleaness)^2 / (3 * 2))),
        sss = ss * s)
tmp2 %>%
    mutate(
        three_multiple = (cleaness * means.x * means.y),
        sums = sum(three_multiple),
        ss = mean(cleaness) * (110.92 + 135.17 + (mean(cleaness)^2 / (3 * 2))),
        numerator = (sums - ss)^2,
        denominator = ((3 * 2) * 110.92 * 135.17),
        SSn = sum(numerator / denominator)
        )
