library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")

#Q1
## ANOVA
df <- data.frame(
            agents = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5)),
            bolts = rep(c(1, 2, 3, 4, 5), times = 4),
            strength = c(73, 68, 74, 71, 67, 73, 67, 75, 72, 70, 75, 68, 78, 73, 68, 73, 71, 75, 75, 69)
)
# factorize the variables
df$agents <- as.factor(df$agents)
df$bolts <- as.factor(df$bolts)

## ANOVA
res.aov <- aov(strength ~ agents + bolts, data = df)
summary(res.aov)

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
df2 <- merge(df, df_agents[, c(1, 2)], by = "agents")
df3 <- merge(df2, df_bolts[, c(1, 2)], by = "bolts")
df3 %>%
    mutate(diff_sqr = (strength - means.x - means.y + mean(strength))^2) %>%
        mutate(diff_sum = sum(diff_sqr))



# Assumption check
## density plot
hist(df$strength, col = "gray", prob = TRUE, main = "Histogram with Density plot")
lines(density(df$strength))

## check normality assumption
qqnorm(df$strength, pch = 1, main = "Normal QQ-plot")
qqline(df$strength)
                
## Shapiro-Wilk normality test
shapiro.test(df$strength)

## residual plot
### model prediction
model <- lm(strength ~ agents + bolts, data = df)
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
