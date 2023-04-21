library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("remotes")
library("rootdetectR")

# Q1
## ANOVA
## dataset
df <- data.frame(
        glass = c(rep(1, 9), rep(2, 9), rep(3, 9)),
        temp = rep(c(100, 100, 100, 125, 125, 125, 150, 150, 150), times = 3),
        light = c(
            58.0, 56.8, 57.0, 107, 106.7, 106.5, 129.2, 128.0, 128.6, 55, 53,
            57.9, 107, 103.5, 105, 117.8, 116.2, 109.9, 54.6, 57.5, 59.9, 106.5,
            107.3, 108.6, 101.7, 105.4, 103.9
            )
        )
head(df)

# factorize the variables
df$glass <- as.factor(df$glass)
df$temp <- as.factor(df$temp)
attach(df)
typeof(glass); typeof(temp)
class(glass); class(temp)

# ANOVA
model <- aov(light ~ glass + temp + glass * temp, data = df)
summary(model)


# break down the ANOVA
## grand mean
mean(light)

## mean diff by each glass categories (-> SSA)
df_glass <- df %>%
    group_by(glass) %>%
        summarise_at(vars(light), list(row_mean = mean)) %>%
            as.data.frame() %>%
                # mean(row_mean) = grand_mean
                mutate(diff_sqr_row = (row_mean - mean(row_mean))^2) %>%
                mutate(SSA = 3 * 3 * sum(diff_sqr_row))

## mean diff by each temperature categories (-> SSB)
df_temp <- df %>%
    group_by(temp) %>%
        summarise_at(vars(light), list(col_mean = mean)) %>%
            as.data.frame() %>%
                # mean(col_mean) = grand_mean
                mutate(diff_sqr_col = (col_mean - mean(col_mean))^2) %>%
                mutate(SSB = 3 * 3 * sum(diff_sqr_col))


## merge by {glass|temp|trt}
tmp1 <- merge(df, df_glass, by = "glass")
tmp2 <- merge(tmp1, df_temp, by = "temp")

## SSAB
### y_ij_bar
tmp3 <- tmp2 %>% group_by(glass, temp) %>% mutate(interaction_mean = mean(light))
## mean diff by interaction (-> SSinteraction)
tmp4 <- tmp3 %>%
    mutate(diff_interaction =
        ((interaction_mean + mean(tmp3$light) - row_mean - col_mean)^2))
tmp4$SSAB <- sum(tmp4$diff_interaction)

## SSE
tmp4$SSE <- tmp4 %>%
    mutate(diff_error = (light - interaction_mean)^2) %>%
    pull(diff_error) %>%
    sum()

## SST
mean(tmp4$light)
tmp4$SST <- tmp4 %>%
    mutate(diff_sqr = (light - 92.90741)^2) %>%
    pull(diff_sqr) %>%
    sum()


# Assumption check
## check normality assumption
qqnorm(tmp4$light, pch = 1, main = "Normal QQ-plot")
qqline(tmp4$light)


## Shapiro-Wilk normality test
shapiro.test(model$residuals)

## residual plot
### model prediction
### histogram and density plot

hist(model$residuals, col = "gray", prob = TRUE,
    main = "Histogram with Density plot")
lines(density(model$residuals))

g_1 <- ggplot(tmp4, aes(x = predict(model), y = model$residuals)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "Predicted Values", y = "Residuals",
        title = "Predicted values vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 8)
        )

g_2 <- ggplot(tmp4, aes(x = glass, y = model$residuals)) +
    geom_point() +
    labs(x = "Factor A", y = "Residuals",
        title = "Factor A vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
       axis.title = element_text(size = 8)
        )

g_3 <- ggplot(tmp4, aes(x = temp, y = model$residuals)) +
    geom_point() +
    labs(x = "Factor B", y = "Residuals",
        title = "Factor B vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
       axis.title = element_text(size = 8)
        )

g_4 <- ggplot(tmp4, aes(x = light, y = model$residuals)) +
    geom_point() +
    labs(x = "Response", y = "Residuals",
        title = "Response vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
       axis.title = element_text(size = 8)
        )

## multiple graph on 1 page
grid.arrange(g_1, g_2, g_3, g_4, ncol = 2, nrow = 2)

## interaction plot
interaction.plot(
    x.factor = tmp4$glass,
    trace.factor = tmp4$temp,
    response = tmp4$light,
    fun = mean,
    ylab = "Mean of y",
    xlab = "Glass Type",
    col = c("pink", "grey40", "blue"),
    lty = 1,
    lwd = 2,
    trace.label = "Temperature")

# Pairwise comparison
## Bonferroni
pairwise.t.test(tmp4$light, df$glass, p.adjust.method = "bonferroni")
## Tukey
tukey_to_matrix(TukeyHSD(model)$`glass:temp`)
## output to txt file
write.table(round(tukey_to_matrix(TukeyHSD(model)$`glass:temp`), 7), file="514/test.txt")


# regression model
## dataframe again
df <- data.frame(
        glass = c(rep(1, 9), rep(2, 9), rep(3, 9)),
        temp = rep(c(100, 100, 100, 125, 125, 125, 150, 150, 150), times = 3),
        light = c(
            58.0, 56.8, 57.0, 107, 106.7, 106.5, 129.2, 128.0, 128.6, 55, 53,
            57.9, 107, 103.5, 105, 117.8, 116.2, 109.9, 54.6, 57.5, 59.9, 106.5,
            107.3, 108.6, 101.7, 105.4, 103.9
            )
        )
## add contrasts for glass variable
x1 <- c(rep(1, 9), rep(0, 9), rep(-1, 9))
x2 <- c(rep(0, 9), rep(1, 9), rep(-1, 9))
## make temp to -1, 0, 1
t <- (df$temp - 125) / 25
## combine new columns
df <- cbind(df, x1, x2, t)

## run the model
lm <- lm(data = df, light ~ x1 + x2 + t + I(t^2) + x1*t + x2*t + x1*I(t^2) + x2*I(t^2))
summary(lm)





jk <- c(130,155,74,180,34,40,80,75,20,70,82,58,150,188,159,126,136,122,106,115,25,70,58,45,138,110,168,160,174,120,150,139,96,104,82,60)
sqrt(var(jk))
