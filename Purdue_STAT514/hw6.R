library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")


# Q1
## ANOVA
## dataset
df <- data.frame(
        order = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)),
        operator = rep(c(1, 2, 3, 4), times = 4),
        trt = c("c", "d", "a", "b", "b", "c", "d", "a", "a", "b", "c", "d", "d", "a", "b", "c"),
        time = c(10, 14, 7, 8, 7, 18, 11, 8, 5, 10, 11, 9, 10, 10, 12, 14)
        )
head(df)

# factorize the variables
df$order <- as.factor(df$order)
df$operator <- as.factor(df$operator)
df$trt <- as.factor(df$trt)
attach(df)
typeof(order); typeof(operator); typeof(trt)
class(order); class(operator); class(trt)


# ANOVA
model <- aov(time ~ operator + order + trt, data = df)
summary(model)


# break down the ANOVA
## grand mean
mean(time)

## mean diff by each order categories (-> SSrow)
df %>%
    group_by(order) %>%
        summarise_at(vars(time), list(row_mean = mean)) %>%
            as.data.frame() %>%
                # mean(row_mean) = grand_mean
                mutate(diff_sqr_row = (row_mean - mean(row_mean))^2) %>%
                mutate(SSrow = sum(diff_sqr_row)) -> df_order

## mean diff by each order categories (-> SStrt)
df %>%
    group_by(trt) %>%
        summarise_at(vars(time), list(trt_mean = mean)) %>%
            as.data.frame() %>%
                # mean(trt_mean) = grand_mean
                mutate(diff_sqr_trt = (trt_mean - mean(trt_mean))^2) %>%
                mutate(SStrt = sum(diff_sqr_trt)) -> df_trt

## mean diff by each operator categories (-> SScolumn)
df %>% 
    group_by(operator) %>%
        summarise_at(vars(time), list(col_mean = mean)) %>%
            as.data.frame() %>%
                # mean(col_mean) = grand_mean
                mutate(diff_sqr_col = (col_mean - mean(col_mean))^2) %>%
                mutate(SScol = sum(diff_sqr_col)) -> df_operator

## diff by each cell -> SSE
## merge by {order|operator|trt}
tmp1 <- merge(df, df_order, by = "order")
tmp2 <- merge(tmp1, df_operator, by = "operator")
tmp3 <- merge(tmp2, df_trt, by = "trt")

tmp3 %>%
    mutate(
        SST = sum((time - mean(time))^2),
        SSE = SST - (nlevels(df$order) * SSrow) - (nlevels(df$trt) * SStrt) - (nlevels(df$operator) * SScol)
        )


# Assumption check
## check normality assumption
qqnorm(df$time, pch = 1, main = "Normal QQ-plot")
qqline(df$time)

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
    theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 8)
        )

g_2 <- ggplot(df, aes(x = trt, y = model$residuals)) +
    geom_point() +
    labs(x = "Treatments", y = "Residuals",
        title = "Treatments vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
       axis.title = element_text(size = 8)
        )

g_3 <- ggplot(df, aes(x = order, y = model$residuals)) +
    geom_point() +
    labs(x = "Row Blocks", y = "Residuals",
        title = "Row Blocks vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
       axis.title = element_text(size = 8)
        )

g_4 <- ggplot(df, aes(x = operator, y = model$residuals)) +
    geom_point() +
    labs(x = "Column Blocks", y = "Residuals",
        title = "Column Blocks vs Residuals") +
    theme(plot.title = element_text(hjust = 0.5, size = 10),
       axis.title = element_text(size = 8)
        )

# multiple graph on 1 page
grid.arrange(g_1, g_2, g_3, g_4, ncol = 2, nrow = 2)
