library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("remotes")
library("rootdetectR")

# Q1
## dataset
df <- data.frame(
        loom = c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5)),
        output = c(
            14.0, 14.1, 14.2, 14.0, 14.1,
            13.9, 13.8, 13.9, 14.0, 14.0,
            14.1, 14.2, 14.1, 14.0, 13.9,
            13.6, 13.8, 14.0, 13.9, 13.7,
            13.8, 13.6, 13.9, 13.8, 14.0
            )
        )
head(df)

## (a)
## ANOVA
model <- aov(output ~ loom, data = df)
summary(model)


# Q2
## dataset
df2 <- data.frame(
        part = c(
                rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6),
                rep(6, 6), rep(7, 6), rep(8, 6), rep(9, 6), rep(10, 6)
                ),
        operator = rep(c(1, 1, 1, 2, 2, 2), times = 10),
        output = c(
            50, 49, 50, 50, 48, 51,
            52, 52, 51, 51, 51, 51,
            53, 50, 50, 54, 52, 51,
            49, 51, 50, 48, 50, 51,
            48, 49, 48, 48, 49, 48,
            52, 50, 50, 52, 50, 50,
            51, 51, 51, 51, 50, 50,
            52, 50, 49, 53, 48, 50,
            50, 51, 50, 51, 48, 49,
            47, 46, 49, 46, 47, 48
            )
        )
head(df2)
# (a)
attach(df2)

# break down the ANOVA
## grand mean
mean(output)

## mean diff by each part categories (-> SStao)
df_part <- df2 %>%
    group_by(part) %>%
        summarise_at(vars(output), list(row_mean = mean)) %>%
            as.data.frame() %>%
                # mean(row_mean) = grand_mean
                mutate(diff_sqr_row = (row_mean - mean(row_mean))^2) %>%
                mutate(SStao = sum(diff_sqr_row))

## mean diff by each operator categories (-> SSbeta)
df_operator <- df2 %>%
    group_by(operator) %>%
        summarise_at(vars(output), list(col_mean = mean)) %>%
            as.data.frame() %>%
                # mean(col_mean) = grand_mean
                mutate(diff_sqr_col = (col_mean - mean(col_mean))^2) %>%
                mutate(SSbeta = sum(diff_sqr_col))


## merge by {part|operator}
tmp1 <- merge(df2, df_part, by = "part")
tmp2 <- merge(tmp1, df_operator, by = "operator")

## SStaobeta
### y_ij_bar
tmp3 <- tmp2 %>% group_by(part, operator) %>% mutate(interaction_mean = mean(output))
### mean diff by interaction (-> SSinteraction = SStaobeta)
tmp4 <- tmp3 %>%
    mutate(diff_sqr_interaction =
        ((interaction_mean + mean(tmp3$output) - row_mean - col_mean)^2))
tmp4$SStaobeta <- sum(tmp4$diff_sqr_interaction)

## SSE
tmp4$SSE <- tmp4 %>%
    mutate(diff_sqr_error = (output - interaction_mean)^2) %>%
    pull(diff_sqr_error) %>%
    sum()

## SST
mean(tmp4$output)
tmp4$SST <- tmp4 %>%
    mutate(diff_sqr = (output - 49.95)^2) %>%
    pull(diff_sqr) %>%
    sum()


