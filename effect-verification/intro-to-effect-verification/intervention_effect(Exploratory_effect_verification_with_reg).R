# Exploratory effect verification

# library and data
# get the experimentdatar from Github
# コロンビアで行われた私立学校の学費の割引に関する実験分析の研究のデータ
# install.packages("devtools")
# remotes::install_github("itamarcaspi/experimentdatar")
library("experimentdatar")
library("broom")
library("tidyverse")
data(vouchers)
vouchers

# recreation of Angrist(2002) Table 3. bogota 1995
# prepare for the regression
## set character strings for regression
formula_x_base <- "VOUCH0"

formula_x_covariate <- "SVY + HSVISIT + AGE + STRATA1 + STRATA2
  + STRATA3 + STRATA4 + STRATA5 + STRATA6 + STRATAMS
  + D1993 + D1995 + D1997 + DMONTH1 + DMONTH2 + DMONTH3
  + DMONTH4 + DMONTH5 + DMONTH6 + DMONTH7 + DMONTH8 + DMONTH9
  + DMONTH10 + DMONTH11 + DMONTH12 + SEX2"

formula_y <- c("TOTSCYRS", "INSCHL", "PRSCH_C", "USNGSCH",
  "PRSCHA_1", "FINISH6", "FINISH7", "FINISH8", "REPT6", "REPT", "NREPT",
  "MARRIED", "HASCHILD", "HOURSUM", "WORKING3")

## regression for formula_y without covariances
base_reg_formula <- paste(formula_y, "~", formula_x_base)
names(base_reg_formula) <- paste(formula_y, "base", sep = "_")

## regression for formula_y with covariances
covariate_reg_formula <-
    paste(formula_y, "~", formula_x_base, "+", formula_x_covariate)
names(covariate_reg_formula) <-
    paste(formula_y, "covariate", sep = "_")

## vector of model formula
table3_formula <- c(base_reg_formula, covariate_reg_formula)

## convert vector into dataframe
models <- table3_formula %>%
    enframe(name = "model_index", value = "formula")

# regression
## extract bogota 1995 data
regression_data <- vouchers %>% filter(TAB3SMPL == 1, BOG95SMP == 1)

## all regression
df_models <- models %>%
    mutate(model = map(.x = formula,  .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

## model output's reorganization
df_results <- df_models %>%
    mutate(formula = as.character(formula)) %>%
    select(formula, model_index, lm_result) %>%
    unnest(cols = c(lm_result))

# analysis of voucher usage and commuting to private school
# PRSCHA_1 = if attending private schools USNGSCH = if using some vouchers
# usage of vouchers and enrollment ratio
using_voucher_results <- df_results %>%
    filter(term == "VOUCH0", str_detect(model_index, "PRSCHA_1|USNGSCH")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

## display the results using ggplot
using_voucher_results %>%
  ggplot(aes(y = estimate, x = model_index)) +
  geom_point() +
  geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                    ymin = estimate - std.error * 1.96,
                    width = 0.1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

# display retention tendency
# get effects on PRSCH_C, INSCHL, FINISH6-8, REPT by VOUCH0
going_private_results <- df_results %>%
    filter(term == "VOUCH0",
           str_detect(model_index, "PRSCH_C|INSCHL|FINISH|REPT")) %>%
    select(model_index, term, estimate, std.error, p.value) %>%
    arrange(model_index)

# ggplot
going_private_results %>%
    filter(str_detect(model_index, "covariate")) %>%
    ggplot(aes(y = estimate, x = model_index)) +
    geom_point() +
    geom_errorbar(aes(ymax = estimate + std.error * 1.96,
                      ymin = estimate - std.error * 1.96,
                      width = 0.1)) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          plot.margin = margin(0.5, 1, 0.5, 1, "cm"))

# difference of effect between gender
# recreation Angrist(2002)のTable.4 & 6 bogota 1995
# extract data
data_tbl4_bog95 <- vouchers %>%
    filter(BOG95SMP == 1, TAB3SMPL == 1,
           !is.na(SCYFNSH), !is.na(FINISH6), !is.na(PRSCHA_1),
         !is.na(REPT6), !is.na(NREPT), !is.na(INSCHL),
         !is.na(FINISH7),
         !is.na(PRSCH_C), !is.na(FINISH8), !is.na(PRSCHA_2),
         !is.na(TOTSCYRS), !is.na(REPT)
    ) %>%
    select(VOUCH0, SVY, HSVISIT, DJAMUNDI, PHONE, AGE,
         STRATA1:STRATA6, STRATAMS, DBOGOTA, D1993, D1995, D1997,
         DMONTH1:DMONTH12, SEX_MISS, FINISH6, FINISH7, FINISH8,
         REPT6, REPT, NREPT, SEX2, TOTSCYRS, MARRIED, HASCHILD,
         HOURSUM, WORKING3, INSCHL, PRSCH_C, USNGSCH, PRSCHA_1)

# regression with data of female students
## data extraction
regression_data <- data_tbl4_bog95 %>% filter(SEX2 == 0)

## regression all
df_models <- models %>%
    mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

## reorganize model result
df_results_female <- df_models %>%
    mutate(formula = as.character(formula),
           gender = "female") %>%
    select(formula, model_index, lm_result, gender) %>%
    unnest(cols = c(lm_result))

# regression with data of male students
## data extraction
regression_data <- data_tbl4_bog95 %>% filter(SEX2 == 1)

## regression all
df_models <- models %>%
    mutate(model = map(.x = formula, .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

## reorganize model result
df_results_male <- df_results %>%
    mutate(formula = as.character(formula),
           gender = "male") %>%
    select(formula, model_index, lm_result, gender) %>%
    unnest(cols = c(lm_result))
