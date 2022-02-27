# Exploratory effect verification

# library and data
# get the experimentdatar from Github
# コロンビアで行われた私立学校の学費の割引に関する実験分析の研究のデータ
# remotes::install_github("itamarcaspi/experimentdatar") # nolint
library(experimentdatar)
library(broom)
library(tidyverse)
data(vouchers)
vouchers

## prepare for the regression
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
covariate_reg_formula <- paste(
  formula_y, "~", formula_x_base, "+", formula_x_covariate)
names(covariate_reg_formula) <- paste(formula_y, "covariate", sep = "_")

## vector of model formula
table3_formula <- c(base_reg_formula, covariate_reg_formula)

## convert vector into dataframe
models <- table3_formula %>%
    enframe(name = "model_index", value = "formula")

## regression
### extract bogota 1995 data
regression_data <- vouchers %>% filter(TAB3SMPL == 1, BOG95SMP)

### all regression
df_models <- models %>%
    mutate(model = map(.x = formula,  .f = lm, data = regression_data)) %>%
    mutate(lm_result = map(.x = model, .f = tidy))

### model output's reorganization
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

