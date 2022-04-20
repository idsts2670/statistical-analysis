# library
library("tidyverse")
library("broom")

# set working directory
setwd("~/Documents/statistical-analysis/effect-verification/intro-to-effect-verification")

# read the data
email_data <- read_csv("data/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")

# create the dataset without the data with e-mail sent to women
male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>%
  # add treatement columns
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# create the data with selection_biased
## set seed
set.seed(1)

## make half in some conditions
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## make biased data
biased_data <- male_df %>%
  mutate(
    # make half in `obs_rate_c` column if filters matched
    obs_rate_c =
      ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1), # nolint
    # make half in `obs_rate_t` column if filters matched
    obs_rate_t =
      ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t), # nolint
    # generate random number in `random_number` column
    random_number =
      runif(n = NROW(male_df))) %>%
  filter((treatment == 0 & random_number < obs_rate_c) |
      (treatment == 1 & random_number < obs_rate_t))

names(biased_data)

# regression with biased_data
## execute the regression analysis
biased_reg <- lm(data = biased_data, formula = spend ~ treatment + history)
# summary report
summary(biased_reg)

## change the lm's output to dataframe
biased_reg_coef <- tidy(biased_reg)