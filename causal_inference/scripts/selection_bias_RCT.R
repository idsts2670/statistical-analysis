## ----setup, include = FALSE, cache = FALSE------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  tidy = TRUE
)


## -----------------------------------------------------------------------------
library("ggplot2")
library("tidyverse")
library("MASS")


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
# Create the input_dir (input directory)
current_note_path <- getwd()
INPUT_DIR <- file.path(current_note_path, "causal_inference/data")

# If INPUT_DIR has not been created yet, create it
if (!dir.exists(INPUT_DIR)) {
  dir.create(INPUT_DIR)
}

# Create the output_dir (output directory)
OUTPUT_DIR <- file.path(current_note_path, "causal_inference/output")

# If OUTPUT_DIR has not been created yet, create it
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}

# Read CSV files using a function to specify the directory automatically
read_csv <- function(name, ...) {
  path <- file.path(INPUT_DIR, paste0(name, ".csv"))
  print(paste("Load:", path))
  return(read.csv(path, ...))
}


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
email_data <- read_csv("Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20")
head(email_data)


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
# create the data w/o the womens E-Mail campaign
male_df <- email_data %>% 
  filter(segment != "Womens E-Mail") %>%
  mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))
head(male_df)


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
summary_by_segement <- male_df %>%
  group_by(treatment) %>%
  # mean of conversion by treatment
  summarise(conversion_rate = mean(conversion),
            # mean of spend by treatment
            spend_mean = mean(spend),
            # number of customers by treatment
            total = n())
summary_by_segement


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
# pull the data with Men's E-Mail Campaign
mens_mail <- male_df %>%
  filter(treatment == 1) %>%
  pull(spend)

# pull the data without  Men's E-Mail Campaign
no_mail <- male_df %>%
  filter(treatment == 0) %>%
  pull(spend)

rct_ttest <- t.test(mens_mail, no_mail, var.test = TRUE)
rct_ttest


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
# create the selection biased data
## set the seed
set.seed(1)

## make half depending on the condition
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## create the biased data
biased_data <- male_df %>%
  mutate(
    obs_rate_c = if_else(
      (history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
    obs_rate_t = if_else(
      (history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
    random_number = runif(n = NROW(male_df))) %>%
    filter((treatment == 0 & random_number < obs_rate_c) | (treatment == 1 & random_number < obs_rate_t))
head(biased_data, 20)


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
summary_by_segment_biased <- biased_data %>%
  group_by(treatment) %>%
  summarise(conversion_rate = mean(conversion),
            spend_mean = mean(spend),
            count = n()
            )
summary_by_segment_biased


## ----warning=FALSE, message=FALSE, include=FALSE------------------------------
# purchase data with mail campaign
mens_mail_biased <- biased_data %>%
  filter(treatment == 1) %>%
  pull(spend)

# purchase data without mail campaign
no_mail_biased <- biased_data %>%
  filter(treatment == 0)  %>%
  pull(spend)

# t-test
rct_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.test = TRUE)
rct_ttest_biased

