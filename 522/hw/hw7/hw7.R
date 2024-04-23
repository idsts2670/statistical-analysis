library("tidyverse")
library("car")
library("ggplot2")


df <- read_csv("/Users/satoshiido/Documents/programming/statistical-analysis/522/hw7/HW7data.csv")
df$t_hat_yr
df$t_hat_srs_y

# Histogram of 
ggplot(df, aes(t_hat_srs_y)) +
  geom_histogram(binwidth = 10, fill = 'grey', color = 'black') +
  labs(x = 'SRS t_hat', y = 'Count') +
  ggtitle("Estimate of SRS t_hat") +
  theme_minimal()

ggplot(df, aes(t_hat_yr)) +
  geom_histogram(binwidth = 10, fill = 'grey', color = 'black') +
  labs(x = 'Ratio t_hat_yr', y = 'Count') +
  ggtitle("Estimate of Ratio t_hat_yr") +
  theme_minimal()

