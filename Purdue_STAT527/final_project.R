# library
library("MASS")
library("lmtest")
library("dplyr")
library("data.table")
library("openxlsx")

# the dataset
print(getwd())
df1 <- read.csv("Purdue_STAT527/unicorn_data_1.csv")
df2 <- read.csv("Purdue_STAT527/unicorn_data_2_founded_year.csv")

head(df1)
head(df2)

# Merge two datasets
## convert company names into lowercase ones
df1$Company <- tolower(df1$Company)
df2$Company <- tolower(df2$Company)

## merge
df <- merge(df1, df2, by = "Company")

# export the merged dataset
write.xlsx(df, "Purdue_STAT527/unicorn_data.xlsx")
