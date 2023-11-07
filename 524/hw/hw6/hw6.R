# library
library("tidyverse")
library("ggplot2")
library("MASS")
library("car")
library("fs")
library("moments")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 8.12
# data setup
col_names <- c("Total_population", "Professional_degree", "Employed_age_over_16", "Government_employement", "Median_home_value")
df <-  read.table(file.path(main_path, "hw3/T4-6.txt"), header=FALSE, col.names=col_names)