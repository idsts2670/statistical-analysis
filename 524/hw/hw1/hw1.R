# library
library("tidyverse")
library("dplyr")
library("car")
library("ggplot2")
library('reshape2')
library('caTools')
library('Rcmdr')
library('MASS')
library('ggExtra')

# problem 1-2
## dataframe
df <- data.frame(
                x1 = c(1, 2, 3, 3, 4, 5, 6, 8, 9, 11),
                x2 = c(18.95, 19.00, 17.95, 15.54, 14.00, 12.95, 8.94, 7.49,
                    6.00, 3.99))
ggplot(df, aes(x1)) + geom_dotplot(method="histodot")
ggplot(df, aes(x2)) + geom_dotplot(method="histodot")

p1 <- ggplot(df, aes(x1, x2)) + geom_point() + theme_bw()
p1
ggMarginal(p1, type = "histogram")

# problem 1-6
col_names <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7")
df2 <-  read.table("/Users/satoshiido/Documents/statistical-analysis/524/T1-5.txt", header=FALSE, col.names=col_names)
df2

ncol(df2)

ggplot(df2, aes(x1)) + geom_dotplot(method="histodot")
for(i in 1:ncol(df2)) {
    ggplot(df2, aes(x{i})) + geom_dotplot(method="histodot")
}

plotlist <- list(
    plot1 = ggplot(df2, aes(x1)) +
    geom_dotplot(method="histodot", binwidth=0.1),
    plot2 = ggplot(df2, aes(x2)) +
    geom_dotplot(method="histodot", binwidth=1),
    plot3 = ggplot(df2, aes(x3)) +
    geom_dotplot(method="histodot", binwidth=0.1),
    plot4 = ggplot(df2, aes(x4)) +
    geom_dotplot(method="histodot", binwidth=0.1),
    plot5 = ggplot(df2, aes(x5)) +
    geom_dotplot(method="histodot", binwidth=0.3),
    plot6 = ggplot(df2, aes(x6)) +
    geom_dotplot(method="histodot", binwidth=0.3),
    plot7 = ggplot(df2, aes(x7)) +
    geom_dotplot(method="histodot", binwidth=0.05)
)

# pdf file output
pdf(file="/Users/satoshiido/Documents/statistical-analysis/524/test.pdf", height=8, width=10)
lapply(plotlist, print)
dev.off()

## mean
summarise_all(df2, mean)
## variance & covariance
cov(df2)
cov2cor(cov(df2))


sigma <- matrix(c(2,sqrt(2)/2, sqrt(2)/2,1),2,2)
sigma
eigen(sigma)
(-0.8880738)^2 + (-0.4597008)^2
(-0.4597008)^2 + (0.8880738)^2