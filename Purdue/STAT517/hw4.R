library("tidyverse")
library("broom")

# 3.6.5 from textbook
## normal distribution
(1 - pnorm(2, 0, 1))*2

## X has a t-distribution with 1 degree of freedom.
(1 - pt(2.0, 1))*2

## X has a t-distribution with 3 degrees of freedom.
(1 - pt(2.0, 3))*2

## X has a t-distribution with 10 degrees of freedom.
(1 - pt(2.0, 10))*2

## X has a t-distribution with 30 degrees of freedom.
(1 - pt(2.0, 30))*2
