setwd("/Users/satoshiido/Documents/statistical-analysis/")
chan <- read.csv("Purdue_STAT526/chan.csv")

# Censoring and Truncation: Example
ind <- sample(97, 20)
delta <- chan$Death[ind]
V <- chan$Entry[ind]
X <- chan$Exit[ind]

# plot
plot(V, 1:20, pch = "|", xlim = c(720, 1200))
points(X, 1:20, pch = c("o", "x")[delta + 1])
for(i in 1:20) lines(c(V[i], X[i]), c(i, i), lty = 2)

# risk set size
t <- seq(720, 1200, len = 100)
at.risk <- outer(chan$En, t, "<=") & outer(chan$Ex, t, ">")
plot(t, apply(at.risk, 2, sum), type = "l")

