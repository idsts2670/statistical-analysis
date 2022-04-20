get.dd <- function(d)
  {
  n.sample <- nrow(d)
  y.mean <- mean(d$y)
  d$y.rnd <- rpois(n.sample, lambda = y.mean)
  fit1 <- glm(y.rnd~1, data = d, family = poisson)
  fit2 <- glm(y.rnd~x, data = d, family = poisson)
  fit1$deviance - fit2$deviance
  }
pb <- function(d, n.bootstrap)
  {replicate(n.bootstrap, get.dd(d))}
