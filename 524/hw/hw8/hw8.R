library("tidyverse")
library("ggplot2")
library("MASS")

# path setup
current_note_path <- getwd()
main_path <- file.path(current_note_path, "524/hw")

# 11.24
# (a)
df <- read.table(file.path(main_path, "hw8/T11-4.txt"), header = F) %>% data.frame()
is.data.frame(df)
colnames(df) <- c("x1", "x2", "x3", "x4", "Population")

df$Population <- as.factor(df$Population)

# create scatter plot using ggplot() function
plot1 <- ggplot(df, aes(x = x1, y = x2, color = Population)) +
  geom_point()
plot1

plot2 <- ggplot(df, aes(x = x1, y = x3, color = Population)) +
  geom_point()
plot2


plot3 <- ggplot(df, aes(x = x1, y = x4, color = Population)) +
  geom_point()
plot3

# (b)
df_list <- split(df, f = df$Population)
mu1 <- as.matrix(colMeans(df_list$`0`[c("x1", "x2")]))
mu2 <- as.matrix(colMeans(df_list$`1`[c("x1", "x2")]))
s1 <- as.matrix(cov(df_list$`0`[c("x1", "x2")]))
s2 <- as.matrix(cov(df_list$`1`[c("x1", "x2")]))

# (c)
-0.5 * (solve(s1) - solve(s2))

t(mu1) %*% solve(s1) - t(mu2) %*% solve(s2)

k = 0.5 * log(det(s1) / det(s2)) + 0.5 * (t(mu1) %*% solve(s1) %*% mu1 - t(mu2) %*% solve(s2) %*% mu2)


  # prediction <- apply(newdata, 1, function(y) 
  #   d2.y1 <- (y - g1.means) %*% solve(g1.covar) %*% (y - g1.means)
  #   d2.y2 <- (y - g2.means) %*% solve(g2.covar) %*% (y - g2.means)
  #   ifelse(d2.y1^2 > d2.y2^2, 1, 0)
  # })


two.group.quadratic.classification <- function(data, grouping, newdata) {
  dat.split <- split(data, grouping)
  g1 <- as.data.frame(dat.split[1])
  g2 <- as.data.frame(dat.split[2])
  g1.means <- apply(g1, 2, mean)
  g2.means <- apply(g2, 2, mean)
  g1.covar <- cov(g1)
  g2.covar <- cov(g2)
  k <- 0.5 * log(det(g1.covar) / det(g2.covar)) + 0.5 * (t(g1.means) %*% solve(g1.covar) %*% g1.means - t(g2.means) %*% solve(g2.covar) %*% g2.means)
  prediction <- apply(newdata, 1, function(y) {
    quad <- -0.5 * t(y) %*% (solve(g1.covar) - solve(g2.covar)) %*% y + (t(g1.means) %*% solve(g1.covar) - t(g2.means) %*% solve(g2.covar)) %*% y - k
    ifelse(quad >= 0, 0, 1)
    })
  class.table <- table(grouping, prediction, dnn = c("Actual Group","Predicted Group"))
  pred.errors <- sum(diag(t(apply(class.table, 2, rev)))) / dim(data)[1]
  results <- list("Prediction" = prediction, "Table of Predictions" = class.table, "Error Rate" = pred.errors)
  return(results)
}

df.quad <- two.group.quadratic.classification(df[,1:2], df[,5], df[,1:2])
df.quad

df2.quad <- two.group.quadratic.classification(df[2:46,1:2], df[,5], df[,1:2])
df2.quad

#f
n1 = 21
n2 = 25
sp = ((n1-1) * s1 + (n2-1) * s2) / (n1 + n2 - 2)
sp

t(mu1 - mu2) %*% solve(sp)

two.group.fisher.classification <- function(data, grouping, newdata) {
  dat.split <- split(data, grouping)
  g1 <- as.data.frame(dat.split[1])
  g2 <- as.data.frame(dat.split[2])
  g1.means <- apply(g1, 2, mean)
  g2.means <- apply(g2, 2, mean)
  g1.covar <- cov(g1)
  g2.covar <- cov(g2)
  n1 = nrow(g1)
  n2 = nrow(g2)
  sp = ((n1 - 1) * g1.covar + (n2-1) * g2.covar) / (n1+n2-2)

  m = 0.5 * t(g1.means-g2.means) %*% solve(sp) %*% (g1.means+g2.means)
  print(m)
}

# 11.29
df2 <- read.table(file.path(main_path, "hw8/T11-6.txt"), header = F) %>% data.frame()
colnames(df2)<- c("GPA","GMAT","group")
df2$group = as.factor(df2$group)

dat.split <- split(df2[,1:2], f = df2$group)
g1 <- as.data.frame(dat.split[1])
g2 <- as.data.frame(dat.split[2])
g3 <- as.data.frame(dat.split[3])
x.means = apply(data2[,1:2], 2, mean)
g1.means <- apply(g1, 2, mean)
g2.means <- apply(g2, 2, mean)
g3.means <- apply(g3, 2, mean)
g1.covar <- cov(g1)
g2.covar <- cov(g2)
g3.covar <- cov(g3)
n1 = nrow(g1)
n2 = nrow(g2)
n3 = nrow(g3)
sp = ((n1-1)*g1.covar + (n2-1)* g2.covar + (n3-1)* g3.covar) / (n1+n2+n3-3)
sp
W = (n1+n2+n3-3)*sp
solve(W)
B = (g1.means-x.means)%*%t(g1.means-x.means) + (g2.means-x.means)%*%t(g2.means-x.means) + (g3.means-x.means)%*%t(g3.means-x.means)
solve(W)%*%B
eigen = eigen(solve(W)%*%B)
a1 = eigen[["vectors"]][,1]
a2 = eigen[["vectors"]][,2]
x0 = as.matrix(c(3.21,497))
y1 = a1%*%x0
y2 = a2%*%x0
y11 = a1%*%g1.means
y12 = a2%*%g1.means
y21 = a1%*%g2.means
y22 = a2%*%g2.means
y31 = a1%*%g3.means
y32 = a2%*%g3.means

(y1-y11)^2 + (y2-y12)^2
(y1-y21)^2 + (y2-y22)^2
(y1-y31)^2 + (y2-y32)^2
