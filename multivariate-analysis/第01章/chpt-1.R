par(family = "HiraKakuProN-W3")
kjs <- c('協調性', '自己主張', 'ストレス')
plot(jhk[, kjs])

effectv <- function(x, y, clevel=0.95)
{
  library(vcd)
  library(MBESS)
  tmpcross <- table(x, y) #クロス集計法の算出
  n <- sum(tmpcross) #標本サイズの算出
  size <- dim(tmpcross) #集計表の行数と列数を算出
  dof <- prod(size-1) #自由度を算出
  resas <- assocstats(tmpcross)
  chi <- resas$chisq_tests['Pearson', 'X^2']
  v <- resas$cramer
  resconf <- conf.limits.nc.chisq(Chi.Square = chi,
                                  df=dof, conf.level = clevel)
  if(resconf$Lower.Limit>0)
  {
    ll <- sqrt((dof+resconf$Lower.Limit)/((min(size)-1)*n))
    ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
    return(list=c(効果量V=v, カイ2乗値=chi, 信頼水準=clevel,
                  区間下限=ll, 区間上限=ul))
  } 
  else if(resconf$Lower.Limit==0)
  {
    resconf <- conf.limits.nc.chisq(Chi, Square=chi,
                                   df=dof, conf.level = NULL, alpha.lower=0, 
                                   alpha.upper = (1-clevel)/2)
    ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
    return(list=list(
      c(効果量V=v, カイ2乗値=chi, 信頼水準=clevel, 区間下限=0, 区間上限=ul)))
  }
}

effectd1 <- function(x1, x2, clevel = 0.95)
{
  library(MBESS)
  #各郡の標本サイズの算出
  n1 <- length(x1); n2 <- length(x2)
  #各郡の平均の算出
  m1 <- mean(x1); m2 <- mean(x2)
  #各郡の標本標準偏差の算出
  s1 <- sqrt(mean((x1-m1)^2))
  s2 <- sqrt(mean((x2-m2)^2))
  #母標準偏差の推定値の算出
  sast <- sqrt(((n1*s1^2)+(n2*s2^2))/(n1+n2-2))
  #効果量の算出
  d <- (m1-m2)/sast
  #独立な2郡のt検定の実行(等分散仮定)と自由度の算出
  rest <- t.test(x1, x2, paired = FALSE, var.equal = TRUE)
  #効果量の信頼区間の算出
  resconf <- conf.limits.nct(t.value=rest$statistic, 
                             df=rest$parameter, conf.level=clevel)
  ll <- resconf$Lower.Limit*sqrt((n1+n2)/(n1*n2))
  ul <- resconf$Upper.Limit*sqrt((n1+n2)/(n1*n2))
  u3 <- pnorm(d, 0, 1)
  return(list=c(効果量=d, 信頼水準=clevel, 区間下限=ll, 区間上限=ul, U3=u3))
}

Sys.setenv(LANG = "ja_JP.UTF-8")
Sys.setlocale(locale="Japanese")

mat <- read.csv('学力調査結果.csv')
head(mat, 5)
dim(mat)
colnames(mat)
library(lattice)
library(gplots)
par(family = "HiraKakuProN-W3")
histogram(~プレ得点|部活, data=mat, breaks = 20)
boxplot(プレ得点~部活, data=mat, horizontal = TRUE)
summary(mat$プレ得点)
tapply(mat$数学, mat$性別, mean)
var.test(数学~性別, data = mat) #二郡の母分散は等しいという帰無仮説を棄却できないから、等分散を仮定してt検定を行う
t.test(数学~性別, data = mat, var.equal=TRUE) #母平均に差が無いという帰無仮説を棄却できない
Fmath <- mat$数学[mat$性別=='F']
Mmath <- mat$数学[mat$性別=='M']
effectd1(Mmath, Fmath, clevel = 0.95)
goukei <- apply(mat[, c('プレ得点', 'ポスト得点')], 1, sum)
head(goukei, 3)
spre1 <- scale(mat$プレ得点)
head(spre1, 3)
plot(mat$プレ得点, spre1, xlab='プレ得点', ylab='spre1')
cor(mat$プレ得点, spre1)
library(psych)
fivename <- c('国語', '社会', '数学', 'プレ得点', 'ポスト得点') 
mat3 <- mat[, fivename]
partial.r(mat3, c(1, 2, 3), c(4, 5))
#2値カテゴリカル変数化するための階級幅を作成する
kogoc <- c(-Inf, mean(mat$国語), Inf)
kcat <- cut(mat$国語, breaks = sogoc, right=FALSE, labels=c(0,1))
sogoc <- c(-Inf, mean(mat$社会), Inf)
scat <- cut(mat$社会, breaks = sogoc, right=FALSE, labels=c(0,1))
eogoc <- c(-Inf, mean(mat$英語), Inf)
ecat <-  cut(mat$英語, breaks = sogoc, right=FALSE, labels=c(0,1))
library(polycor)
mat2 <- data.frame(国語=kcat, 社会=scat, 英語=ecat)
hetcor(mat2, ML=TRUE)
effectv(mat$部活, mat$性別, clevel = 0.95)
library(grid)

