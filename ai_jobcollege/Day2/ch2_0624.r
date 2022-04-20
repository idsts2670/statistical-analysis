#############################################################
############### Day2：確率/統計的推定/仮説検定 ###############
#############################################################

### 必要な外部データ ###
# - Basket_en2.csv
# これらのデータを作業ディレクトの下にdataフォルダを作成し、格納しておく


###################### Prerequiste ###################### 
##### install packages if necessary



##### Definitions 
root_dir <- getwd()
csvPATH1 <- "./data/Basket_en2.csv"


################# Day2-Exercise1 バスケット分析 #################
#### バスケット分析を例に、確率と条件付き確率を確認

trans_dat <- read.transactions(csvPATH1, sep = ",")  # read.csv/read.tableでないことに注意
class(trans_dat)  # arulesパッケージのtransactionsと呼ばれるデータ形式

## データ内容確認
as(trans_dat,"matrix")
as(trans_dat,"data.frame")
summary(trans_dat)

itemFrequencyPlot(trans_dat, type="absolute")

# Affinity between the two items i and j
# A(i,j) = sup({i,j})/(sup({i}) + sup({j}) - sup({i,j}))
#affinity(trans_dat)

# 相関ルールの抽出
res <- apriori(trans_dat, parameter=list(minlen=2, maxlen=3, support=0.2, confidence=0))

inspect(res)



################# Day2-Exercise2 確率分布の生成 #################
#### 乱数を確率分布から生成し確認

### Rでの確率分布関連のfunctionはほとんど
# d$dist p$dist q$dist r$dist  
# $dist = norm, binom, unif...etc

# d: density => 確率密度（正規分布のグラフの高さ）
# p: probability => 下側確率の値に対する,分位点 . ( = 確率変数 Xに対し P(X < 値) = ? を返す)
# q: quantile => 分位点に対する、値(e.g. Z値) . pとq は表裏一体. Quantile( P(X < 値)) = 値 
# r: random => 乱数生成

#### 正規分布 (norm: normal distribution)
####95%をカバーする範囲、分位点

## dnorm
#平均=0, 標準偏差=1の正規分布に関して、xのときの確率密度
dnorm(0, mean=0, sd=1)  #x=0

# xを変化させて正規分布のグラフを描いてみる
x <- seq(-4, 4, by=0.05)  # -4~4までの順列, 0.05刻み

plot(x, #x軸
     dnorm(x, mean=0, sd=1), #y軸
     #type="l", #点じゃなくて線
     main = "Standard Normal Disrtibution",
     xlab= "x", #x軸のラベル
     ylab="f(x)"
)

### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 標準正規分布の分位点とカバーする範囲の理解
# 95%をカバー => 有意度 alpha=0.05
# alpha/2 = 0.025  => 1 - alpha/2 = 0.975
plot(x, dnorm(x, mean=0, sd=1), type="l", xlab= "x", ylab="f(x)",
     main="Standard Normal Disrtibution, alpha=0.05")

xval <- seq(-4,-1.96, by= 0.04 )
xvals <- c(xval, rev(xval))
xvals <- c(xvals, -1 * xvals)
yvals <- c( rep(0,length(xval)), dnorm(rev(xval), mean=0,sd=1))
yvals <- c(yvals, rev(yvals))
polygon( xvals, yvals, col="gray" )

abline(v=1.96, col="red",lwd=2, lty=2)
abline(v=-1.96, col="red",lwd=2, lty=2)
text(-1.96,0.3,"-1.96",col="red",pos=2)
text(1.96,0.3,"1.96",col="red",pos=4)
text(-1.96,0.05,"2.25%",col="blue",pos=2)
text(1.96,0.05,"1-0.975",col="blue",pos=4)
# 白の部分が、有意水準alpha=0.05に対する信頼区間(confidence interval)
# 色付きの部分が棄却域(rejection region).
### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 分位点(Quantile) ってどういうこと?
plot(x, dnorm(x, mean=0, sd=1), type="l", xlab= "x", ylab="f(x)",
     main = "Standard Normal Disrtibution, Quantile 0.975")
xval <- seq(-4, 1.96, by= 0.04 )
xvals <- c(xval, rev(xval))
yvals <- c( rep(0,length(xval)), dnorm(rev(xval), mean=0,sd=1))
polygon( xvals, yvals, col="gray" )

abline(v=1.96, col="red",lwd=2, lty=2)
text(1.96,0.3,"1.96",col="red",pos=4)
text(0,0.2,"97.5%",col="blue")
# 標準正規分布において、分位点1.96以下をカバーする確率は、97.5%
### ### ### ### ### ### ### ### ### ### ### ### ### ### 


## 分位点=1.96以下をカバーする確率は？
## p: probability
pnorm(1.96)  # 0.975 

## 97.5%をカバーする分位点は？
## q: quantile
qnorm(0.975)  # 1.959964


### 二項分布 (binom: binomial  distribution)
#同様に, dbinom, pbinom, qbinom, rbinomでできる

### 一様分布 (unif: uniform  distribution)
#同様に, dunif, punif, qunif, runif でできる

### どの分布でも一緒!


### σ×1,σ×2,σ×3がカバーする確率

# 1 sigma
sigma <- 1 
1 - 2*pnorm(-1 * sigma)   # 約70%

# 2 sigma
1 - 2*pnorm(-2 * sigma)   # 約95%

# 3 sigma
1 - 2*pnorm(-3 * sigma)   # 99%以上


#### TOEICの例 ####
#TOEICのスコアが、平均580、標準偏差170の正規分布に従っていると仮定する.
#ランダムに抽出したあるTOEIC受験者のスコアが800点以上である確率は？
mu <- 580
sigma <- 170
pnorm(800, mean=580, sd=170)   # 800点以下
1 - pnorm(800, mean=mu, sd=sigma)  # 800点以上


#### 二項分布の例  ####
#サイコロを3回振る、1回だけ6の目が出る確率は？
dbinom(1, 3, 1/6)   # 5/6 * 5/6 * 1/6 * 3と同じ

# dbinom(x, size, prob)に関して
# size:試行回数
# x:成功回数
# prob:成功確率

#1回のコンタクトで契約が取れる確率が4%、100件の客先に1回ずつコンタクトした場合、
#5件以上契約が取れる確率は？10件以上契約が取れる確率は？

dbinom(1, 100, 0.04)  # 1件だけ契約が取れる確率

n <- 100
p <- 0.04
1 - pbinom(5,n,p) # 5件以上契約が取れる確率は？

1 - pbinom(10,n,p) # 10件以上契約が取れる確率は？

mean(rbinom(10000, n, p))# 期待値
n * p # 理論上の期待値

# 以下は同じ結果となる
dbinom(0,n,p) + dbinom(1,n,p) + dbinom(2,n,p) + dbinom(3,n,p) +  dbinom(4,n,p) + dbinom(5,n,p)
pbinom(5,n,p) 



############## Day2-Exercise3 母平均、母比率の推定 #############
#### 標本から、母平均、母比率の推定を行ってみる
#### 母集団はシミュレーションにより生成、標本抽出をランダムに行う（母集団と標本の違いを確認）
#### 区間推定を理解する
#### ヒストグラムによる、誤差バー表現

## 母集団の作成
set.seed(12345)
population <- rnorm(n=20000, mean=3000000, sd=600000)

## 母集団の平均と標準偏差
length(population) # N: 20000
mean(population)   # 平均: 2999101
sd(population)     # 標準偏差: 595977.8
# 乱数を利用したので300万と60万から少しずれるが、これらを全数調査しないと分からない真の値とする

## 10標本ランダム抽出
N <- 10
set.seed(12345)
sample10 <- sample(population, size=N, replace=FALSE)
mean(sample10)   # 平均: 2926423
sd(sample10)     # 標準偏差: 360670.7

s.e <- sd(sample10)/sqrt(length(sample10))  # 平均の標準誤差: 114054.1
s.e

q.t <- qt(c(0.05/2, 1-0.05/2), df=N-1)  # 自由度n-1のt分布のα=0.95に対応する分位点: -2.262157, 2.262157
q.t

mean(sample10) + s.e * q.t  # 信頼区間: 2668414 3184431

# よって、10標本の場合、平均値が2926423で、その信頼区間が[2668414, 3184431]

## 関数で実行する場合
t.test(sample10)


## 100標本ランダム抽出
N <- 100
set.seed(12345)
sample100 <- sample(population, size=N, replace=FALSE)
mean(sample100)   # 平均: 2989098
sd(sample100)     # 標準偏差: 590920.9

t.test(sample100)  # 2871846 3106349


## 1000標本ランダム抽出


### 標本数が大きくなるに従い、信頼区間が狭まる（信頼性が増す）ことを確認



###################### Day2-Exercise4 検定 ######################

#### 母平均、母比率の差の検定を行ってみる

### t.test()
### 新しいマーケ施策AとBの効果検証のため、20人の顧客に施策を実施し（A,B各10名ずつ）、
### 売り上げ(円)を比較した
A.sales <- c(1000, 980, 1200, 1260, 1500, 1005, 820, 1490, 1500, 960)
B.sales <- c(880, 1080, 1580, 2180, 1900, 1950, 1200, 910, 2100, 1890)
t.test(A.sales, B.sales)

### prop.test()
### 新しいマーケ施策AとBの効果検証のため、100人の顧客に施策を実施し（A,B各50名ずつ）、
### 売り上げを比較した
A.B.res <- c(5, 13)    # Aにおける購入人数:5、Bにおける購入人数:9
A.B.pop <- c(50, 50)   # A,Bの人数
prop.test(A.B.res, A.B.pop)


#### いくつかの母集団をシミュレーションで生成、標本サイズを変えながらランダムに標本を抽出
#### （検定により、母集団の違いを判断できるかを確認）
###シミュレーションにより、検定の性質を理解する

## Simulation A -> A,Bは異なったグループ

## サンプル数各グループで20
N <- 20
# 母集団Aからサンプリング
set.seed(0)
A20 <- rnorm(N,30,10)
# 母集団Bからサンプリング
set.seed(0)
B20 <- rnorm(N,35,10)
# 比較
t.test(A20, B20)

## サンプル数各グループで40
N <- 40
# 母集団Aからサンプリング
set.seed(0)
A40 <- rnorm(N,30,10)
# 母集団Bからサンプリング
set.seed(0)
B40 <- rnorm(N,35,10)
# 比較
t.test(A40, B40)

## サンプル数を少なくしたり多くしたりシミュレーションしてみる

## サンプル数各グループで？？？（いろいろ試す）
N <- 100000
# 母集団Aからサンプリング
set.seed(0)
A <- rnorm(N, 30, 10)
# 母集団Bからサンプリング（Aとほぼ平均値の差をなくしている）
set.seed(0)
B <- rnorm(N, 30.1, 10)
# 比較
t.test(A, B)


## 各グループのサンプル数（N_group）を変化させてシミュレーション実験
N_group <- 20

N_sim <- 1000  # シミュレーション回数。N_sim回の検定を実行
p_val <- c()
for(i in 1:N_sim){
  A <- rnorm(N_group, 30, 10)
  B <- rnorm(N_group, 35, 10)
  ttest <- t.test(A, B)
  p_val <- append(p_val, ttest$p.val)
}

mean(p_val)   # p値の平均
sum(p_val<=0.05) / N_sim   # N_sim中、p値が0.05を下回った割合


### サンプル数と棄却できる割合の関係を理解する



## Simulaton B -> A,Bは同じグループ


## 各グループのサンプル数（N_group）
N_group <- 20

N_sim <- 1000  # シミュレーション回数。N_sim回の検定を実行
p_val <- c()
for(i in 1:N_sim){
  A <- rnorm(N_group, 30, 10)
  B <- rnorm(N_group, 30, 10)
  ttest <- t.test(A, B)
  p_val <- append(p_val, ttest$p.val)
}

mean(p_val)   # p値の平均
sum(p_val<=0.05) / N_sim   # N_sim中、p値が0.05を下回った割合

### 実際には差がないが棄却してしまう。誤って棄却してしまう割合は有意水準に等しい


## 検出力、サンプルサイズに関してはDay3で取り上げる

power.t.test(n=10, delta=5,sd=10, sig.level=0.05, 
              power=NULL, strict=T)

power.t.test(n=NULL, delta=5,sd=10, sig.level=0.05, 
             power=0.8, strict=T)

power.t.test(n=NULL, delta=5,sd=10, sig.level=0.05, 
             power=0.95, strict=T)






#################  Day2 おまけ #################

#***************************************# 
###########  set.seedに関して  ########## 
#***************************************# 

### 確率分布からのサンプリング
##rnorm 

z0 <- rnorm(10000, mean=0, 1)  # 標準正規分布から10000個のポイントをサンプリング
head(z0)

z1 <- rnorm(10000, mean=0, 1)  # 標準正規分布から10000個のポイントをサンプリング
mean(z0)
mean(z1)
mean(z0) == mean(z1)

# 何もせずにサンプリングをすると毎回結果が変わってしまう。
# seedをセットすることで、seedが同じ限りはいつも同じ結果が得られるようになる。

set.seed(12345) 
z10000 <- rnorm(10000, mean=0, 1)
set.seed(12345) 
zz <- rnorm(10000, mean=0, 1)
head(z10000)
head(zz)
mean(z10000) == mean(zz)

#10000サンプル vs 本当の標準正規分布
hist(z10000,col="gray", freq=F, main="10000 Samples from Standard Normal Distribution") # 縦軸:絶対数ではなく、密度   
curve(dnorm(x, mean=0, sd=1), add=T, col="red", lwd=2) 
