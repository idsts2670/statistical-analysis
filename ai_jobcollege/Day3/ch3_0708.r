#############################################################
################## Day3：分散分析/実験計画法 ################
#############################################################

### 必要な外部データ ###
# - Benefit.csv
# - oathTab_Result.csv
# 作業ディレクトの下のdataフォルダに格納

###################### Prerequiste ###################### 
# データパスの取得
root_dir <- getwd()
root_dir # プロジェクトのディレクトリを指していることを確認
csvPATH <- "./data/Benefit.csv"



#################### Day3-Exercise1 分散分析 ####################
#### 分散分析ANOVA の実施

####### 課題１ ####### 

iris   # フィッシャーの菖蒲データ。サンプルデータとして最も有名。

str(iris)
summary(iris)
# Species変数は3水準、各水準で50の観測値のカテゴリカル変数

#散布行列
plot(iris[,1:4])
plot(iris[,1:4], col=as.numeric(iris$Species))  # Speciesで色分け

#hist
par(mfrow=c(2,2))
  with(iris, hist(Sepal.Length)) 
  with(iris, hist(Sepal.Width ))
  with(iris, hist(Petal.Length))
  with(iris, hist(Petal.Width))
par(mfrow=c(1,1))

#個別にそれぞれの分布を箱ひげ図から確認する
par(mfrow=c(2,2))
  with(iris, boxplot(Sepal.Length~Species, main="Sepal.Length")) 
  with(iris, boxplot(Sepal.Width~Species, main="Sepal.Width"))
  with(iris, boxplot(Petal.Length~Species, main="Petal.Length"))
  with(iris, boxplot(Petal.Width~Species, main="Petal.Width"))
par(mfrow=c(1,1))
# 中央値: 太い横線が水準別にずれている => それぞれ違いそう
# ばらつき: 箱の幅とひげをあわせた幅がPetal.LengthやPetal.Widthでばらつきがみられる

# 平均値と分散の、Speciesの水準別に計算 
# Petal.Length を代表にやってみる
with(iris, by(Sepal.Width, INDICES=Species, FUN=mean))   # 平均
with(iris, by(Sepal.Width, INDICES=Species, FUN=var))    # 分散
# byはファクター別にあるfunctionを使う（SQLのgroup byみたいなの）


### Rで分散分析
## Rでのモデル式の書き方は多くの場合, y ~ x 
## 実施方法は複数ある

# (1) lm関数（回帰分析でも用いられる）
anova(lm(Sepal.Width ~ Species, data=iris)) # H0(SpeciesによってSepal.Widthの平均に差はない)を棄却

# (2) aov関数
summary(aov(Sepal.Width ~ Species, data=iris))

# (3) oneway.test関数
oneway.test(Sepal.Width ~ Species, data=iris, var.equal=TRUE)


## 3変数のペア比較 - 各水準の組み合わせで検定を行いたい場合

# Sepal.Width
with(iris, pairwise.t.test(Sepal.Width, Species, p.adjust.method="none"))
# Petal.Length
with(iris, pairwise.t.test(Petal.Length, Species, p.adjust.method="none"))



####### 課題２ ####### 
benefit <- read.csv(file=csvPATH, header=T)

str(benefit)
summary(benefit)

hist(benefit$Satisfaction) #満足度のヒストグラム
#plot(benefit$Benefit, benefit$Age) # benefit vs age

par(mfrow=c(1,2))
  with(benefit, boxplot(Satisfaction ~ Benefit ))
  with(benefit, boxplot(Satisfaction ~ Age ))
par(mfrow=c(1,1))
# 満足度のばらつきを観測

# model 1 (Benefitのみ)
model1 <- lm(Satisfaction ~ Benefit, data=benefit)
anova(model1)
# Benefitは有意

# model 2  (BenefitとAge)
model2 <- lm(Satisfaction ~ Benefit + Age, data=benefit)
anova(model2)
# Ageは有意でない

# model 3 (BenefitとAgeと交互作用)
model3 <- lm(Satisfaction ~ Benefit + Age + Benefit * Age, data=benefit)
anova(model3)
# Benefit*Ageも有意

## 交互作用プロット
interaction.plot(benefit$Benefit, benefit$Age, benefit$Satisfaction)
interaction.plot(benefit$Age, benefit$Benefit, benefit$Satisfaction)


## モデルの比較
## 注: モデルの比較に関してはDay4で詳細を解説

# Multiple R-squared(決定係数)：1に近いほど良い
summary(model1)$r.squared  # model1
summary(model2)$r.squared  # model2
summary(model3)$r.squared  # model3

# AIC：小さいほど良い
AIC(model1)  # model1
AIC(model2)  # model2
AIC(model3)  # model3



################## Day3-Exercise2 検定の多重性 ##################
#### 検定の多重性に関するシミュレーション

####  シミュレーション1  ####

n_group <- 15
n_obs <- 20

#set.seed(123)    # seedを変え、シミュレーションを繰り返してみる
value <- rnorm(n_group*n_obs)
group <- as.factor(rep(1:n_group, each=n_obs))

df_sim1 <- data.frame(group, value)

str(df_sim1)
summary(df_sim1)

with(df_sim1, boxplot(value ~ group ))  # 箱ひげ図
with(df_sim1, by(value, INDICES=group, FUN=mean))   # 平均
with(df_sim1, by(value, INDICES=group, FUN=var))    # 分散

## ペア比較
res <- with(df_sim1, pairwise.t.test(value, group, p.adjust.method="none"))
res

## ペア比較の数
choose(n_group, 2)   # n_groupから2つ選ぶ場合の組み合わせの数

## p値が0.05以下となった検定結果の数
sum(res$p.value<0.05, na.rm=TRUE)

## 多重比較法（ボンフェローニ法）によるp値の調整
res_adj <- with(df_sim1, pairwise.t.test(value, group), p.adjust.method="bonferroni")
res_adj
sum(res_adj$p.value<0.05, na.rm=TRUE)


####  シミュレーション2  ####

###正規分布で実験を行う
## setting. alpha =0.05
alpha <- 0.05
test <- function(a, n){ return(1 - (1-a)^n) }  #間違いを犯す確率. a=alpha, n:検定回数 

### 検定回数: n=1
n <- 1

p_vals <- vector(mode="numeric",1e4) # numericなベクトルで初期化. 1e4 = 10の4乗のこと (指数表記)
length(p_vals)   # 長さ10,000のベクトル

for(i in 1:1e4) # 10,000回シミュレーションとして, 
{
  set.seed(i) #1:1e4までのseedingを使って, 再現性をキープし(固定値にしないように注意)、
  x <- rnorm(100) # 標準正規分布から100個サンプリングを行い,
  p_vals[i] <- t.test(x=x, mu=0)$p.value # one-sample t-test(前行でサンプリングした乱数の平均が0かを検定)を行い、p値をとっておく
}

mean(p_vals < 0.05) # <実験結果> 10,000回シミュレーションのp値の平均値 0.0498
test(alpha,1) # <理論値> 危険率(α)を0.05と設定しているので、0.05

### 検定回数: n=5
n <- 5

p_vals5 <- matrix(NA, nrow=1e4, ncol=n)  # matrixを初期化
nrow(p_vals5)   # 行数 10,000
ncol(p_vals5)   # 列数 5

for(j in 1:1e4)
{
  set.seed(j)
  X <- replicate(n=n, expr=rnorm(100)) # 標準正規分布から100*5個サンプリングを行い
  p_vals5[j, ] <- apply(X=X, MARGIN=2, FUN=function(x){
    t.test(x=x, mu=0)$p.value  # サンプリングした値、5つの列、に対しt-test。5つの結果が返る
  })  
}
# sapplyはベクトルやリストの各要素に同じfunctionを使う。
# ここでは、実験結果, p値が 0.05未満のものを5検定すべてとりだし、平均を取っている。

mean(sapply(X=1:nrow(p_vals5), FUN=function(i){ any(p_vals5[i, ] < 0.05) })) # 実験値: 0.2282
# <実験結果> 10,000回シミュレーション、各5回の検定、1回でも有意となった割合 0.2282
test(alpha,n) # 0.226

# => 検定を重ねるごとに、検定の質が落ちている。そこで,


## 多重比較法（ボンフェローニ法） 検定回数で割る
mean(sapply(X=1:nrow(p_vals5), FUN=function(i){ any(p_vals5[i, ] < 0.05 / n) }))
# 0.0504に抑えられている



############# Day3-Exercise3 検出力、サンプルサイズ #############
#### サンプルサイズを計算してみる

##(i). 危険率 alpha=0.1、検出力: 1-Beta=0.8、効果量=0.1（20%に対して10%の改善）
improve <- 0.2*0.1
prop1 <- power.prop.test(n=NULL, p1=0.2, p2=0.2-improve, sig.level=0.10, power=0.8)
prop1
ceiling(prop1$n)  # 各グループの人数

##(ii). 危険率 alpha=0.1、検出力: 1-Beta=0.8、効果量=0.15（20%に対して15%の改善）
  # 各グループの人数

##(iii). 危険率 alpha=0.1、検出力: 1-Beta=0.8、効果量=0.2（20%に対して20%の改善）
improve <- 0.2*0.2
prop3 <- power.prop.test(n=NULL, p1=0.2, p2=0.2-improve, sig.level=0.05, power=0.8)
prop3
ceiling(prop3$n)  # 各グループの人数

# => 効果量が大きく期待できると、サンプル数が少なくて済む

##(iiii). 危険率 alpha=0.05、検出力: 1-Beta=0.95、効果量=0.1（20%に対して10%の改善）
improve <- 0.2*0.1
prop4 <- power.prop.test(n=NULL, p1=0.2, p2=0.2-improve, sig.level=0.05, power=0.95)
prop4
ceiling(prop4$n)  # 各グループの人数 

# => 危険率、検出力を厳しくすると、沢山のサンプル数が必要



################### Day3-Exercise4 直行表実験 ###################
#install.packages("DoE.base")
library(DoE.base) # Design of Experiment Package


### 直行表の作成(実験の計画段階)

## 各要因とその水準の定義
tutor <- c("Y", "M")
msg <- c("人工知能", "AI")
description <- c("仕組みを学べる", "最高の講師から学べる")
colour <- c("White", "Blue")
# 4要因、各2水準なのですべての組み合わせは2^4 = 16

## 完全実施要因計画(Full Factorial)  16回
full <- oa.design(nlevels=c(2,2,2,2), nruns=16, randomize=FALSE, seed=12345)
full
## 一部実施要因計画(Fractional Factorial)  8回
frac <- oa.design(nlevels=c(2,2,2,2), nruns=, randomize=FALSE, seed=12345)
frac

## 水準名を利用
oathTab <- oa.design(
  nlevels=c(2,2,2,2),
  factor.names=list(tutor=tutor, msg=msg, description=description, colour=colour),
  #columns="min34",
  nruns=8,
  randomize=FALSE,
  seed=12345
)
oathTab 

#これを保存.　
write.csv(oathTab, file="./data/oathTab_Design.csv", row.names=FALSE)
# => これを実際は実装する。


### 実験後
##  結果が, oathTab_Result.csvのregistrationに保存されているとする。

oath_result <- read.csv('./data/oathTab_Result.csv')
str(oath_result)
summary(oath_result)

## 要因別集計 - 要因別に水準間のregistration平均値を求める
for(i in 1:4){
  print(
  aggregate(oath_result$registration, by=list(oath_result[,i]), FUN="mean")
  )
}


## モデルの当てはめ
#exp_result <- aov(registration~., data=oath_result)
exp_result <- lm(registration~., data=oath_result)
# 分散分析
anova(exp_result)
# 要因の係数, モデルの当てはまりの確認
summary(exp_result)

# Multiple R-squared: 0.9536,	Adjusted R-squared: 0.8916 から確認できる通り、モデルの当てはまり度合いは高い

# tutor要因が有意水準0.01で有意
# description要因が有意水準0.1で有意


## モデルを当てまめることにより、最適な組み合わせの把握、予測が行える

# ベストな組み合わせ
# summary(exp_result) より判断
best.levels <- c("Y","AI","最高の講師から学べる","Blue")
# その組み合わせのdata.frameの作成
bast_data <- head(oath_result,0)[,c(1,2,3,4)]
bast_data[1,] <- best.levels
bast_data

# 予測（予測値の信頼区間も含める）
predict(exp_result, newdata=bast_data, interval="prediction", level=0.95)

# 実験データへの予測
predict(exp_result)

## best.levelsは実際に実験していないが、最大の予測結果となる

