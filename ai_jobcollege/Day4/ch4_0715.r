#############################################################
##################### Day4： 回帰モデル #####################
#############################################################

### 必要な外部データ ###
# - MediaEffect.csv
# - Multicollinearity.csv
# - unicef98.dat
# 作業ディレクトの下のdataフォルダに格納

#################### Day4-Exercise1 回帰モデル ####################

### LifeCycleSavingsデータ
## savings ratio (aggregate personal saving divided by disposable income) 1960–1970
# sr     - aggregate personal savings
# pop15  - % of population under 15
# pop75  - % of population over 75
# dpi    - real per-capita disposable income
# ddpi   - % growth rate of dpi

View(LifeCycleSavings)
str(LifeCycleSavings)
summary(LifeCycleSavings)

plot(LifeCycleSavings)

### 単回帰: pop15
single.m <- lm(sr~pop15 , data=LifeCycleSavings)
summary(single.m)  
# R2 0.2075
# pop15のEstimateはマイナス。p-valsより、有意と判断

## 回帰係数の信頼区間
confint(single.m, level=0.95)

## 散布図と推定した単回帰モデルを重ね合わせる
plot(LifeCycleSavings$pop15, LifeCycleSavings$sr)
abline(single.m, col="blue")

# モデル予測値の出力方法（散布図の点を青い線に縦に落とした個所）
predict(single.m)

# 残差の出力方法（点と青い線の距離）
resid(single.m)

## 予測値と実測値プロット
#plot(LifeCycleSavings$sr, predict(single.m), xlab="Actual", ylab="Predict")
plot(LifeCycleSavings$sr, predict(single.m), xlab="Actual", ylab="Predict", xlim=c(0,23), ylim=c(0,23))
abline(0, 1, col="red")

## 残差プロット
plot(predict(single.m), resid(single.m), xlab="Predict", ylab="Residual")
abline(0, 0, col="red")

## R標準のモデル当てはまり確認プロット
par(mfrow=c(2,2))
plot(single.m)
par(mfrow=c(1,1))

## 予測と回帰の信頼区間、予測区間
pred.conf.single <- predict(single.m, interval="confidence", level=0.95)
pred.conf.single
pred.pred.single <- predict(single.m, interval="prediction", level=0.95)
pred.pred.single


### 予測と回帰の信頼区間、予測区間をプロットしてみる
# data.frameに必要な変数をまとめる
df.ci <- cbind(pop15=LifeCycleSavings$pop15,       # x 
               sr=LifeCycleSavings$sr,             # y
               pred=predict(single.m),             # 予測値
               ci.lower=pred.conf.single[,2],      # 回帰の信頼区間（下）
               ci.upper=pred.conf.single[,3],      # 回帰の信頼区間（上）
               ci.pred.lower=pred.pred.single[,2], # 予測の信頼区間（下）
               ci.pred.upper=pred.pred.single[,3]) # 予測の信頼区間（上）
df.ci <- as.data.frame(df.ci)
# sort by x
df.ci <- df.ci[order(df.ci$pop15),]

# 上下限の設定
x.lim <- c(20,50)
y.lim <- c(0,23)
# データのプロット
plot(df.ci$pop15, df.ci$sr, xlim=x.lim, ylim=y.lim, xlab="x: pop15", ylab="y: sr")
# 予測値(回帰直線)
par(new=TRUE)
plot(df.ci$pop15, df.ci$pred, type="l", col="black", xlim=x.lim, ylim=y.lim, xlab="", ylab="")
# 回帰の信頼区間
par(new=TRUE)
plot(df.ci$pop15, df.ci$ci.lower, type="l", col="blue", xlim=x.lim, ylim=y.lim, xlab="", ylab="")
par(new=TRUE)
plot(df.ci$pop15, df.ci$ci.upper, type="l", col="blue", xlim=x.lim, ylim=y.lim, xlab="", ylab="")
# 予測の信頼区間
par(new=TRUE)
plot(df.ci$pop15, df.ci$ci.pred.lower, type="l", col="red", xlim=x.lim, ylim=y.lim, xlab="", ylab="")
par(new=TRUE)
plot(df.ci$pop15, df.ci$ci.pred.upper, type="l", col="red", xlim=x.lim, ylim=y.lim, xlab="", ylab="")


### 重回帰: pop15, pop75, dpi, ddpi
full.m <- lm(sr~pop15+pop75+dpi+ddpi, data=LifeCycleSavings)
summary(full.m)
# R2 0.3385 単回帰に比べ、モデルの説明力はアップ
# pop15とddpiが有意

## 回帰係数の信頼区間
confint(full.m, level=0.95)

## 予測値と実測値プロット
plot(LifeCycleSavings$sr, predict(full.m), xlab="Actual", ylab="Predict", xlim=c(0,23), ylim=c(0,23))
abline(0, 1, col="red")

## 残差プロット
plot(predict(full.m), resid(full.m), ylim=c(-10,10), xlab="Predict", ylab="Residual")
abline(0, 0, col="red")


## 予測と回帰の信頼区間、予測区間
predict(full.m, interval="confidence", level=0.95)
predict(full.m, interval="prediction", level=0.95)


### 新規データに対する予測
new.data <- data.frame(pop15=c(25,35,45),
                       pop75=c(1,2,3),
                       dpi=c(300,700,1100),
                       ddpi=c(2,3,4))
new.data

predict(full.m, newdata=new.data)



##################### Day4-Exercise2 ダミー変数 #####################

#install.packages("dummies")
library(dummies)

csvname1 <- "MediaEffect.csv"
csvPATH1 <- file.path("./data",csvname1)

media <- read.csv(csvPATH1) 
str(media)
summary(media)

## ダミー変数の作成
dummy_var <- dummy(media$Media)

## 元データへの結合
dummy_media <- cbind(media, dummy_var)

str(dummy_media)    # Factor型がInt型となっていることを確認
summary(dummy_media)


## 3水準変数を確認
# irisデータのSpecies変数
View(iris)
str(iris)

## ダミー変数の作成
dummy_species <- dummy(iris$Species, sep=".")
dummy_iris <- cbind(iris, dummy_species)
View(dummy_iris)


### 分析における比較

#Media: factor
m1 <- lm(N_New  ~ Expence + Media, data=dummy_media)
summary(m1)

#media_B: integer(dummy)
m2 <- lm(N_New  ~ Expence + MediaB, data=dummy_media)
summary(m2)

#media_A: integer(dummy)
m3 <- lm(N_New  ~ Expence + MediaA, data=dummy_media)
summary(m3)

#media: dummy
m4 <- lm(N_New  ~ Expence+ MediaA + MediaB, data=dummy_media)
summary(m4)
# 両方入れては片方が意味がない(この場合, 後の変数のMediaB)

## 3水準変数の場合
m101 <- lm(Sepal.Length~Species, data=dummy_iris)
summary(m101)
# 2水準のみの結果が表示されている



##################### Day4-Exercise3 共分散分析 #####################

## プロットでデータを確認
plot(media$Expence, media$N_New, col=media$Media)
legend("topleft", legend=c("Media A","Media B"), pch=1, col=c("black","red"))

#by(media$N_New, media$Media, summary)


# 共変量(Expence)なし
m_no_covariance <- lm(N_New ~ Media , data=media)
summary(m_no_covariance)
# MediaB: 係数 96.61  p値 0.0075

# 共変量(Expence)あり
m_covariance <- lm(N_New ~ Media + Expence, data=media)
summary(m_covariance)
# MediaB: 係数 63.501  p値 0.0132

# 広告費用を考慮したうえで、広告媒体A,B間で入会者に与える影響は有意に異なると言える


## 散布図とMedia別に推定した回帰直線を重ね合わせる
plot(media$Expence, media$N_New, col=media$Media)
legend("topleft", legend=c("Media A","Media B"), pch=1, col=c("black","red"))
# Media A,B別の直線
intercept <- m_covariance$coefficients[1]
effectB <- m_covariance$coefficients[2]
slope <- m_covariance$coefficients[3]
abline(a=intercept, b=slope, col="black")   # Media A
abline(a=intercept+effectB, b=slope, col="red")   # Media B


## 本来、共分散分析が実行積る前提として、二直線の傾きが等しいとの仮定がある
## （二直線の傾きが等しいとの前提の上、直線の切片が異なるかを分析）
## MediaとExpenceの交互作用を確認することにより、二直線の傾きが等しいか確認することになる
## MediaとExpenceの交互作用が有意になった場合、二直線の傾きは異なることになる
m_interaction <- lm(N_New ~ Media + Expence + Media:Expence, data=media)
summary(m_interaction)
# p値0.55828で有意とならない

## Media AとBを別に推定して、重ね合わせた場合
plot(media$Expence, media$N_New, col=media$Media)
legend("topleft", legend=c("Media A","Media B"), pch=1, col=c("black","red"))
# Media別に回帰分析
m_mediaA  <- lm(N_New ~ Expence, data=media[media$Media=="A",])
m_mediaB  <- lm(N_New ~ Expence, data=media[media$Media=="B",])
abline(m_mediaA, col="black")
abline(m_mediaB, col="red")



#################### Day4-Exercise4 多重共線性 ####################

csvname2 <- "Multicollinearity.csv"
csvPATH2 <- file.path("./data",csvname2)

dat <- read.csv(csvPATH2) 

str(dat)
summary(dat)

plot(dat)
# yとx1,x2間の線形の関係が強そう。また、x1とx2間にも強い線形の関係が見られる

cor(dat)   # 相関係数(1または-1に近いほど直線関係が強い) Day5で学習

# x1,x2,x3
res_123 <- lm(y ~ x1 + x2 + x3, data = dat)
summary(res_123)

# x1,x2
res_12 <- lm(y ~ x1 + x2, data = dat)
summary(res_12)

# x1,x3
res_13 <- lm(y ~ x1 + x3  , data = dat)
summary(res_13)

# x2,x3
res_23 <- lm(y ~ x2 + x3  , data = dat)
summary(res_23)

# x1
res_1 <- lm(y ~ x1, data = dat)
summary(res_1)

# x2
res_2 <- lm(y ~ x2, data = dat)
summary(res_2)

# x3
res_3 <- lm(y ~ x3, data = dat)
summary(res_3)


####  おまけ  ####
#install.packages("car")
library(car)

## VIF(Variance Inflation Factor)
# https://en.wikipedia.org/wiki/Variance_inflation_factor
# 多重共線性の強さを示す指標
# 2.5〜3を超えるあたりから気をつける必要がある
vif(res_123)
vif(res_12)
vif(res_13)
vif(res_23)


## AIC
AIC(res_123, res_12, res_13, res_23, res_1, res_2, res_3)



##################### Day4-Exercise5 変数選択法 #####################

datname3 <- "unicef98.dat"
datPATH3 <- file.path("./data", datname3)

unicef <- read.table(datPATH3, header=TRUE)  # csvではなくtsv

summary(unicef)
# y: Child.Mortality 
# X1: Literacy.Fem
# X2: Literacy.Ad
# X3: Drinking.Water
# X4: Polio.Vacc 
# X5: Tetanus.Vacc.Preg
# X6: Urban.Pop 
# X7: Foreign.Aid

#NAが入っていたので、一旦取り除く
unicef = unicef[complete.cases(unicef), ]
summary(unicef)

unicef_lm_full <- lm(Child.Mortality~Literacy.Fem+Literacy.Ad+Drinking.Water+Polio.Vacc+Tetanus.Vacc.Preg+Urban.Pop+Foreign.Aid,
                     data=unicef)
summary(unicef_lm_full)
#R2=74.4 RSS=36.27 変数のp-value いくつか > 0.1

### AICによる変数選択
library(MASS)

## 増減法"both"
resAIC <- stepAIC(unicef_lm_full, direction="both")
# direction引数で、"both", "backward", "forward"を指定

# 選ばれた変数での結果
summary(resAIC)


## 減少法"backward"
stepAIC(unicef_lm_full, direction="backward")

## 増加法forward"
stepAIC(lm(Child.Mortality~1, data=unicef), 
        direction="forward",
        scope=list(upper=~Literacy.Fem+Literacy.Ad+Drinking.Water+Polio.Vacc+Tetanus.Vacc.Preg+Urban.Pop+Foreign.Aid)
        )



######### Day4-Exercise6 ランダムフォレストによる変数のスクリーニング #########

#install.packages("randomForest")
library(randomForest)

##上と同様のデータで行う

rf <- randomForest(Child.Mortality~Literacy.Fem+Literacy.Ad+Drinking.Water+Polio.Vacc+Tetanus.Vacc.Preg+Urban.Pop+Foreign.Aid,
                   data=unicef)

# 説明変数の記述を省く場合
rf <- randomForest(Child.Mortality~., data=unicef[-1])
rf

# show features
importance(rf)
varImpPlot(rf)  #値が大きいほど、基本的に重要度が高い.


####  おまけ  ####
## 機械学習ライクなデータの指定方法

# 特徴量
inputVars <- c("Literacy.Ad", "Literacy.Fem", "Drinking.Water",
               "Polio.Vacc", "Tetanus.Vacc.Preg","Urban.Pop", "Foreign.Aid")
# ターゲット
targetVar<- "Child.Mortality"

X <- unicef[,inputVars]
Y <- unicef[, targetVar]

rf <- randomForest(x=X, y=Y, importance=TRUE)


## Classificationの実行も同様に可能
head(iris)

#feature
X <- iris[,1:4]
Y <- iris[,5]

#classifier
rf_c <- randomForest(x=X, y=Y, importance=T)
rf_c

# show features
print(importance(rf_c))
varImpPlot(rf_c)

