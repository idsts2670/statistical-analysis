#データの読み込み
kwz <- read.csv("井の中の蛙.csv")

#データフレームの確認
head(kwz)

#級内相関係数の算出
library(ICC) #パッケージICCの読み込み
ICCest(as.factor(学級), 学業的自己概念, data=kwz, alpha=0.05, CI.type="Smith")

#中心化
kwz$テスト得点.m <- ave(kwz$テスト得点, kwz$学級) #テスト得点の集団平均（学級平均）値の算出
kwz$テスト得点.cwc <- kwz$テスト得点-kwz$テスト得点.m #集団平均中心化
kwz$テスト得点.cgm <- kwz$テスト得点-mean(kwz$テスト得点) #全体平均中心化

#相関係数の算出
round(cor(kwz[,c("テスト得点.m", "テスト得点.cwc", "テスト得点.cgm")]), 3)

#ランダム切片モデル
library(lmerTest) #パッケージlmerTestの読み込み
model1.cwc <- lmer(学業的自己概念‾テスト得点.cwc+(1|学級), data=kwz, REML=FALSE)
summary(model1.cwc)

#ランダム傾きモデル
model2.cwc <- lmer(学業的自己概念‾テスト得点.cwc+(1+テスト得点.cwc|学級), data=kwz, REML=FALSE)
summary(model2.cwc)

#テスト得点の学級平均値の中心化
kwz$テスト得点.cm <- kwz$テスト得点.m-mean(kwz$テスト得点)

#集団レベルの変数を含むマルチレベルモデル
model3.cgm <- lmer(学業的自己概念‾テスト得点.cgm+テスト得点.cm+(1+テスト得点.cgm|学級), data=kwz, REML=FALSE)
summary(model3.cgm)

#クロスレベルの交互作用項を含むマルチレベルモデル
model4.cgm <- lmer(学業的自己概念‾テスト得点.cgm+テスト得点.cm+テスト得点.cgm*テスト得点.cm+(1+テスト得点.cgm|学級), data=kwz, REML=FALSE)
summary(model4.cgm)

#モデル比較
anova(model3.cgm, model4.cgm)

#信頼区間の算出
confint(model4.cgm, method="Wald")


#演習と解答
kch <- read.csv("価値.csv")

#問1解答
library(ICC)
ICCest(as.factor(学生), 興味, data=kch, alpha=0.05, CI.type="Smith")

#問2解答
kch$価値.cwc <- kch$価値-ave(kch$価値, kch$学生)
kch$期待.c <- kch$期待-mean(kch$期待)

#問3解答
library(lmerTest)
model1 <- lmer(興味‾価値.cwc+(1|学生), data=kch, REML=FALSE)
summary(model1)

#問4解答
model2 <- lmer(興味‾価値.cwc+(1+価値.cwc|学生), data=kch, REML=FALSE)
summary(model2)

#問5解答
model3 <- lmer(興味‾価値.cwc+期待.c+価値.cwc*期待.c+(1+価値.cwc|学生), data=kch, REML=FALSE)
summary(model3)

#問6解答
anova(model1, model2, model3)

