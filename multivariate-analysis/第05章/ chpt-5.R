kwz <- read.csv('井の中の蛙.csv')
head(kwz)
#マルチレベルモデルによる分析

#級内相関係数を算出してマルチレベルモデルの必要性を判断
install.packages("ICC")
library(ICC)
ICCest(as.factor(学級), 学業的自己概念, data=kwz, alpha=0.05, CI.type = 'Smith') #学業的自己概念の級内相関係数を求める

#中心化について
kwz$テスト得点.m <- ave(kwz$テスト得点, kwz$学級) #テスト得点について集団（学級）ごとに平均（学級平均）値の算出
kwz$テスト得点.cwc <- kwz$テスト得点 - kwz$テスト得点.m #集団平均中心化
kwz$テスト得点.cgm <- kwz$テスト得点 - mean(kwz$テスト得点) #全体平均中心化
round(cor(kwz[, c('テスト得点.m', 'テスト得点.cwc','テスト得点.cgm')]), 3) #集団平均値と中心化後の変数との相関係数の算出
head(kwz$テスト得点.cwc)

#ランダム切片モデル
#「学業水準の高い生徒ほど学業的自己概念は高い傾向にあるのか」について
#「切片」は学級によって異なり、「傾き」は学級間で等しいことと仮定してマルチレベルモデル分析をする
install.packages("lmerTest")
library(lmerTest)
model1.cwc <- lmer(学業的自己概念~テスト得点.cwc+(1|学級), data=kwz, REML=FALSE)
summary(model1.cwc)

#ランダム傾きモデル
#上記で計算したランダム切片に加えて傾きも学級間で変動することを仮定したモデルを作成
model2.cwc <- lmer(学業的自己概念~テスト得点.cwc+(1+テスト得点.cwc|学級), data=kwz, REML=FALSE)
summary(model2.cwc)

#集団レベルの変数の効果を検討するためのマルチレベルモデル
#テスト得点の学級平均値の中心化
kwz$テスト得点.cm <- kwz$テスト得点.m - mean(kwz$テスト得点)
#集団レベルの変数の効果を検討するマルチレベルモデル
model3.cgm <- lmer(学業的自己概念~テスト得点.cgm + テスト得点.cm + (1+テスト得点.cgm|学級), data=kwz, REML=FALSE)
summary(model3.cgm)

#クロスレベルの交互作用効果を検討するためのマルチレベルモデル
model4.cgm <- lmer(学業的自己概念~テスト得点.cgm + テスト得点.cm + テスト得点.cgm*テスト得点.cm + (1+テスト得点.cgm|学級), data=kwz, REML = FALSE)
summary(model4.cgm)



