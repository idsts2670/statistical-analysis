#データの読み込み
dkk <- read.csv("動機づけ.csv")

#データフレームの確認
head(dkk)

#モデルの記述
dkk.model <- '
 F1=‾I1+I2+I3+I4
 F2=‾E1+E2+E3+E4
'

#母数の推定
library(lavaan) # lavaanパッケージの読み込み
dkk.fit <- cfa(dkk.model, data=dkk, std.lv=TRUE) #母数の推定

#結果の出力
summary(dkk.fit, fit.measures=TRUE, standardized=TRUE)


#データの読み込み
math <- read.csv("数学テスト.csv")

#データフレームの確認
head(math)

#モデルの記述
math.model <- '
 F1=‾math1+math2+math3+math4+math5+math6+math7+math8+math9+math10
'

#母数の推定
math.fit <- cfa(math.model, data=math, ordered=c("math1", "math2", "math3", "math4", "math5", "math6", "math7", "math8", "math9", "math10"), std.lv=TRUE)

#結果の出力
summary(math.fit, fit.measures=TRUE, standardized=TRUE)

#因子が1つで観測変数が2つの確認的因子分析
math.model2 <- ' #モデルの記述
 F1=‾math1+math2
'
math.fit2 <- cfa(math.model2, data=math, ordered=c("math1", "math2"), std.lv=TRUE) #母数の推定

#等値制約
math.model3 <- ' #モデルの記述
 F1=‾b*math1+b*math2
'  
math.fit3 <- cfa(math.model3, data=math, ordered=c("math1", "math2"), std.lv=TRUE) #母数の推定
summary(math.fit3, fit.measures=TRUE, standardized=TRUE) #結果の出力

#因子負荷を1に固定した確認的因子分析
dkk.fit2 <- cfa(dkk.model, data=dkk) #母数の推定
summary(dkk.fit2, fit.measures=TRUE, standardized=TRUE) #結果の出力

# 不適解
dkk.model3 <- ' #モデルの記述
 F1=‾I1+I2
 F2=‾I3+I4
'
dkk.fit3 <- cfa(dkk.model3, data=dkk, std.lv=TRUE) #母数の推定
summary(dkk.fit3, fit.measures=TRUE, standardized=TRUE) #結果の出力



#データの読み込み
knj <- read.csv("感情.csv")

#データフレームの確認
head(knj)

#モデルの記述（4因子モデル）
knj.model1 <- '
 F1=‾A1+A2+A3
 F2=‾C1+C2+C3
 F3=‾M1+M2+M3
 F4=‾P1+P2+P3
'
knj.fit1 <- cfa(knj.model1, data=knj, std.lv=TRUE) #母数の推定
summary(knj.fit1, fit.measures=TRUE, standardized=TRUE) #結果の出力

#モデルの記述（2次因子モデル）
knj.model2 <- '
 F1=‾A1+A2+A3
 F2=‾C1+C2+C3
 F3=‾M1+M2+M3
 F4=‾P1+P2+P3
 H=‾F1+F2+F3+F4
'
knj.fit2 <- cfa(knj.model2, data=knj, std.lv=TRUE) #母数の推定
summary(knj.fit2, fit.measures=TRUE, standardized=TRUE) #結果の出力



#演習と解答
#問1解答
skk <- read.csv("性格.csv")
skk.model1 <- '
 F1=~温和+陽気+外向的+親切+社交的+協力的+積極的+素直
'
skk.fit1 <- cfa(skk.model1, data=skk, std.lv=TRUE)
summary(skk.fit1, fit.measures=TRUE, standardized=TRUE)

#問2解答
skk.model2 <- '
 F1=~陽気+外向的+社交的+積極的
 F2=~温和+親切+協力的+素直
'
skk.fit2 <- cfa(skk.model2, data=skk, std.lv=TRUE)
summary(skk.fit2, fit.measures=TRUE, standardized=TRUE)

