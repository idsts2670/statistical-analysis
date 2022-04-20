#データの読み込み、データフレームの確認
dkk <- read.csv('動機づけ.csv') #データの読み込み
head(dkk)

#確認的因子分析の実行
#因子分析モデルの記述
dkk.model <- 
  'F1 = ~I1+I2+I3+I4
  F2 = ~E1+E2+E3+E4'

#母数の推定
library(lavaan) #パッケージの読み込み
dkk.fit <- cfa(dkk.model, data=dkk, std.lv=TRUE)
#結果の出力
summary(dkk.fit, fit.measures=TRUE, standardized=TRUE) #CFI,TLI,RMSEA,SRMRの全てで基準をクリアしている

#順序カテゴリカル変数を扱った確認的因子分析
math <- read.csv('数学テスト.csv') #データの読み込み
head(math)
math.model <- 'F1=~math1+math2+math3+math4+math5+math6+math7+math8+math9+math10'
#母数の推定
math.fit <- cfa(math.model, data=math, ordered = c('math1', 'math2', 'math3', 'math4', 'math5', 'math6', 'math7', 'math8', 'math9', 'math10'), std.lv=TRUE)
#結果の出力
summary(math.fit, fit.measures=TRUE, standardized=TRUE)

#モデルの識別性
#モデルの識別性と等値制約
#因子が1つで観測変数が2つの確認的因子分析
math.model2 <- 'F1=~math1+math2' #モデルの記述
math.fit2 <- cfa(math.model2, data=math, ordered=c('math1', 'math2'), std.lv=TRUE) #母数の推定 #自由度が-1（=(2×3)/2-4）になるためモデルは認識されない

#等値制約
#母数をある値に固定したり、ある母数と別の母数の値が等しいとしたりすること
math.model3 <- 
  'F1 = ~b*math1 + b*math2
  ' #モデルの記述
math.fit3 <- cfa(math.model3, data=math, ordered=c('math1', 'math2'), std.lv=TRUE) #母数の推定
summary(math.fit3, fit.measures=TRUE, standardized=TRUE) #結果の出力

#モデルの識別性と母数の制約
#因子負荷を1に固定した確認的因子分析
dkk.model <- 
  'F1 = ~I1+I2+I3+I4
   F2 = ~E1+E2+E3+E4
  '
dkk.fit2 <- cfa(dkk.model, data=dkk) #母数の推定
summary(dkk.fit2, fit.measures=TRUE, standardized=TRUE) #結果の出力

#不適解の問題
dkk.model3 <-
  'F1=~I1+I2
   F2=~I3+I4
  '
dkk.fit3 <- cfa(dkk.model3, data=dkk, std.lv=TRUE) #母数の推定
summary(dkk.fit3, fit.measures=TRUE, standardized=TRUE) #結果の出力

#高次因子分析（ここでは2次因子モデル vs 4因子モデルを適合度の観点から比較する）
#データの読み込み、データフレームの確認
knj <- read.csv('感情.csv') #データの読み込み
head(knj)

#確認的因子分析の実行

#4因子モデル
knj.model1 <- '
  F1=~A1+A2+A3
  F2=~C1+C2+C3
  F3=~M1+M2+M3
  F4=~P1+P2+P3
  '
knj.fit1 <- cfa(knj.model1, data=knj, std.lv=TRUE) #母数の推定
summary(knj.fit1, fit.measures=TRUE, standardized=TRUE) #結果の出力

#2次因子モデル（高次因子モデル）
knj.model2 <- '
  F1=~A1+A2+A3
  F2=~C1+C2+C3
  F3=~M1+M2+M3
  F4=~P1+P2+P3
  H=~F1+F2+F3+F4'
knj.fit2 <- cfa(knj.model2, data=knj,std.lv=TRUE) #母数の推定
summary(knj.fit2, fit.measures=TRUE, standardized=TRUE) #結果の出力 #AIC, BICの値は2次因子モデルの方が小さく、当てはまりが良いと示された

#章末問題
#問1
pkk <- read.csv('性格.csv')
head(pkk)
pkk.model1 <- '
  P1 = ~温和+陽気+外向的+親切+社交的+協力的+積極的+素直
  '
pkk.fit <- cfa(pkk.model1, data=pkk, std.lv=TRUE)
summary(pkk.fit, fit.measures=TRUE, standardized=TRUE)

#問2
pkk.model2 <- '
  F1 = ~陽気+外向的+社交的+積極的
  F2 = ~温和+親切+協力的+素直
  '
pkk.fit2 <- cfa(pkk.model2, data=pkk, std.lv=TRUE)
summary(pkk.fit2, fit.measures=TRUE, standardized=TRUE) #2因子モデルの方がAIC, BIC共に小さいためベター












