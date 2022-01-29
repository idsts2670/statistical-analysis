#データの読み込み
shp <- read.csv("失敗.csv")

#データフレームの確認
head(shp)

#モデルの記述
shp.model <- '
  失敗不安~叱責
  学習意欲~励まし
  学業成績~失敗不安+学習意欲
'
#母数の推定
library(lavaan) #パッケージの読み込み
shp.fit <- sem(shp.model, data=shp) #母数の推定

#パス解析の結果の出力
summary(shp.fit, standardized = TRUE, rsquare = TRUE)

#信頼区間の出力も出せる
summary(shp.fit, standardized = TRUE, rsquare = TRUE, ci = TRUE)


#モデルの評価とモデルの修正
#適合度指標の出力
summary(shp.fit, standardized = TRUE, rsquare = TRUE, ci = TRUE, fit.measures = TRUE) 

#パッケージlavaanで求められる全ての分析結果を表示
fitmeasures(shp.fit)


#モデルの修正 - 修正指標
#修正指標の出力
modindices(shp.fit)

#誤差変数の間に相関を仮定したモデルの記述
shp.model2 <- '
  失敗不安~叱責
  学習意欲~励まし
  学業成績~失敗不安+学習意欲
  失敗不安~~学習意欲
  '

#誤差変数の間に相関を仮定したモデルでの母数推定
shp.fit2 <- sem(shp.model2, data=shp)
summary(shp.fit2, standardized=TRUE, rsquare=TRUE,ci=TRUE, fit.measures=TRUE)

#母数の推定
#標本共分散行列の算出
shp.cov <- cov(shp) #パス解析では共分散行列に着目して計算が行われるからデータ行列がなくても、共分散行列と標本サイズがあればパス解析を実行することができる
#標本共分散行列をもとにした母数の推定
shp.cov.fit <- sem(shp.model2, sample.co=shp.cov, sample.nobs=500) #標本サイズは500
summary(shp.cov.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE)


#章末演習
#データの読み込み
hyk <- read.csv('授業評価.csv')
#データフレームの確認
head(hyk)

#問1:モデルの記述
hyk.model <- '
  興味~困難度
  興味~有用性
  学習行動~興味
  成績~学習行動
  '
#問2:母数の推定
hyk.fit <- sem(hyk.model, data=hyk)

#問3:モデルの当てはまりのよさを評価
summary(hyk.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE)
#適合度指数は,CFI=.813,TLI=.664,RMSEA=.113(90%CI[.070,.160]), SRMR=.079であり、
#CFI%TLI >= .95, RMSEA <= .06, SRMR <=0.08であれば適合していると判断できることを踏まえると、
#SRMR以外は基準を満たしていないため、モデルのデータに対する当てはまりは良くないと言える。

#問4:修正指標の提示
modindices(hyk.fit) #修正指標の出力
#学習行動←困難度,困難度←学習行動の変数間に相関があることを仮定して1つ目のモデルを加える修正案が考えられる

#問5:モデルの修正とモデルの評価
hyk.model2 <- '
  興味~困難度+有用性
  学習行動~興味+困難度
  成績~学習行動
  困難度~学習行動
  '
hyk.fit2 <- sem(hyk.model2, data=hyk)
summary(hyk.fit2, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE)
#適合度指数は,CFI=.976,TLI=.940,RMSEA=.057(90%CI[.000,.116]),SRMR=.040であり、TLI以外は基準を超えているため、改善されたモデルの当てはまりは十分と言える

#問6:パス係数の信頼区間
#興味~困難度:[0.198,10.824]
#興味~有用性:[0.288,4.685]
#学習行動~興味:[5.723,15.778]
#学習行動~困難度:[1.333,10.805]
#成績~学習行動:[0.358,0.636]
#困難度~学習行動:[-0.961,-0.431]

