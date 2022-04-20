#データの読み込み
shp <- read.csv("失敗.csv")

#データフレームの確認
head(shp)

#モデルの記述
shp.model <- '
 失敗不安‾叱責
 学習意欲‾励まし
 学業成績‾失敗不安+学習意欲
'

#母数の推定
library(lavaan) # パッケージlavaanの読み込み
shp.fit <- sem(shp.model, data=shp) #母数の推定

#結果の出力（標準化推定値，決定係数）
summary(shp.fit, standardized=TRUE, rsquare=TRUE)

#結果の出力（標準化推定値，決定係数，信頼区間）
summary(shp.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE)

#結果の出力（標準化推定値，決定係数，信頼区間，適合度指標）
summary(shp.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE)

#すべての適合度指標の出力
fitmeasures(shp.fit)

# 修正指標の出力
modindices(shp.fit)

#誤差変数の間に相関を仮定したモデルで分析
shp.model2 <- ' #モデルの記述
 失敗不安‾叱責
 学習意欲‾励まし
 学業成績‾失敗不安+学習意欲
 失敗不安‾‾学習意欲
'
shp.fit2 <- sem(shp.model2, data=shp) #母数の推定
summary(shp.fit2, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE) #結果の出力

# 標本共分散行列をもとにした分析
shp.cov <- cov(shp) #標本共分散行列の算出
shp.cov.fit <- sem(shp.model2, sample.cov=shp.cov, sample.nobs=500) #母数の推定
summary(shp.cov.fit, standardized=TRUE, rsquare=TRUE, ci=TRUE, fit.measures=TRUE) #結果の出力


#演習と解答
#問1解答
hyk <- read.csv("授業評価.csv")
hyk.model <- '
 興味‾困難度+有用性
 学習行動‾興味
 成績‾学習行動
'

#問2解答
hyk.fit <- sem(hyk.model, data=hyk)

#問3解答
summary(hyk.fit, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE)

#問4解答
modindices(hyk.fit)

#問5解答
hyk.model2 <- '
 興味‾困難度+有用性
 学習行動‾興味+困難度
 成績‾学習行動
'
hyk.fit2 <- sem(hyk.model2, data=hyk)
summary(hyk.fit2, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE)

#問6解答
summary(hyk.fit2, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE, ci=TRUE)



