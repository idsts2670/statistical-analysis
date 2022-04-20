#データの読み込み，データフレームの確認
sws <- read.csv("幸せ調査.csv") #データの読み込み
head(sws)

#モデルの記述（係数の固定）
sws.model1 <- "
f1 =‾ 1 * E1 + E2 + E3
f2 =‾ 1 * R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + f2
f4 ~ f1 + f2 +f3
f1 ~~ f2
"
#モデルの記述（分散の固定）
sws.model2 <- "
f1 =~ E1 + E2 + E3
f2 =~ R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + f2
f4 ~ f1 + f2 + f3
f1 ~~ 1 * f1 + f2
f2 ~~ 1 * f2
"

#モデルの推定
library(lavaan)
sws.fit <- lavaan(model = sws.model1, data = sws, auto.var = TRUE) #auto.var-外生変数の分散を推定対象とする（TRUE）,しない（FALSE）
# sws.fit<-lavaan(model = sws.model1, sample.cov = cov(sws), sample.nobs = nrow(sws), auto.var = TRUE) #標本共分散行列とサンプルサイズを指定して推定

#結果の出力
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE) #fit.measures-適合度, standardizes-標準化推定値, ci-95%信頼区間を出力する（TRUE）

#すべての適合度指標を出力
fitmeasures(sws.fit)

#モデル適合に関する部分的評価の指標（残差行列）
residuals(sws.fit, type = "cor")

#モデル適合に関する部分的評価の指標（修正指標）
modindices(sws.fit)

#分散説明率の出力
lavInspect(sws.fit, "rsquare")

#推定値付パス図の描画
library(semPlot)
semPaths(sws.fit, whatLabels = "std", layout = "tree2", curve = 1.2, #whatLabels-パスに付与する数値の種類, layout-パス図の形式, curve-両方向矢印の曲がりの程度
	optimizeLatRes = TRUE, edge.color = "black", nCharNodes = 0, #optimizeLatRes-潜在変数に刺さる誤差の角度を調整する, edge.color-矢印の色, nCharNodes-図形内に表示する文字数
	edge.label.position = c(rep(0.4, 17), rep(0.5, 18)), edge.label.cex = 0.8)#edge.label.position-矢印上の数値表示位置（母数の数の分だけ指定する。共分散は二つ分カウントされる）

#モデルの記述（母数の関数の定義を含む）
sws.model3<-"
f1 =~ 1 * E1 + E2 + E3
f2 =~ 1 * R1 + R2 + R3
f3 =~ 1 * M1 + M2 + M3
f4 =~ 1 * H1 + H2 + H3
f3 ~ f1 + a * f2
f4 ~ f1 + b * f2 +c * f3
f1 ~~ f2
DRE  := b
IDRE := a * c
TTE  := b + a * c
"
sws.fit <- lavaan(model = sws.model3, data = sws, auto.var = TRUE)
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)


#演習と解答

#問3解答（例）
sws <- read.csv("幸せ調査.csv")
sws.model<-"
f1 =~E1 + E2 + 1 * E3
f2 =~R1 + R2 + 1 * R3
f3 =~M1 + M2 + 1 * M3
f4 =~H1 + H2 + 1 * H3
f1 ~f3
f4 ~f2 + f3
"
sws.fit <- lavaan(model = sws.model, data = sws, auto.var = TRUE)

#問4，問5
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

