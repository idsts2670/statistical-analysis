#データの読み込み、データフレームの確認
sws <- read.csv('幸せ調査.csv') #データの読み込み
head(sws)
#モデルの記述（係数の固定）
sws.model1 <- '
  f1=~1*E1+E2+E3
  f2=~1*R1+R2+R3
  f3=~1*M1+M2+M3
  f4=~1*H1+H2+H3
  f3~f1+f2
  f4~f1+f2+f3
  f1~~f2'
#モデルの記述（分散の固定）
sws.model2 <- '
  f1=~E1+E2+E3
  f2=~R1+R2+R3
  f3=~1*M1+M2+M3
  f4=~1*H1+H2+H3
  f3~f1+f2
  f4~f1+f2+f3
  f1~~1*f1+f2 #分散を固定して、f1とf2の相関を表現
  f2~~1*f2 #分散を固定
'
#モデルの推定および評価
library(lavaan)
sws.fit <- lavaan(model=sws.model1, data=sws, auto.var=TRUE)
#結果の出力
summary(sws.fit, fit.measures=TRUE, standardized=TRUE, ci=TRUE) #モデルの適合度指標・標準化推定値・モデル母数に関する95%信頼区間の出力は制御
#全ての適合度指標を出力
fitmeasures(sws.fit)

#適合度の悪さの詳細と修正の可能性の追究
#モデル適合に関する部分的評価の指標（残差行列）
residuals(sws.fit, type='cor')
#モデル適合に関する部分的評価の指標（修正指標）
modindices(sws.fit)

#内生変数に対する影響や内生変数の説明率の確認
#分散説明率の出力
lavInspect(sws.fit, 'rsquare')

#パス図による変数間の関係の視覚的な確認
#推定値つきパス図の描画
install.packages('semPlot')
library(semPlot)
semPaths(sws.fit, whatLabels = 'std', layout = 'tree2', curve = 1.2, 
         optimizeLatRes = TRUE, edge.color = 'black', nCharNodes = 0, 
         edge.label.position = c(rep(0.4, 17), rep(0.5, 18)), edge.label.cex =0.8)
#母数の関数として表現される量の定義と推定
#モデルの記述（母数の関数の定義を含む）
sws.model3<-"
f1=~1*E1+E2+E3
f2=~1*R1+R2+R3
f3=~1*M1+M2+M3
f4=~1*H1+H2+H3
f3~f1+a*f2
f4~f1+b*f2+c*f3
f1~~f2
DRE:=b
IDRE:=a*c
TTE:=b+a*c
"
#モデルの推定及び評価
sws.fit <- lavaan(model = sws.model3, data = sws, auto.var = TRUE)
summary(sws.fit, fit.measures = TRUE, standardized = TRUE, ci = TRUE)


#章末問題
#問1
#心の健康、人間関係の良好さ

#問2
#51(モデル推定のために固定する係数は除いて計算する)


#問3
sws.model4 <-"
  f1=~1*E1+E2+E3
  f2=~1*R1+R2+R3
  f3=~1*M1+M2+M3
  f4=~1*H1+H2+H3
  f1~f3
  f4~f2+f3
  "

sws.fit1 <- lavaan(model=sws.model4, data=sws, auto.var=TRUE)
summary(sws.fit1, fit.measures=TRUE, standardized=TRUE, ci=TRUE)

#問4
#CFI, TLIの数値は良好である一方でRMSEA, SRMRの数値は基準より高いためモデルがデータによく適合しているとは言えない

#問5
#ほとんど誤差だが、"人間関係の良好さ"(=0.350)の方が"心の健康"(=0.330)より若干影響が強い





