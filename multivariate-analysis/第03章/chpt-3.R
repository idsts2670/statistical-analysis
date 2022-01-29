#モデル作成と母数の推定・診断
csdat <- read.csv('顧客満足度データ.csv') #データの読み込み
head(csdat, 3)
res1 <- lm(顧客数~立地満足度+設備満足度+店舗面積満足度+トレーナー満足度, data=csdat)
summary(res1) #出力の一部
#多重共線性のチェック
cor(csdat$トレーナー満足度, csdat$トレーナー数) #説明変数間の相関係数
resm1 <- lm(顧客数~トレーナー満足度, data=csdat) #単回帰分析
summary(resm1)
resm2 <- lm(顧客数~トレーナー満足度+トレーナー数, data=csdat) #重回帰分析
summary(resm2) #resm1とresm2で比較
#VIFの算出例
library(car) #パッケージcarの読み込み
vif(resm2) #関数vifによるVIFの算出 → 10以上であるので両者は多重共生線の要因になっている
vif(res1) #顧客満足度の4変数に関するVIFの算出 → どの説明変数のVIFも2を下回っているため大丈夫
#偏回帰係数の信頼区間
confint(res1, level = 0.95)
#単位の異なる説明変数が混在する場合の対処法
#標準偏回帰係数の算出
scsdat <- as.data.frame(scale(csdat))#データの標準化とデータフレーム化
res2 <- lm(顧客数~立地満足度+トレーナー数, data=scsdat)
summary(res2) #出力の一部
#質的変数も含めた重回帰分析
res3 <- lm(顧客数~立地満足度+設備満足度+店舗面積満足度+トレーナー満足度+接客研修+入会特典, data=csdat)
summary(res3)
#AICの算出
extractAIC(res1) #説明変数が量的変数のみのモデル
extractAIC(res3) #説明変数に質的変数も含めた回帰モデル
#BICの算出
extractAIC(res1, k=log(30)) #説明変数が量的変数のみの回帰モデル
extractAIC(res3, k=log(30)) #説明変数に質問変数も含めた回帰モデル

#切片と偏回帰係数の信頼区間の検証
qt(0.025, 25) #下限確率0.025を与えるqtの算出
qt(0.975, 25) #上限確率0.025を与えるqtの算出
信頼区間の算出
21.640-2.059539*7.036 #信頼区間の下限
21.640+2.059539*7.036 #信頼区間の上限 confintの値と誤差の範囲で一致している

#章末演習
kamokudat <- read.csv('科目内試験結果.csv')
dim(kamokudat) #行数列数
colnames(kamokudat) #変数名確認
restest <- lm(final~t1+t2+t3+t4+t5, data=kamokudat)#重回帰分析
library(car)
vif(restest) #VIFで多重共線性の判定
restest2 <- lm(final~t1+t3+t4+t5, data=kamokudat)
summary(restest2)
confint(restest2, level = 0.95)
restest3 <- lm(final~t1+t2, data=kamokudat) #t1とt2だけで重回帰分析
extractAIC(restest2, k=log(30)) #AICの結果からrestest2の方がデータに適合しているとわかる
extractAIC(restest3, k=log(30))
round(cor(kamokudat), 2)

