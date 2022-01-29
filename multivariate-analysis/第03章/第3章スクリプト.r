#データの読み込み
csdat <- read.csv("顧客満足度データ.csv")
head(csdat,3)


#関数lmによる重回帰分析の実行
res1<-lm(顧客数‾立地満足度+設備満足度+店舗面積満足度+トレーナー満足度,data=csdat)
summary(res1)　

#説明変数間の相関係数
cor(csdat$トレーナー満足度,csdat$トレーナー数)

#説明変数間の相関が高い場合の重回帰分析の出力
resm1 <- lm(顧客数‾トレーナー満足度,data=csdat)
summary(resm1)

resm2 <- lm(顧客数‾トレーナー満足度+トレーナー数,data=csdat)
summary(resm2)

#VIFの算出例
library(car) #パッケージcarの読み込み
vif(resm2) #関数vifによるVIFの算出


#顧客満足度の4変数に関するVIFの算出
vif(res1)

#切片と偏回帰係数の信頼区間の算出
confint(res1,level=0.95)

#標準偏回帰係数の算出
scsdat <- as.data.frame(scale(csdat))#データの標準化とデータフレーム化
res2 <- lm(顧客数‾立地満足度+トレーナー数,data=scsdat)#標準偏回帰係数の推定

#関数lmの出力の一部
summary(res2)

#質的変数も含めた重回帰分析
res3 <- lm(顧客数‾立地満足度+設備満足度+店舗面積満足度+トレーナー満足度+接客研修+入会特典,data=csdat)
summary(res3)　

#AICの算出
extractAIC(res1) #説明変数が量的変数のみの回帰モデル
extractAIC(res3) #説明変数に質的変数も含めた回帰モデル

#BICの算出
extractAIC(res1,k=log(30))#説明変数が量的変数のみの回帰モデル
extractAIC(res3,k=log(30))#説明変数に質的変数も含めた回帰モデル

#t分布
pt(-0.6949209,25)*2  #切片のp値
(1- pt(3.075611,25))*2  #設備満足度の偏回帰係数のp値


#qtの算出
qt(0.025,25) #下側確率0.025を与えるqtの算出
qt(0.975,25) #上側確率0.025を与えるqtの算出


#信頼区間の算出
21.640-2.059539*7.036 #信頼区間の下限
21.640+2.059539*7.036 #信頼区間の上限

#演習と回答
#問1解答
kamokudat <- read.csv("科目内試験結果.csv")
dim(kamokudat)
colnames(kamokudat)

#問2解答
restest <- lm(final‾t1+t2+t3+t4+t5,data=kamokudat)

#問3解答
library(car)
vif(restest)

#問4解答

#t4を削除する場合
restest2 <- lm(final‾t1+t2+t3+t5,data=kamokudat)
#t2を削除する場合
restest2 <- lm(final‾t1+t3+t4+t5,data=kamokudat)
summary(restest2)

#問5解答
confint(restest2)


#問6解答
restest3 <- lm(final‾t1+t2,data=kamokudat)
extractAIC(restest2)
extractAIC(restest3)

#問7解答
round(cor(kamokudat),2)
