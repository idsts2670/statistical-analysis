#データの読み込み
dkk <- read.csv("動機づけ.csv")

#データフレームの確認
head(dkk)

#固有値の算出
cor.dkk <- cor(dkk) #相関行列の算出
eigen(cor.dkk) #固有値の算出

#スクリープロットの出力
library(psych) #パッケージpsychの読み込み
VSS.scree(dkk) #スクリープロットの出力

#平行分析
fa.parallel(dkk, fm="ml", fa="pc", n.iter=100)

#探索的因子分析（初期解）
fa.dkk1 <- fa(dkk, nfactors=2, fm="ml", rotate="none") #母数の推定
print(fa.dkk1, sort=TRUE, digits=3) #結果の出力

#探索的因子分析（初期解）：相関行列を指定した分析
fa.dkk.cor <- fa(cor.dkk, nfactors=2, fm="ml", rotate="none", n.obs=500) #母数の推定
print(fa.dkk.cor, sort=TRUE, digits=3) #結果の出力

#探索的因子分析（プロマックス回転）
library(GPArotation) #パッケージGPArotationの読み込み
fa.dkk2 <- fa(dkk, nfactors=2, fm="ml", rotate="promax") #母数の推定
print(fa.dkk2, sort=TRUE, digits=3) #結果の出力

#α係数の算出
dkk.nht <- dkk[, c("I1","I2","I3","I4")]
alpha(dkk.nht) #内発的動機づけ尺度のα係数

dkk.ght <- dkk[, c("E1","E2","E3","E4")]
alpha(dkk.ght) #外発的動機づけ尺度のα係数

#ω係数の算出
omega(dkk.nht, nfactors=1) #内発的動機づけ尺度のω係数

omega(dkk.ght, nfactors=1) #外発的動機づけ尺度のω係数


#データの読み込み
math <- read.csv("数学テスト.csv")

#データフレームの確認
head(math)

#因子数の決定
cor.math <- polychoric(math)$rho #ポリコリック相関行列の算出
eigen(cor.math) #固有値の算出
VSS.scree(cor.math) #スクリープロットの出力
fa.parallel(cor.math, fm="ml", fa="pc", n.iter=100, n.obs=300) #平行分析

#探索的因子分析（1因子解）
fa.math <- fa.poly(math, nfactors=1, fm="ml") #母数の推定
print(fa.math, sort=TRUE, digits=3) #結果の出力

#信頼性係数の算出
alpha(cor.math, n.obs=300) #アルファ係数の算出
omega(cor.math, nfactors=1, n.obs=300) #オメガ係数の算出



#演習と解答
#問1解答
skk <- read.csv("性格.csv")
cor.skk <- cor(skk)
eigen(cor.skk)

#問2解答
library(psych)
VSS.scree(skk)

#問3解答
fa.parallel(skk, fm="ml", fa="pc", n.iter=100)

#問4解答
library(GPArotation)
fa.skk <- fa(skk, nfactors=2, fm="ml", rotate="promax")
print(fa.skk, sort=TRUE, digits=3)

#問5解答
skk2 <- skk[,c("陽気", "積極的", "外向的", "社交的")]
alpha(skk2)

skk3 <- skk[,c("協力的", "温和", "素直", "親切")]
alpha(skk3)

#問6解答
omega(skk2, nfactors=1)
omega(skk3, nfactors=1)


