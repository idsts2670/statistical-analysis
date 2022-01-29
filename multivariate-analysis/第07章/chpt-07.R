#データの読み込み、データフレームの確認
dkk <- read.csv('動機づけ.csv') #データの読み込み
head(dkk)

#因子数の決定
#ガットマン基準
cor.dkk <- cor(dkk) #相関行列の算出
eigen(cor.dkk) #固有値の算出
$values

#スクリーテスト
#スクリープロットの出力
library(psych) #パッケージの読み込み
VSS.scree(dkk) #スクリープロットの出力
#固有値の推移がなだらかになる直前までの固有値の数を因子数とする。
#平行分析
fa.parallel(dkk, fm='ml', fa='pc', n.iter=100)

#因子負荷の推定
#因子負荷の推定（初期解）
fa.dkk1 <- fa(dkk, nfactors = 2, fm = 'ml', rotate = 'none') #母数の推定
print(fa.dkk1, sort=TRUE, digits = 3) #結果の出力
#相関行列をもとにした因子負荷の推定(初期解)
fd.dkk.cor <- fa(cor.dkk, nfactors=2, fm='ml', rotate='none', n.obs=500)

#因子軸の回転→推定
#因子負荷の推定（プロマックス回転）
install.packages(c("psych","GPArotation"),dependencies=TRUE)
library(GPArotation) #パッケージの読み込み
fa.dkk2 <- fa(dkk, nfactors = 2, fm='ml', rotate='promax') #母数の推定
print(fa.dkk2, sort=TRUE, digits=3) #結果の出力 #'With factor correlations of'が表示されるようになった

#信頼性の評価（基本はα係数だが、近年ω係数が報告されることも多い）
#α係数の算出
dkk.nht <- dkk[, c('I1', 'I2', 'I3', 'I4')]
alpha(dkk.nht) #内発的動機づけ尺度のα係数
dkk.ght <- dkk[, c('E1', 'E2', 'E3', 'E4')]
alpha(dkk.ght) #外発的動機づけ尺度のα係数
#ω係数
omega(dkk.nht, nfactors=1) #内発的動機づけ尺度のω係数


#順序カテゴリカル変数の探索的因子分析と信頼性の評価
#データの読み込み、データフレームの確認
math <- read.csv('数学テスト.csv') #データの読み込み
head(math)
#因子数の決定
cor.math <- polychoric(math)$rho #ポリコリック相関行列の算出
eigen(cor.math) #固有値の算出（ガットマン基準）
VSS.scree(cor.math) #スクリープロットの出力
fa.parallel(cor.math, fm='ml',fa= 'pc', n.iter=100, n.obs=300) #平行分析
#因子負荷の推定
fa.math <- fa.poly(math, nfactors = 1, fm='ml')#母数の推定
print(fa.math, sort=TRUE, digits=3) #結果の出力
#信頼性の評価
alpha(cor.math, n.obs=300) #α係数の算出
omega(cor.math, nfactors=1, n.obs=300) #ω係数の算出


#章末演習
#データの読み込み
skk <- read.csv('性格.csv')
head(skk)
#問1
#因子数の決定（ガットマン基準）
cor.skk <- cor(skk) #相関行列の算出
eigen(cor.skk)$values #固有値の算出→2因子解が示唆される
#問2
#因子数の決定（スクリープロット）
library(psych)
VSS.scree(skk) #2因子解が示唆される
#問3
#因子数の決定（平行分析）
fa.parallel(skk, fm='ml', fa='pc', n.iter=500) #2因子解が示唆される
#問4
#因子分析（最尤法）
install.packages(c("psych","GPArotation"),dependencies=TRUE) #GPArotationパッケージのインストール
library(GPArotation) #パッケージの読み込み
fa.skk <- fa(skk, nfactors = 2, fm='ml', rotate='promax') #母数の推定
print(fa.skk, sort=TRUE, digits=3) #結果の出力
#問5
#信頼性の評価（α係数）
skk.act <- skk[, c('陽気', '積極的', '外向的', '社交的')]
alpha(skk.act)
skk.pas <- skk[, c('協力的', '温和', '素直', '親切')]
alpha(skk.pas)

#信頼性の評価（ω係数）
omega(skk.act, nfactors=1) #0.76
omega(skk.pas, nfactors=1) #0.79
