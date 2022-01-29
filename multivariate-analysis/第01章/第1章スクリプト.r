#データの読み込み
jhk <- read.csv("人事評価結果.csv") #データの読み込み

#データの確認
dim(jhk) #手順1. dimによる多変量データの行数列数の確認
colnames(jhk) #手順2. colnamesによる変数名の確認
head(jhk,3) #手順3. headによる最初の3行の表示

#ヒストグラムの描画
library(lattice)#パッケージlatticeの読み込み
histogram(‾ストレス,data=jhk,breaks=20,type="count")

#代表値の算出
mean(jhk$ストレス) #ストレスの平均値
median(jhk$ストレス) #ストレスの中央値　
sort(table(jhk$年代)) #年代の最頻値

#散布度の算出
sd(jhk$ストレス) #ストレスのSD
var(jhk$ストレス) #ストレスの分散

#中央値からの平均偏差の算出
mean(abs(jhk$ストレス-median(jhk$ストレス)))

#群別にヒストグラムを描画
histogram(‾協調性|年代+性別,data=jhk,breaks=15)

#群間での分布比較
tapply(jhk$協調性,jhk$性別,mean) #性別毎に「協調性」の平均値を求める
tapply(jhk$協調性,jhk$性別,sd) #性別毎に「協調性」のSDを求める


#箱ヒゲ図の描画
boxplot(jhk$技能,horizontal=TRUE) #「技能」の箱ヒゲ図を横置きに作成する(horizontal=FALSEで縦置き)
boxplot(協調性‾性別,data=jhk,horizontal=TRUE) #性別毎に「協調性」の箱ヒゲ図を描画する


#四分位数も含めた要約統計量の算出
summary(jhk$技能)


#等分散のF検定
var.test(協調性‾性別,data=jhk)

#独立な2群のt検定（等分散を仮定）
t.test(協調性‾性別,data=jhk,var.equal=TRUE)

#Welch法によるt検定
t.test(協調性‾性別,data=jhk)

#対応のあるt検定
score <- c(jhk$総合,jhk$昨年総合)
year <- c(rep("今年",800),rep("昨年",800))
t.test(score‾year,paired=TRUE)

#群ごとに信頼区間を描画
library(gplots)#パッケージgplotsの読み込み
plotmeans(協調性‾性別,data=jhk,p=0.95,ylim=c(49,54))


#信頼区間の算出
t.test(jhk$協調性[jhk$性別=="F"])
t.test(jhk$協調性[jhk$性別=="M"])


#列(変数に対する基礎集計)
varname <- c("協調性","自己主張","技能","知識")
jhk2 <- jhk[,varname] #データフレームから4変数を抽出する
apply(jhk2,2,mean) #変数別に平均値を求める
apply(jhk2,2,sd) #変数別にSDを求める

#行(観測対象に対する基礎集計)
apply(jhk2,1,sum) #社員毎に4変数の合計点を求める
apply(jhk2,1,sd) #社員毎に4変数のSDを求める

#多変数の分布を群間で比較
by(jhk2,jhk$性別,apply,2,mean) #4変数の平均の算出
by(jhk2,jhk$性別,apply,2,sd) #4変数のSDの算出

#標準化の手続き
zscore <- scale(jhk2) #z得点の算出
head(zscore,2)
tscore <- zscore*10 + 50 #偏差値の算出
head(tscore,2)

#散布図の描画
gino <- jhk$技能
chisiki <- jhk$知識
plot(gino,chisiki,xlab="技能",ylab="知識")

#散布図行列の描画
kjs <- c("協調性","自己主張","ストレス")
plot(jhk[,kjs])

#層別散布図の描画
library(lattice)　#パッケージlatticeの読み込み
xyplot(知識‾技能|年代+部署,data=jhk)


#相関係数の算出
cor(jhk$協調性,jhk$ストレス) #2変数間の相関係数を求める

#相関行列の算出
cor(jhk[,kjs])　#corに多変数を投入し相関行列を求める

#共分散行列の算出
cov(jhk[,kjs])

#相関係数の検定
library(psych)　#パッケージpsychのインストール
corkekka <- corr.test(jhk[,kjs])　
corkekka$t #t値の算出
corkekka$p #p値の算出


#クロス集計表の作成
(cross <- table(jhk$部署,jhk$年代))


#クロス集計表を割合表記
prop.table(cross)　#全度数を基準にした割合表記
prop.table(cross,1)　#行方向の割合表記
prop.table(cross,2)　#列方向の割合表記


#層別クロス集計表の作成
xtabs(‾部署+年代+性別,data=jhk)


#連関係数の算出
library(vcd)　#パッケージvcdの読み込み
assocstats(cross)　#部署と年代の連関係数の算出

#クロス集計表と連関係数の関係
(m1 <- matrix(c(50,0,0,50),ncol=2))　#完全連関のケース
assocstats(m1)　#完全連関のクロス集計表における連関係数
(m2 <- matrix(c(10,20,100,200),ncol=2))　#独立のケース
assocstats(m2)　#独立したクロス集計表における連関係数


#クロス集計表に対するカイ二乗検定
(reschisq <- chisq.test(cross))

#残差分析
reschisq$stdres


#偏相関係数の算出
sixname <- c("協調性","自己主張","技能","知識","総合","昨年総合")
jhk3 <- jhk[,sixname]
cor(jhk3[,5],jhk3[,6]) #総合，昨年総合の相関係数を求める
partial.r(jhk3,c(5,6),c(1,2,3,4)) #協調性，自己主張，技能，知識を統制した偏相関係数を求める


#順序カテゴリカル変数を含めたデータフレームの作成
(sogoc <- c(-Inf,mean(jhk$総合),Inf))　#2値カテゴリカル変数化するための階級幅を作成する
(scat <- cut(jhk$総合,breaks=sogoc,right=FALSE,labels=c(0,1))) #階級幅を利用してデータを0と1に変換する
(ginoc <- c(-Inf,summary(jhk$技能)[c(2,5)],Inf))　#多値カテゴリカル変数化するための階級幅を作成する
(gcat <- cut(jhk$技能,breaks=ginoc,right=FALSE,labels=c(0,1,2)))　#階級幅を利用してデータを0と1と2に変換する

#順序カテゴリカル変数を含めた相関行列の算出
library(polycor)　#パッケージpolycorの読み込み
jhk4 <- data.frame(総合カテ=scat,技能カテ=gcat,知識=jhk$知識)　#量的変数「昨年総合」も含めてデータフレームを作成する
hetcor(jhk4,ML=TRUE) #最尤法で相関行列の算出


#関数effectd1の読み込み
effectd1 <- function(x1,x2,clevel=0.95)
{
    library(MBESS)
    #各群の標本サイズの算出
    n1 <- length(x1);n2 <- length(x2)
    #各群の平均の算出
    m1 <- mean(x1);m2 <- mean(x2)
    #各群の標本標準偏差の算出
    s1 <- sqrt(mean((x1-m1)^2))
    s2 <- sqrt(mean((x2-m2)^2))
    #母標準偏差の推定値の算出
    sast <- sqrt(((n1*s1^2)+(n2*s2^2))/(n1+n2-2))
    #効果量の算出
    d <- (m1-m2)/sast
    #独立した2群のｔ検定の実行(等分散仮定)と自由度の算出
    rest <- t.test(x1,x2,paired=FALSE,var.equal=TRUE)
    #効果量の信頼区間の算出
    resconf <- conf.limits.nct(t.value=rest$statistic,
    df=rest$parameter,conf.level=clevel)
    ll <- resconf$Lower.Limit*sqrt((n1+n2)/(n1*n2))
    ul <- resconf$Upper.Limit*sqrt((n1+n2)/(n1*n2))
    u3 <- pnorm(d,0,1)
    return(list=c(効果量=d,信頼水準=clevel,区間下限=ll,
    区間上限=ul,U3=u3))
}

#独立な２群のt検定に対応した効果量の算出
#事前に関数effectd1をRに読み込んでおく
fdat <- jhk$協調性[jhk$性別=="F"]
mdat <- jhk$協調性[jhk$性別=="M"]
effectd1(fdat, mdat, clevel=0.95)

#関数effectd2の読みこみ
#本関数は欠測値には対応していません。
effectd2 <- function(x1,x2,clevel=0.95)
{
	library(MBESS)
	#標本サイズの算出
	n <- length(x1-x2)
    #差異の平均v.barの算出
    v.bar <- mean(x1-x2)
    #差異の不偏分散の平方根svの算出
    sv.p <- sd(x1-x2)
    #効果量の算出
    d.p <- v.bar/sv.p
    #対応のあるｔ検定の実行と自由度の算出
    rest <- t.test(x1,x2,paired=TRUE)
    #効果量の信頼区間の算出
    resconf <- conf.limits.nct(t.value=rest$statistic,
    df=rest$parameter,conf.level=clevel)
    ll <- resconf$Lower.Limit/sqrt(n)
    ul <- resconf$Upper.Limit/sqrt(n)
    u3 <- pnorm(d.p,0,1)
    return(list=c(効果量=d.p,信頼水準=clevel,区間下限=ll,
    区間上限=ul,U3=u3))
}

#対応のあるt検定における効果量の算出
effectd2(jhk$総合,jhk$昨年総合,clevel=0.95)


#相関係数の信頼区間の算出
corkekka2 <- corr.test(jhk[,kjs],alpha=0.05) 
print(corkekka2,short=FALSE)


effectv <- function(x,y,clevel=0.95)
{
    library(vcd)
    library(MBESS)
    #クロス集計表の算出
    tmpcross <- table(x,y)
    #標本サイズの算出
    n <- sum(tmpcross)
    #集計表の行数と列数を算出
    size <- dim(tmpcross)
    #自由度を算出
    dof <- prod(size-1)	
    #カイ二乗値とクラメールVの算出
    resas <- assocstats(tmpcross)
    chi <- resas$chisq_tests["Pearson","X^2"]	
    v <- resas$cramer
    #カイ二乗値を所与としたときの非心度の上限値，下限値を算出
    resconf <- conf.limits.nc.chisq(Chi.Square=chi,
    df=dof,conf.level=clevel)
    
    if(resconf$Lower.Limit>0)#下限値λLがを超える領域に入った場合
    {
        #信頼区間の下限・上限の算出  
        ll <- sqrt((dof+resconf$Lower.Limit)/((min(size)-1)*n))
        ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
        return(list=c(効果量V=v,カイ二乗値=chi,信頼水準=clevel,
        区間下限=ll,区間上限=ul))
    }else if(resconf$Lower.Limit==0) #下限値λlが負値になった場合
    {
        #信頼区間の下限を0に制約した上で上限を算出       
        resconf <- conf.limits.nc.chisq(Chi.Square=chi,
        df=dof,conf.level=NULL,alpha.lower=0,alpha.upper=(1-clevel)/2)
        ul <- sqrt((dof+resconf$Upper.Limit)/((min(size)-1)*n))
        return(list=list(
        "下限値λLが負値になったので信頼区間の下限値を0にしました。",
        c(効果量V=v,カイ二乗値=chi,信頼水準=clevel,区間下限=0,
        区間上限=ul)))
    }
}

#クラメールVに対する信頼区間の算出
effectv(jhk$年代,jhk$部署,clevel=.95)


#演習と解答
#問1解答
mat <- read.csv("学力調査結果.csv")
library(lattice)
histogram(‾プレ得点|部活,data=mat)
boxplot(プレ得点‾部活,data=mat,horizontal=TRUE)

#問2解答
tapply(mat$プレ得点,mat$部活,mean)
tapply(mat$プレ得点,mat$部活,median)
tapply(mat$プレ得点,mat$部活,sd)

#問3解答
m <- mat$数学[mat$性別=="M"]
f <- mat$数学[mat$性別=="F"]
t.test(m,f,var.equal=TRUE)
effectd1(m,f,clevel=0.95)

#問4解答
goukei <- apply(mat[,c("プレ得点","ポスト得点")],1,sum)

#問5解答
spre1 <- scale(mat$プレ得点)
plot(mat$プレ得点,spre1)
cor(mat$プレ得点,spre1)

#問6解答
library(psych)
matc <- mat[,c("プレ得点","ポスト得点","国語","社会","英語")]
partial.r(matc,c(3:5),c(1:2))

#問7解答
library(polycor)
kcat <- cut(mat$国語,breaks=c(-Inf,mean(mat$国語),Inf),right=FALSE,labels=c(0,1))
scat <- cut(mat$社会,breaks=c(-Inf,mean(mat$社会),Inf),right=FALSE,labels=c(0,1))
ecat <- cut(mat$英語,breaks=c(-Inf,mean(mat$英語),Inf),right=FALSE,labels=c(0,1))
mat2 <- data.frame(kcat,scat,ecat)
hetcor(mat2,ML=TRUE)

#問8解答
effectv(mat$性別,mat$部活,clevel=0.95)
