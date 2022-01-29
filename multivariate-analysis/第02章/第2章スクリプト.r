#データフレームの構造の表示
jhk <- read.csv("人事評価結果.csv")#データの読み込み
str(jhk)#関数strによるデータ構造の出力

#データフレームに変数を追加する
jhk$総合平均 <- apply(jhk[,10:11],1,mean)#「総合平均」をデータフレームに追加
str(jhk)

#度数分布表
score <-c(1,5,2,10,8,2,1,4,3,3)
table(score)


#factor型への変換と度数分布表
fscore <- factor(score,levels=seq(0,10,1))#関数factorによって変換
str(fscore)#構造の確認
table(fscore)#度数分布表


#=, ≠による条件抽出
mdat <- subset(jhk,性別=="M")#男性のデータのみ抽出する
head(mdat)
mdat2 <- subset(jhk,性別!="F")#こちらも男性のデータのみ抽出する
head(mdat2)


#<, ≦, >,≧による条件抽出
cope1 <- subset(jhk, 協調性<50)#協調性が50点未満(<)の行を抽出する
head(cope1)
cope2 <- subset(jhk, 協調性<=50)#50点以下(≦)の行を抽出する
head(cope2)
cope3 <- subset(jhk, 協調性>50)#50点より大きい(>)行を抽出する
head(cope3)
cope4 <- subset(jhk, 協調性>=50)#50点以上(≧)の行を抽出する
head(cope4)

#論理積と論理和による条件抽出
m1 <- subset(jhk,(性別=="M")|(年代=="熟練"))#男性または熟練(論理和)
head(m1)
m2 <- subset(jhk,(性別=="M")&(年代=="熟練")&(技能>=50))#男性かつ熟練かつ技能が50点以上(論理積のみ)
head(m2)
m3 <- subset(jhk,(性別=="M")&((年代=="中堅")|(年代=="熟練")))#男性かつ中堅または熟練(論理和と論理積)
head(m3)

#subsetを利用しない行の条件抽出
cond <- (jhk$性別=="M")&((jhk$年代=="中堅")|(jhk$年代=="熟練"))#条件に合致する行についてはTRUEを，合致しない行にはFALSEを与える
head(cond)#上に定義したベクトルcondの最初の6要素を表示
m4 <- jhk[cond,]
head(m4)

#欠損データの読み込み
kesson <- read.csv("欠損データ.csv")#欠損箇所が空白の場合
kesson

#欠測データの読み込み2
kesson2 <- read.csv("欠損データ2.csv")#欠損箇所が数値の場合
kesson2

#引数na.stringsによる欠損値の指定
kesson3 <- read.csv("欠損データ2.csv",na.strings=c(999,9999))#na.stringsで欠損値を指定
kesson3

#na.omitによる完全データ行列の生成
kanzen <- na.omit(kesson3)
kanzen

#complete.casesによる行の抽出
cind <- complete.cases(kesson3)#NAのない行番号を取得する
cind

#complete.casesの結果を利用して完全データを生成する
kanzen2 <- kesson3[cind,]#上述のkanzenと内容は一致する
kanzen2

#関数sortによる1変数のソート
score <-c(1,5,2,10,8,2,1,4,3,3)
sort(score,decreasing=FALSE)#decreasingは省略可．TRUEで降順にソート


#「ソートデータ.csv」の内容
sdat <- read.csv("ソートデータ.csv")#データの読み込み
sdat

#関数orderによるソートされた行番号の取得
posi <- order(sdat$協調性)#関数orderで協調性を昇順にソートしたときの位置番号を取得する
posi
posi2 <- order(sdat$協調性,decreasing=TRUE)#decreasing=TRUEで降順の行番号を取得

#1変数によるデータ行列のソート
sdat[posi,]#昇順の場合
sdat[posi2,]#降順の場合

#複数変数によるデータフレームのソート
posi3 <- order(sdat$協調性,sdat$総合)
sdat[posi3,]


#マージするデータ
datA <-read.csv("マージデータA.csv")#行数8
datB <-read.csv("マージデータB.csv")#行数4
datA

#マージ
merge(datA,datB,by="ID")#byには両データに共通して含まれている変数名を指定

#欠損値のある行も残すマージ
merge(datA, datB, by="ID", all=TRUE)

#置換を行うデータ
vec <- c(2,3,4,5,1,2,3,1,2)#テストデータの作成
tmat <- matrix(vec,ncol=3)
tmat


#関数whichによる置換対象要素の座標の取得
loc2 <- which(tmat==2,arr.ind=TRUE)
loc4 <- which(tmat==4,arr.ind=TRUE)
loc2


#数値の置換
tmatc <- tmat #変換前のデータ行列のコピーを作成する
tmatc[loc4] <- 2 #tmatで4の座標を選択し，2を代入
tmatc[loc2] <- 4 #tmatで2の座標を選択し，4を代入
tmatc

#固定長データの読み込み
itemresp <- readLines("項目反応固定長.txt")
itemresp

#数値の位置情報を作成
spoint <- c(1,seq(7,11,1)) #始点の生成
epoint <- c(6,seq(7,11,1)) #終点の生成
spoint
epoint

#変数を作成
raw0 <- sapply(itemresp,substring,spoint,epoint)
raw0

#行列の整形
dimnames(raw0) [[2]]  <- 1:5 #後に行名が煩雑になるので整数値を付与
raw1 <- t(raw0) #行と列を交換
colnames(raw1) <- c("ID",paste("問",1:5,sep="")) #変数名を付与
raw1

#正誤データへの変換
key <- read.csv("key.txt") #正答キーの読み込み
key[,1]
binmat <- sweep(raw1[,-1],2,key[,1],FUN="==")*1#正答を1，誤答を0に変換
binmat

#作業ディレクトリを「POSフォルダ」へ変更
#フォルダ内のファイルの表示
fname <- dir()
fname


#ID-POSデータの一例
pos0 <- read.csv("201301.csv")#データの読み込み
head(pos0,3)
str(pos0)#変数の型

#複数のデータを一度に読み込む
tmp <- lapply(fname,read.csv,stringsAsFactors=FALSE)

#複数のデータフレームを縦につなげる
posall <- do.call(rbind,tmp)#関数do.callとrbindを併用して複数のデータフレームを縦に繋げる

#Factor型への変換
locv <- c("顧客ID","店舗","商品カテゴリ")
posall[,locv] <- lapply(posall[,locv],as.factor)

#「顧客ID」によるソート
tmploc <- order(posall$顧客ID,posall$購買日,posall$購買時間)
pos <- posall[tmploc,]
head(pos)

#RFM分析
R <- tapply(posall$購買日,posall$顧客ID,max)#顧客ID毎に最新購買日を求める
F <- tapply(posall$顧客ID,posall$顧客ID,length)#顧客ID毎に総購買回数を求める
M <- tapply(posall$購買金額,posall$顧客ID,sum)#顧客ID事に総購買金額を求める
rfm <- data.frame(R=R,F=F,M=M)#R,F,Mをデータフレームとして統合
tmploc2 <- order(rfm$M,rfm$F,rfm$R,decreasing=TRUE)#優先順位をM > F > Rとしてソート
rfm2 <- rfm[tmploc2,]#ソートしたデータを保存
rfm2[1:7,]#上位20パーセント以内(36名*0.2=7.2名)の顧客の表示


#さまざまなクロス集計表の算出
t1 <- table(posall$顧客ID,posall$商品カテゴリ)#全体
t2 <- xtabs(‾顧客ID+商品カテゴリ+店舗,data=posall)#店舗別(3店舗)
t3 <- xtabs(‾顧客ID+商品カテゴリ+購買日,data=posall)#購買日別(336日)

#Factor型変数によるクロス集計表
dim(table(posall[,c("顧客ID","商品カテゴリ")]))#データ全体のクロス集計表の行数と列数を求める
storeA <- subset(posall,店舗=="A")#店舗Aにおけるクロス集計表の行数と列数を求める
dim(table(storeA[,c("顧客ID","商品カテゴリ")]))

#顧客ID別に月ごとの総購買金額を求める
cid <- posall$顧客ID #顧客IDのベクトル
buym <- substr(posall$購買日,1,6)#購買月のベクトル
resmat <- tapply(posall$購買金額,list(cid,buym),sum) #関数tapplyの群の引数をリスト形式で与える。ここでは顧客IDと購買月を指定している

resmat[is.na(resmat)] <- 0 #該当データが存在せず，NA値がアサインされている部分に0を代入する
head(resmat,3) 

#関数getitemnameの読みこみ
#ベクトルxに対して，その要素が1以上の場合に，その要素名を返す自作関数getitemnameを定義
getitemname <- function(x)
{
	return(names(which(x>=1)))	
}

res2 <- apply(t1,1,getitemname)#自作関数を顧客IDと商品カテゴリのクロス集計表に適用する
head(res2,2)#最初の2要素を抽出


#来店日をdate形式に変換
tmpdate <- paste(substr(posall$購買日,1,4),"-",substr(posall$購買日,5,6),"-",
substr(posall$購買日,7,8),sep="")#関数as.Dateの引数として使えるように来店日を文字列に変換
tmpdate[1:5]
ndate <- as.Date(tmpdate)#文字列をdate形式に変換する
restime <- tapply(ndate,posall$顧客ID,diff)#顧客別に来店間隔を求める
head(restime,2)#2名分の来店間隔を表示

#顧客ID別に来店間隔の分布を描画する
restime2 <- lapply(restime,as.numeric) #リストの要素を数値化しておく
par(mfrow=c(2,3)) 
lapply(restime2[1:6],hist,breaks=10,xlab="diff",main="",col="grey")#最初の1〜6の顧客のヒストグラムを描画

#顧客ID別に来店間隔の分布を要約する
library(psych) #パッケージpsychの読み込み
resd <- lapply(restime2,describe) #パッケージpsychのdescribe関数で要約
resd[1:2]


#演習と回答
#問1解答(作業ディレクトリを「POSフォルダ2」へ変更)
fname <- dir()
fname2 <- paste(fname,"/2013",sprintf("%02d",1:12),".csv",sep="")
tmpall <- lapply(fname2,read.csv,stringsAsFactors=FALSE)
posall2 <- do.call(rbind,tmpall)
locv <- c("顧客ID","店舗","商品カテゴリ")
posall2[,locv] <- lapply(posall2[,locv],as.factor)

#問2解答
loc2 <- (substr(posall2$購買日,1,6)=="201302")
loc5 <- (substr(posall2$購買日,1,6)=="201305")
pos02 <- posall2[loc2,]
pos05 <- posall2[loc5,]

#問3解答
store02 <- tapply(pos02$購買金額,list(pos02$顧客ID,pos02$店舗),sum)
store05 <- tapply(pos05$購買金額,list(pos05$顧客ID,pos05$店舗),sum)
store02[is.na(store02)] <-0
store05[is.na(store05)] <-0

#問4解答
dat02 <- data.frame(rownames(store02),store02)#顧客IDを変数として含むデータフレームを作成
dat05 <- data.frame(rownames(store05),store05)
colnames(dat02) <- c("顧客ID", "2月店舗A", "2月店舗B","2月店舗C")#変数名を変更
colnames(dat05) <- c("顧客ID", "5月店舗A", "5月店舗B","5月店舗C")
mdat <- merge(dat02,dat05,by="顧客ID")#顧客IDによって二つのデータセットをマージ

#問5解答
get200 <- function(x)#200円以上あった月と店舗の情報を抽出する関数の定義
{		
	loc <- which(x>=200)
	return(names(x[loc]))
}

apply(mdat[,2:7],1,get200)#定義した自作関数を利用して，顧客ID別に情報を抽出


#問6解答
ptime <- factor(round(posall2$購買時間),level=seq(9,21,1))
table(posall2$商品カテゴリ,ptime)

#問7解答
tmpdate2 <- paste(substr(posall2$購買日,1,4),"-",substr(posall2$購買日,5,6),"-",
substr(posall2$購買日,7,8),sep="") #年月日の情報をyyyy-mm-dd形式に変換
ndate2 <- as.Date(tmpdate2) #文字列をdate形式にフォーマットする
restime3 <- tapply(ndate2,posall2$顧客ID,diff) #顧客別に来店間隔の差をもとめる
restime4 <- lapply(restime3,as.numeric) #リストの要素を数値化しておく
x50 <- function(x)#ベクトルの要素が50以上の場合TRUEを，さもくばFALSEを返す関数x50を定義
{
	res <- sum(x>=50)
	return(ifelse(res>=1,TRUE,FALSE))
}
sid <- sapply(restime4,x50)
sid[sid==TRUE]

#問8解答(作業ディレクトリを「第2章へ変更」)
fmat <- readLines("項目反応固定長2.txt") #データの読み込み
sp <- c(1,7:106) #始点の桁
ep <- c(6,7:106) #終点の桁
fmat2 <- sapply(fmat,substring,sp,ep)
dimnames(fmat2)[[2]] <- paste("ID",sprintf("%04d",1:1000),sep="") #行名を変換
fmat3 <- t(fmat2) #行列を転置
key2 <- read.csv("key2.txt")#正答キーの読み込み
sweep(fmat3[,-1],2,key2[,1],FUN="==")*1 #正誤反応データの生成