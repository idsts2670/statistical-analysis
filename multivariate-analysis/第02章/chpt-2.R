jhk <- read.csv('人事評価結果.csv')
head(jhk)
str(jhk)
#総合平均をデータフレームに追加
jhk$総合平均 <- apply(jhk[,10:11], 1, mean)
str(jhk)
#factorの使い所eg
score <- c(1, 5, 2, 10, 8, 2, 1, 4, 3, 3)
table(score)
#factor型への変換
fscore <- factor(score, levels = seq(0, 10, 1))
str(fscore)
table(fscore) #度数分布表を見ると回答がなかったカテゴリにも度数0で表示されている

#=,≠による条件抽出
#男性のデータのみを抽出する
mdat <- subset(jhk, 性別=='M')
head(mdat)
#違う入力方法だが同じデータを抽出する
mdat2 <- subset(jhk, 性別!='F')
head(mdat2)
#<,≦,>,≧による条件抽出
#協調性が50点未満(<)の行を抽出する
cope1 <- subset(jhk, 協調性<50)
head(cope1)
#50点以下(≦)の行を抽出する
cope2 <- subset(jhk, 協調性<=50)
head(cope2)
#50点より大きい(>)行を抽出する
cope3 <- subset(jhk, 協調性>50)
head(cope3)
#50点以上(≧)の行を抽出する
cope4 <- subset(jhk, 協調性>=50)
head(cope4, 10)
#論理和(I)と論理積(&)の使い方
#男性または熟練(論理和)
m1 <- subset(jhk, (性別=='M')|(年代=='熟練'))
head(m1)
#男性かつ熟練かつ技能が50点以上(論理積)
m2 <- subset(jhk, (性別=='M')&(年代=='熟練')&(技能>=50))
head(m2)
#男性かつ中堅または熟練(論理和と論理積)
m3 <- subset(jhk, (性別=='M')&((年代=='中堅')|(年代=='熟練'))) #"男性"という条件を中堅or熟練全体にかけるために()で囲っている
head(m3)
#subsetを利用しない行の条件抽出
#条件に合致する行にはTRUEを、合致しない行にはFALSEを与える
cond <- (jhk$性別=='M')&((jhk$年代=='中堅')|(jhk$年代=='熟練'))
head(cond)
m4 <- jhk[cond, ] #condでTRUEのみが考慮される
head(m4)

#欠損値の処理
#欠損値が空白の場合
kesson <- read.csv('欠損データ.csv')
kesson
#欠損値が数値の場合
kesson2 <- read.csv('欠損データ2.csv')
kesson2
#na.stringsで欠損値を指定
#データの読み込み
kesson3 <- read.csv('欠損データ2.csv', na.strings = c(999, 9999))
kesson3
kanzen <- na.omit(kesson3)
kanzen
#NAのない行番号を取得する
cind <- complete.cases(kesson3)
cind
#complete.casesの結果を利用して完全データを生成する
kanzen2 <- kesson3[cind,] #cindでTRUEとなっている場合のみ考慮 #上記のkanzenと内容は一致する
kanzen2

#ソート
#関数sortによる1変数のソート
score <- c(1,5,2,10,8,2,1,4,3,3)
sort(score, decreasing = FALSE) #decreasingは省略可。FALSEで昇順にソート
sdat <- read.csv('ソートデータ.csv')
sdat
#関数orderで「協調性」を昇順にソートしたときの位置番号を取得する
posi <- order(sdat$協調性)
posi
#decreasing=TRUEで降順の行番号を取得
posi2 <- order(sdat$協調性, decreasing = TRUE)
posi2
#1変数によるデータフレームのソート
sdat[posi,] #昇順の場合
sdat[posi2,] #降順の場合
#複数変数によるデータフレームのソート
posi3 <- order(sdat$協調性, sdat$総合)
sdat[posi3,]

#マージ
#2つのデータを読み込む
datA <- read.csv('マージデータA.csv')
datB <- read.csv('マージデータB.csv')
datA
datB
merge(datA, datB, by="ID") #byには両データに共通して含まれる変数名を指定
merge(datA, datB, by='ID', all=TRUE) #引数allにTRUEを指定すると、全変数情報をマージする

#数値の置き換え
#最初に置換を行うテストデータを作成
vec <- c(2,3,4,5,1,2,3,1,2) 
tmat <- matrix(vec, ncol=3)#columns(列)は3つに設定
tmat
#関数whichで2の座標, 4の座標を行列形式(arr.ind=TRUE)で取得
loc2 <- which(tmat==2, arr.ind = TRUE)#2がある行列番号を取得
loc4 <- which(tmat==4, arr.ind = TRUE)#4がある行列番号を取得
loc2
#数値の置換
tmatc <- tmat #変換前のデータのコピーを作成する
tmatc[loc4] <- 5 #tmatで4が立っている座標を選択して5を代入する
tmatc[loc2] <- 8 #tmatで2が立っている座標を選択して8を代入する
tmatc

#固定長データのハンドリング
itemresp <- readLines('項目反応固定長.txt') #データの読み込み
itemresp
#変数の位置情報を作成する
spoint <- c(1, seq(7,11,1)) #始点の生成
epoint <- c(6, seq(7,11,1)) #終点の生成
spoint
epoint
#変数の作成
raw0 <- sapply(itemresp, substring, spoint, epoint)
raw0
#行列の整形
dimnames(raw0)[[2]] <-1:5 #転置したあとの行名が煩雑になるから整数値に変える
raw1 <- t(raw0) #raw0の転置行列を作成
colnames(raw1) <- c('ID', paste('問',1:5, sep='')) #pasteでくっつけた変数名を列名として付与
raw1
#正誤データへの変換
key <- read.csv('key.txt')
key[,1]
#正答を1, 誤答を0に変換
binmat <- sweep(raw1[,-1], 2, key[,1], FUN='==')*1 #raw1[,-1]で1列名のIDを除き、FUN='=='で項目照合して、一致していたらTRUEが、そうでなければFALSEと出るからそれに1をかける
binmat

#ID-POSデータの読み込み
fname <- dir() #working directoryをPOSデータに移動して関数dirを実行する
fname
#ID-POSデータの一例
pos0 <- read.csv('201301.csv')
pos0
head(pos0) #POSデータは同じ顧客IDがデータ内で複数回登場するという特徴がある
str(pos0)#変数の型を確認
tmp <- lapply(fname, read.csv) #複数データを一度に読み込む
#関数do.callとrbindを併用して複数のデータフレームを縦につなげる
posall <- do.call(rbind, tmp)
#Factor型への変換
locv <- c('顧客ID', '店舗', '商品カテゴリ')
posall[,locv] <- lapply(posall[,locv], as.factor) #lapplyで3つの変数を同時にfactor型に変換
posall
str(posall)

#ID-POSデータによるソート
tmploc <- order(posall$顧客ID, posall$購買日, posall$購買時間)
pos <- posall[tmploc,]
head(pos)

#RFM分析
R <- tapply(posall$購買日, posall$顧客ID, max) #IDを郡として(IDごとに)最新購買日を
F <- tapply(posall$顧客ID, posall$顧客ID, length) #IDを郡として総購買回数を
M <- tapply(posall$購買金額, posall$顧客ID, sum) #IDを郡として総購買金額を
rfm <- data.frame(R=R, F=F, M=M) #R, F, Mをデータフレームとして統合
tmploc2 <- order(rfm$M, rfm$F, rfm$R, decreasing = TRUE) #優先順位をM>F>Rとして降順にソート
rfm2 <- rfm[tmploc2, ] #ソートしたデータの順番をrfm全体に反映させてrfm2とする
rfm2[1:7,] #上位20%（36人×0.2 = 7.2人）の顧客の表示

#ID-POSデータにおけるクロス集計表
t1 <- table(posall$顧客ID, posall$商品カテゴリ) #商品IDと商品カテゴリのクロス表
t2 <- xtabs(~顧客ID+商品カテゴリ+店舗, data=posall) #店舗別ごとにクロス表
t3 <- xtabs(~顧客ID+商品カテゴリ+購買日, data=posall) #購買日ごとにクロス表
#factor型に変換しているためsubsetなどでデータを分割しても分割データ以外のデータも含めて表示される
#データ全体のクロス集計表の行数と列数を求める
dim(table(posall[,c('顧客ID', '商品カテゴリ')]))　#行列数(次元数)は店舗Aで分割した場合と同じ
#店舗Aにおけるクロス集計表の行数と列数を求める
storeA <- subset(posall, 店舗=='A')
dim(table(storeA[,c('顧客ID', '商品カテゴリ')])) #行列数(次元数)は全体の場合と同じ

#顧客ID別に月ごとの購買金額を求める
cid <- posall$顧客ID #顧客IDのベクトル
buym <- substr(posall$購買日, 1, 6) #購買月のベクトル #1~6と指定することで日にちは排除している
#関数tapplyの郡の引数をリスト形式で与える
resmat <- tapply(posall$購買金額, list(cid, buym), sum) #cid(顧客ID)×buym(購買月)の条件で購買金額の総額を求めている
head(resmat, 5) #この段階では欠陥値はそのまま
resmat[is.na(resmat)] <- 0　#該当データが存在せずNAとなっている部分に0を代入する
head(resmat, 3)

#顧客ID別に購入された商品名を取得する -自作関数を利用する
getitemname <- function(x) #ベクトルxに対して、その要素が1以上の場合にその要素名を返す自作関数getitemnameを定義
{
  return(names(which(x>=1)))
}
#自作関数を顧客IDと商品カテゴリのクロス集計表に適用する
res2 <- apply(t1, 1, getitemname)　#各IDごとに最低1つ購買している要素(ここでは商品)を全て取得している
head(res2, 2) #最初の2要素を抽出

#顧客IDごとに来店間隔の分布を描画・要約する
tmpdate <- paste(substr(posall$購買日, 1, 4), '-', 
                 substr(posall$購買日, 5, 6), '-', substr(posall$購買日, 7, 8), sep='') #as.Dateの引数として使える様に、来店日を文字列に変換
tmpdate[1:7]
ndate <- as.Date(tmpdate) #文字列をdate形式に変換する
restime <- tapply(ndate, posall$顧客ID, diff) #顧客別に来店間隔を求める
head(restime, 2) #2人分の来店感覚を表示
#顧客ID別に来店間隔の分布を描画
restime2 <- lapply(restime, as.numeric) #リストの要素を数値化しておく
par(mfrow=c(2, 3)) #最初の1~6の顧客のヒストグラムを描画
lapply(restime2[1:6], hist, breaks=10, xlab='diff', main='')
#顧客ID別に来店間隔の分布を要約
library(psych)
resd <- lapply(restime2, describe) #restime2の情報に関してdescribe関数で要約
resd[1:5]









































































