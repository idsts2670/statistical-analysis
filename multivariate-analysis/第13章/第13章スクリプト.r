#「メーカー」「ブランド力」「技術力」の3重クロス集計表
b3dat <- read.csv("自転車データ3.csv",row.names=1)#データの読み込み
head(b3dat)

xtabs(‾メーカー+ブランド力+技術力,data=b3dat)

#「自転車データ2.csv」の読み込み
b2dat  <- read.csv("自転車データ2.csv",row.names=1)#データの読み込み
b2dat

#るコレスポンデンス分析の実行
library(FactoMineR) #パッケージの読み込み
resb2dat <- CA(b2dat) #コレスポンデンス分析の実行

#固有値の出力
resb2dat$eig

#行スコアと列スコアの出力
resb2dat$row$coord #行スコアの表示
resb2dat$col$coord #列スコアの表示
              
#関数summaryの出力(一部抜粋)
summary(resb2dat)

#「メーカー」のクラスタ分析
z <- scale(b2dat) # 列方向にz得点化

#平方ユークリッド距離
D0 <- dist(z, method = "euclidean")
D <- (1/2)*D0^2

#階層的クラスター分析
resclust <- hclust(D,method="ward.D")
plot(resclust) #デンドログラムの描画

#クラスタの解釈
clus <- factor(cutree(resclust,k=2))　#クラスター番号の取得
clus
b2dat$cluster <- clus
by(b2dat[,-6],b2dat$cluster,apply,2,mean) #クラスタ別の平均値の算出


#関数dummie.data.frameによるデータ行列の変換
library(dummies) #パッケージの読み込み
db3dat <- dummy.data.frame(b3dat,sep=":")　#関数dummie.data.frameの実行
head(db3dat) #最初の6行の一部を表示

#関数CAによる(多重)コレスポンデンス分析の実行
resdb3dat <- CA(db3dat)
resdb3dat$eig #固有値の出力の一部

#列カテゴリのみをプロット
plot(resdb3dat, invisible="row")
#plot(resdb3dat, invisible="col") #行カテゴリ(評価者)のみをプロット

#関数MCAによる多重コレスポンデンス分析の実行1
resb3dat <- MCA(b3dat)

#多重クロス集計表の変換
crosb3dat <- xtabs(‾メーカー+ブランド力+コスパ+技術力+レース実績+デザイン,data=b3dat)#多重クロス集計表の作成
crosdf<- as.data.frame(crosb3dat)#データフレームへの変換
head(crosdf) 
nrow(crosdf) #行数の確認

#度数が0のセルを除外
crosdf2 <- crosdf[(which(crosdf[,7]>=1)),]　#度数が1以上の行だけ選択
nrow(crosdf2)

head(crosdf2)

#関数MCAによる多重コレスポンデンス分析の実行2
rescrosdf2<-MCA(crosdf2,quanti.sup=7,row.w=crosdf2$Freq)

#表13.1に対するコレスポンデンス分析の出力
b2dat2 <- b2dat[,1:3]
resb2dat2 <- CA(b2dat2) #コレスポンデンス分析の実行
summary(resb2dat2) 
round(dist(rbind(resb2dat2$row$coord,重心=c(0,0))),3) #メーカー間の距離

#演習と解答
#問1解答
exdat <- read.csv("自転車データ練習2.csv") 

#問2解答
library(dummies)
dexdat <- dummy.data.frame(exdat,sep=":")

#問3解答
library(FactoMineR)
rdexdat <- CA(dexdat)
summary(rdexdat)

#問4解答
rexdat <- MCA(exdat)
summary(rexdat)

#問5解答
dfexdat <- as.data.frame(xtabs(‾.,data=exdat))
dfexdat2 <- dfexdat[which(dfexdat$Freq>=1),]

#問6解答
rdfexdat2 <- MCA(dfexdat2,quanti.sup=7,row.w=dfexdat2$Freq)
summary(rdfexdat2)
