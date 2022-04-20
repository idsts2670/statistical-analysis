#############################################################
################### Day1：イントロ/記述統計 #################
#############################################################

### 必要な外部データ ###
# - wine.csv
# - MediaEffect.csv
# - unicef98.dat
# - DescriptiveStats.csv
# これらのデータを作業ディレクトの下にdataフォルダを作成し、格納しておく

###################### Prerequiste ###################### 
# データパスの取得
root_dir <- getwd()
csvPATH1 <- "./data/wine.csv"
csvPATH2 <- "./data/MediaEffect.csv"
datPATH3 <- "./data/unicef98.dat"


######### Day1-Exercise1 データの読み込み、データの種類 #########
#### データの読み込み

## main data: data1
wine_dat <- read.table(file=csvPATH1, header=T, sep=",")  #ヘッダー(T:あり F:なし) デリミタ：","
head(wine_dat) # 頭5行

## data2
media_dat <- read.table(file=csvPATH2, header=T, sep=",")  "","" はカンマ区切りのデータであることを示している
head(media_dat)

## data3 
unicef <- read.table(file=datPATH3, header=T, sep=",")  
head(unicef) # バグる spaceで分かれているデータ
unicef <- read.table(file=datPATH3, header=T)  # spaceのときはそのまま
head(unicef) # 正しい読み込み

### データ構造の確認
str(wine_dat)

### Rについてるsample data
data(iris)# 呼び出し
str(iris)

data(Titanic)
Titanic
class(Titanic)  # tableとして格納されているサンプルデータ
str(Titanic)
data.frame(Titanic)  # data.frameで確認
str(data.frame(Titanic))

data(UKgas)
UKgas
class(UKgas)  # ts(time series)として格納されているサンプルデータ
str(UKgas)
data.frame(UKgas)


### データの要約統計量
summary(wine_dat)

### データの形を確認する
nrow(wine_dat)  # 行数の取得
ncol(wine_dat) # 列数の取得

### カラム名の構造を確認してみる。
colnames(wine_dat)  #カラム名リストをベクトルで抽出

### Indexingによるデータのサブセットを切り出す方法
# []で、[行,列]を指定
wine_dat[c(1,10,100),]  # 複数行の抽出
wine_dat[3:13,]
wine_dat[,1]  # 1列目の抽出

wine_dat["Year"]  # カラム名による参照。data.frameとして抽出
wine_dat$Year  # ベクトル抽出

### データ型の変更
wine_dat["quality"] <- as.factor(wine_dat$quality)   # integer to factor

summary(as.integer(wine_dat$quality)) # integer（量的変数）として
summary(as.factor(wine_dat$quality)) # factor（質的変数）として
# 型により計算される統計量が異なる



######### Day1-Exercise2 記述統計 #########
#### 各種統計量の算出
#### 統計量による外れ値の確認

csvPATH4 <- "./data/DescriptiveStats.csv"
dstat_dat <- read.table(file=csvPATH4, header=T, sep=",")

str(dstat_dat)  # num型とfactor型


### 量的変数に対する記述統計

# とりあえずヒストグラムを描いておく
par(mfrow=c(3,1)) 
hist(dstat_dat$Var1) 
hist(dstat_dat$Var2) 
hist(dstat_dat$Var3) 
par(mfrow=c(1,1))  # 初期化

## 平均値、標準偏差
Mean_Var1 <- mean(dstat_dat$Var1)
Mean_Var2 <- mean(dstat_dat$Var2)
Mean_Var3 <- mean(dstat_dat$Var3)
data.frame(Mean_Var1, Mean_Var2, Mean_Var3)

SD_Var1 <- sd(dstat_dat$Var1)
SD_Var2 <- sd(dstat_dat$Var2)
SD_Var3 <- sd(dstat_dat$Var3)
data.frame(SD_Var1, SD_Var2, SD_Var3)

# 手計算する場合
sum(dstat_dat$Var1) / length(dstat_dat$Var1)  # mean
sqrt( sum((dstat_dat$Var1-mean(dstat_dat$Var1))^2) / (length(dstat_dat$Var1)-1) )  # sd

## 分位点、中央値
(10:0)/10 # 文法確認
Q_Var1 <- quantile(dstat_dat$Var1, probs=(10:0)/10)
Q_Var2 <- quantile(dstat_dat$Var2, probs=(10:0)/10)
Q_Var3 <- quantile(dstat_dat$Var3, probs=(10:0)/10)
data.frame(Q_Var1, Q_Var2, Q_Var3)

# data.frameにまとめる
Qtl <- data.frame(Q_Var1, Q_Var2, Q_Var3)
rownames(Qtl)[1] <- "Max"
rownames(Qtl)[6] <- "Median"
rownames(Qtl)[11] <- "Min"
Qtl

# 中央値の関数を利用する場合
median(dstat_dat$Var1)

# 計算した統計量が、分布の形状により見るべきポイントが異なることを理解する


### 質的変数に対する記述統計

## 度数集計（頻度集計）
freq_Var4 <- table(dstat_dat$Var4)
freq_Var4

## 相対度数
prop_Var4 <- prop.table(freq_Var4)
prop_Var4

# data.frameにまとめる
freq_table <- data.frame(freq_Var4, prop_Var4)
freq_table <- freq_table[-3]
names(freq_table) <- c("Level","Freq","Prop")
freq_table

# 大きい順に並び替え
freq_table[order(freq_table$Freq, decreasing=TRUE),]

## 棒グラフを描く場合
barplot(freq_Var4)



################# Day1-Exercise3 データの可視化 #################
#※ RStudioでは、デフォルトでは右下にあるプロット欄が狭くなりすぎていると、画像が表示されないので注意。

###プロットに表示させるには長すぎるので、一旦ここでは一部の変数に注目する。
wine_small <- wine_dat[1:300, c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", "colour", "quality")]
str(wine_small)

### 散布図
pairs(wine_small[, -5], main="ScatterPlot Matrix of Wine data") 
# col引数で色分け
pairs(wine_small[, -5],col=wine_small$colour, main="ScatterPlot Matrix of Wine data, Red and White wine") 
# もしくはplot()関数
plot(wine_small[, -5],col=wine_small$colour, main="ScatterPlot Matrix of Wine data, Red and White wine") 

### plot 関数はデータ型に合わせて描画するプロットを自動で選ぶジェネリック関数
### とりあえずPlot()を適用し、不適切な場合他の関数を試してみるのがお勧め

plot(wine_small$fixed_acidity) # 散布図
hist(wine_small$fixed_acidity) # ヒストグラム
boxplot(wine_small$citric_acid)# 箱ひげ図

### 棒グラフ
count <- table(wine_small$colour) # tableに直してから
barplot(count)
plot(wine_small$colour, col=c("red","white"))# plotにおまかせか

### モザイク図
t1 <- table(wine_small$quality, wine_small$colour)
t1
mosaicplot(t1, color=TRUE)
plot(wine_small$quality, wine_small$colour) # plotまかせ



#################  Day1-Exercise4 様々な統計手法 #################
#### 主に複数の変数を組み合わせて分析（多変量解析）の紹介

########## 仮説検定: Hypothesis Test ##########
# 帰無仮説H0, 対立仮説H1, 有意水準; alpha (= 信頼係数: 1 - alpha)
x0<- wine_dat$volatile_acidity
x1<- wine_dat$fixed_acidity

#H0: x0=x1,  H1: x0 != x1 (not equal), alpha=0.05
t.test(x0, x1, conf.level = 1 - 0.05) #conf.level: 信頼係数. p-valueがとても小さいので, H0を棄却

#H0: x0= x0 + 0.001 ,  H1: x0 != x0 + 0.001  (not equal), alpha=0.05
t.test(x0,x0 + 0.001 , conf.level = 1 - 0.05) # > p-value: 0.7292。　帰無仮説H0を棄却できない。 


########## 回帰分析: Linear Regression ##########

### Model 1
m1 <- lm(density ~ quality + fixed_acidity + volatile_acidity, data=wine_dat)
summary(m1) #model fit

# Plotを確認する。
par(mfrow=c(2,2)) # plotを 1枚絵に 2x2の4つ描けるようにする
plot(m1)
par(mfrow=c(1,1))  # 1枚、1つに戻す

#外れ値(3709) を左上の残差プロットで観測したので
dat2 <- wine_dat[-3709,] # 3709行を削除

### Model 2 
m2 <- lm(density ~ quality + fixed_acidity + volatile_acidity, data=dat2)
summary(m2)  # 若干向上


########## 主成分分析: Principal Compornent Analysis ##########

## irisデータの1,2,3,4列目を使用
df_iris <- iris[,1:4]

## 相関係数行列から実行
res_cor <- prcomp(df_iris, scale=TRUE)
res_cor

summary(res_cor)
# 固有値(の平方根)に関する情報と各主成分の寄与率

### バイプロット
biplot(res_cor)



#################  Day1 おまけ #################
#******************************************# 
###########  ggplot2による可視化  ########## 
#******************************************# 

install.packages("ggplot2")
library(ggplot2)

# もう少し見た目が良いものがほしいので,よく使われる。
# 詳しい文法や事例はGoogleってみると、こんな形が欲しいというものは必ず見つかる。
# こういうのがいっぱい出てくるので
# https://heavywatal.github.io/rstats/ggplot2.html

# 基本的な説明としては、下記のように必要な要素を足し算できることで細かくプロットをいじれるようにしている。

# functionのざっくりとした説明
# ggplot: どのデータを対象とするか。いつも必要。
#     aes: aestheticのことでプロットの見た目を決める.
#     対象の変数(x,y,...),色(color),プロットの色を埋めるか(fill)
# geom_histogram: 描く絵をヒストグラムと指定。中の要素でどんなヒストグラムにするか具体的に指定していく。


###プロットに表示させるには長すぎるので、一旦ここでは一部の変数に注目する。
### Indexをサブセットする用にとっておく
short_ratio <- 1:4 # fixed_acidity  volatile_acidity citric_acid residual_sugar  
colour_index <-which(colnames(wine_dat) =="colour")

#　colはカラーを決める要素なのですが、integer/factorしか許容できないので、as.factorで変換。
pairs(wine_dat[,short_ratio],col=as.factor(wine_dat[,colour_index]),
      main="ScatterPlot Matrix of Wine data, Red and White wine") # R-default

###  Histogramsで最低限の例を示す。

#これはあくまで絵の対象(wine_datの変数: fixed_acidity)を決めるだけ。
ggplot(wine_dat, aes(x=fixed_acidity))

# これにヒストグラムを描くと指定。
ggplot(wine_dat, aes(x=fixed_acidity))+
  geom_histogram()

# 何もしないy軸カウントなので、ヒストグラムらしくDensityにする。
ggplot(wine_dat, aes(x=fixed_acidity, color=colour)) + 
  geom_histogram(aes(y=..density..))

#色を足してみる。
ggplot(wine_dat, aes(x=fixed_acidity, color=colour))+
  geom_histogram(aes(y=..density..))

# fill入れると
ggplot(wine_dat, aes(x=fixed_acidity, color=colour,fill=colour))+
  geom_histogram(aes(y=..density..))

# 色を縦に重ねないようにする(position)
ggplot(wine_dat, aes(x=fixed_acidity, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..), position="identity")

# 見えるように透過させる(alpha)
ggplot(wine_dat, aes(x=fixed_acidity, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..),alpha=0.5, position="identity")

# 密度のカーブを足してみる。
ggplot(wine_dat, aes(x=fixed_acidity, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity") + 
  geom_density(alpha=.2) 

# 足し算の強みは例えば、上のものをこうやって書くことでちょっと変えたものとかを簡単に作れる。
gp <- ggplot(wine_dat, aes(x=fixed_acidity, color=colour, fill=colour))
gp + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")
gp_hist <- gp + geom_histogram(aes(y=..density..), alpha=0.5, position="identity")
gp_hist2  <- gp_hist + geom_density(alpha=.2)
gp_hist2

### Histograms
hist1 <- ggplot(wine_dat, aes(x=fixed_acidity, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2) 

hist2 <- ggplot(wine_dat, aes(x=volatile_acidity, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2) 

hist3 <- ggplot(wine_dat, aes(x=citric_acid, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2) 

hist4 <- ggplot(wine_dat, aes(x=residual_sugar, color=colour, fill=colour)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
  geom_density(alpha=.2) 

hist1 #fixed_acidity
hist2 #volatile_acidity
hist3 #citric_acid
hist4 #residual_sugar

### BoxPlots
box1 <- ggplot(wine_dat, aes(x=colour,y=fixed_acidity, color=wine_dat$quality))+
  geom_boxplot()
box2 <- ggplot(wine_dat, aes(x=colour,y=volatile_acidity, color=wine_dat$quality))+
  geom_boxplot()
box3 <- ggplot(wine_dat, aes(x=colour,y=citric_acid, color=wine_dat$quality))+
  geom_boxplot()
box4 <- ggplot(wine_dat, aes(x=colour,y=residual_sugar, color=wine_dat$quality))+
  geom_boxplot()

box1 #fixed_acidity
box2 #volatile_acidity
box3 #citric_acid
box4 #residual_sugar

### ScatterPlot Matrix
install.packages("GGally") # ggplotの拡張package
library(GGally)
ggpairs(wine_dat[,c(short_ratio,colour_index)], aes(colour = colour, alpha = 0.4))

############################################
###########  Rの基本シンタックス ###########  
############################################
# integer/numerics

#　演算子
1+1 # 
5-1
10/2
2*5 
2**5 # 5乗
sqrt(2) # root 2

# vector 
c(1,2,3) # 

seq(1,10) # sequence. default by 1
seq(1,10,2) # by 2

1:5  # x:y はseq(x, y)のshorthand

length(1:5) # ベクトルの要素数

# character
c("a", "b", "c")
letters # default alphabet 
LETTERS

paste(letters, "Hello", sep=" ") # character くっつける。 sepはデリミタ
paste(LETTERS,collapse="") # 一つのstringにする。

length(c("a","b","c")) # vectorの要素数. 
nchar("abc") # stringの長さ
nchar(letters) #複数可

# Logical vecotrs
c(TRUE,FALSE)
c(T,F) #shorthand

1==1 #boolean equal
1!=1 #boolean not equal
1:3 == 1 # 右なのはどこ?
1 %in% 1:5 # 左は右に入ってる?

!(1==1) # Logical Not. 結果を
!(TRUE)


#functionの書き方
add5 <-function(x){
  return(x + 5)
}
add5(10) 
# 複数なら, function(x, y, z)って感じ。

# Loop. ifとかも基本同じ書き方。
for(i in 1:5){
  print(i)
}
############################End of Code#############################################