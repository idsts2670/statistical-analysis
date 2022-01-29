#階層的クラスター分析
#データの読み込み，データフレームの確認
ysi <- read.csv("野菜の栄養.csv", row.names = 1) #データの読み込み
head(ysi)

#非類似度行列の算出
D0 <- dist(ysi, method = "euclidean")
D <- (1/2) * D0 ^ 2

#デンドログラム作成のための下処理
ysi.out <- hclust(d = D, method="ward.D")

#デンドログラムの作成
plot(as.dendrogram(ysi.out), xlim = c(300000000, 0), xlab = "非類似度", horiz = TRUE)
plot(as.dendrogram(ysi.out), ylim = c(0, 12000), ylab = "非類似度", horiz = FALSE, nodePar = list(lab.cex = 0.8, angle = 180, pch = NA))

#クラスター数の妥当性の確認
CNvalidity <- function(dat, clusters){
	CNvalidity0 <- function(dat, clusters, index = NULL){ #CH, H, KLの値を個別に返却する関数CNvalidity0
	# indexの指定エラー処理
	if(!is.element(index, c("CH", "H", "KL"))){
		stop("indexの指定が正しくありません")
	}
	N <- ncol(clusters) - 2 #数値を出すクラスターの数（最初と最後の列は対象外）
	d <- ncol(dat) #変数の数
	value <- data.frame(cluster.n = names(clusters)[2:(ncol(clusters)-1)], numeric(N)) #指標の値を収めるためのオブジェクト
	colnames(value)[2] <- index #指標名(CH, H, KLのどれか)をベクトルの列名に与える
	W <- function(X){ #平方和積和行列を返却する関数W
		X <- as.matrix(X)
		V <- X-(rep(1, nrow(X)) %*% t(colMeans(X))) #平均偏差化データ
		W <- t(V) %*% V
		return(W)
	}
	from <- as.numeric(names(clusters)[2]) #指標の値を算出する最初のクラスター数
	to <- as.numeric(names(clusters)[(ncol(clusters) - 1)]) #指標の値を算出する最後のクラスター数
	for(i in from:to){　#iは指標を算出するクラスター数
		# dat0-datにクラスター数iのときのクラスター,クラスター数i+1のときのクラスター,クラスター数i-1のときのクラスターを列として加えたデータフレーム
		dat0 <- data.frame(dat, cluster1 = factor(clusters[, names(clusters) == as.character(i)]), 
			cluster2 = factor(clusters[, names(clusters) == as.character(i + 1)]), 
			cluster3 = factor(clusters[, names(clusters) == as.character(i - 1)]))
		Ws1 <- by(data = dat0[, 1:d], INDICES = dat0$cluster1, FUN = W) #クラスター数iのときの各クラスターの平方和積和行列のリスト
		Ww1 <- Reduce(f = "+", Ws1) #クラスター数iのときの群内平方和積和行列(各クラスターの平方和積和行列の和)
		if(index == "CH"){
			indexname <- "Calinski & Harabasz index"
			Z  <- Reduce(f = rbind, by(data = dat0[, 1:d],INDICES = dat0$cluster1, FUN = colMeans)) #クラスター数iのときの各クラスター平均を各行に持つ行列(i×d)
			ZZ <- Z - rep(1, i) %*% t(colMeans(dat0[, 1:d])) #Zから全体平均を減じた行列(i×d)
			Nc <- diag(table(dat0$cluster1))#クラスター数iのときの各クラスターの対象数を対角要素とする対角行列
			Wb <- t(ZZ) %*% Nc %*% ZZ #クラスター数iのときの群間平方和積和行列
			indexvalue <- (sum(diag((Wb))) / (i - 1))/(sum(diag((Ww1))) / (nrow(dat0) - i)) #CHの値
		}else if(index == "H"){
			indexname <- "Hartigan index"
			Ws2 <- by(data = dat0[, 1:d], INDICES = dat0$cluster2, FUN = W) #クラスター数i+1のときの各クラスターの平方和積和行列のリスト
			Ww2 <- Reduce(f = "+", Ws2) #クラスター数i+1のときの群内平方和積和行列(各クラスターの平方和積和行列の和)
			indexvalue <- (sum(diag((Ww1))) / sum(diag((Ww2))) - 1) * (nrow(dat0) - i - 1) #Hの値
			if(i == from){ #diffHを算出するための処理（最初のクラスター数-1のときのHの値）
				Ws3 <- by(data = dat0[, 1:d], INDICES = dat0$cluster3, FUN = W)#クラスター数i-1のときの各クラスターの平方和積和行列のリスト
				Ww3 <- Reduce(f = "+", Ws3) #クラスター数i-1のときの群内平方和積和行列(各クラスターの平方和積和行列の和)		
				indexvalue_sub <- (sum(diag((Ww3))) / sum(diag((Ww1))) - 1 ) * (nrow(dat0) - (i - 1) - 1) #diffHの値
			}
		}else if(index == "KL"){
			indexname <- "Krzanowski & Lai index"
			Ws2 <- by(data = dat0[, 1:d], INDICES = dat0$cluster2, FUN = W)　#クラスター数i+1のときの各クラスターの平方和積和行列のリスト
			Ww2 <- Reduce(f = "+", Ws2) #クラスター数i+1のときの群内平方和積和行列(各クラスターの平方和積和行列の和)
			Ws3 <- by(data = dat0[, 1:d], INDICES = dat0$cluster3, FUN = W)　#クラスター数i-1のときの各クラスターの平方和積和行列のリスト
			Ww3 <- Reduce(f = "+", Ws3) #クラスター数i-1のときの群内平方和積和行列(各クラスターの平方和積和行列の和)
			DIFF1 <- sum(diag((Ww3))) * (i - 1) ^ (2 / d) - sum(diag((Ww1))) * (i) ^ (2 / d) #KLの分子
			DIFF2 <- sum(diag((Ww1))) * (i) ^ (2 / d) - sum(diag((Ww2))) * (i + 1) ^ (2 / d) #KLの分母
			indexvalue <- abs(DIFF1 / DIFF2) #KLの値
		}		
		value[value[, "cluster.n"] == as.character(i), 2] <- indexvalue #指標の値の代入
	}

	if(index == "H"){
		#value-Hの値とdiffHの値を各列とするオブジェクト
		value <- data.frame(value, diffH = c(-1 * diff(c(indexvalue_sub, value[, index]))))
	}
	return(value)
	}
	#関数CNvalidity0を用いた各指標の値の算出と統合
	CHindex <- CNvalidity0(dat = dat, clusters = clusters, index = "CH") 
	 Hindex <- CNvalidity0(dat = dat, clusters = clusters, index = "H") 
	KLindex <- CNvalidity0(dat = dat, clusters = clusters, index = "KL")
	indices <- merge(merge(CHindex, Hindex, by = "cluster.n", sort = FALSE), KLindex, by = "cluster.n", sort = FALSE)
	return(indices)
}
from <- 1; to <- 11
clabel <- function(x){factor(cutree(ysi.out, k = x))}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
head(clusters)
CNvalidity(dat = ysi, clusters = clusters)

#割り当てられたクラスターの確認
(cluster <- factor(cutree(ysi.out, k = 3))) #3つのクラスターに分類

#各クラスターの平均
by(ysi, INDICES = cluster, FUN = function(x){apply(x, 2, mean)}) #FUNに自作関数を指定して，一気に出力

#z得点化データの分析（デンドログラムの作成まで）
ysi.stdz <- scale(ysi)
D0.stdz <- dist(ysi.stdz, method = "euclidean")
D.stdz <- (1 / 2) * D0.stdz ^ 2
ysi.stdz.out <- hclust(d = D.stdz, method = "ward.D")#Ward-inputはユークリッド平方距離
plot(as.dendrogram(ysi.stdz.out), xlim = c(120, 0), xlab = "非類似度", horiz = TRUE)


#非階層的クラスター分析
#クラスターの形成と結果の確認
INTP.KM <- function(dat, ncluster){ #クラスター数に応じて各クラスターの初期値となる値を返す関数
	M <- nrow(dat) #全体の対象数
	center.all <- apply(dat, 2, mean) #全体平均
	DVATmat <- as.matrix((dat - rep(1, M) %*% t(center.all))) #平均偏差化データ
	DTC0 <- diag(DVATmat %*% t(DVATmat)) #各対象と全体平均との平方ユークリッド距離
	INTP <- matrix(0, ncluster, ncol(dat)) #各行に各クラスターの初期値を収めるためのオブジェクト
	dat.ordered <- dat[order(DTC0), ] #平方ユークリッド距離の順での元のデータフレームの並べ替え
	for(i in 1:ncluster){
		INTP[i, ]<-unlist(dat.ordered[1 + (i - 1) * floor(M / ncluster), , drop = FALSE]) #初期値となる対象の観測値の代入
	}
	return(INTP)
}
INTP <- INTP.KM(dat = ysi, ncluster = 3)
(ysi.out2 <- kmeans(x = ysi, centers = INTP))

#クラスター数の妥当性の確認
from <- 1; to <- 11
clabel <- function(x){factor(kmeans(x = ysi, centers = INTP.KM(dat = ysi, ncluster = x))$cluster)}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
CNvalidity(dat = ysi, clusters = clusters)

#z得点化データの分析（クラスターの形成と結果の確認）
INTP.stdz <- INTP.KM(dat = ysi.stdz, ncluster = 3)
(ysi.stdz.out2 <- kmeans(x = ysi.stdz, centers = INTP.stdz))


#問題と解答
#問1解答
tsks <- read.csv("都市の気象.csv", row.names = 1)

#問2解答
tsks.stdz <- scale(tsks)

#問3解答
D0.stdz <- dist(tsks.stdz, method = "euclidean")
D.stdz<-(1 / 2) * D0.stdz ^ 2
tsks.stdz.out <- hclust(d = D.stdz, method = "ward.D")
plot(as.dendrogram(tsks.stdz.out), xlim = c(100, 0), xlab = "非類似度", horiz = TRUE)

#問4解答
from <- 1; to <- 11
clabel <- function(x){factor(cutree(tsks.stdz.out, k = x))}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
CNvalidity(dat = tsks.stdz, clusters = clusters)

#問5解答
(cluster <- factor(cutree(tsks.stdz.out, k = 5))) #5つのクラスターに分類
by(tsks.stdz, INDICES = cluster, FUN = function(x){apply(x, 2, mean)}) #FUNに自作関数を指定して，一気に出力

