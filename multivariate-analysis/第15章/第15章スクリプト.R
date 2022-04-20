#グループ化・グループの影響の検討
#データの読み込み，データフレームの確認
tmp <- read.csv("店舗調査.csv", row.names = 1) #データの読み込み
head(tmp)

#階層的クラスター分析の実行
tmp_clt <- tmp[, c("明るさ", "広さ", "整然さ", "清潔さ")]
D0 <- dist(tmp_clt, method = "euclidean")
D<-(1 / 2) * D0 ^ 2
tmp.out <- hclust(d = D, method = "ward.D")
plot(as.dendrogram(tmp.out), ylab = "非類似度", nodePar = list(lab.cex = 0.5, pch = NA), ylim = c(0, 500))

#クラスター数の違いの図示
cluster3 <- as.factor(cutree(tmp.out, k = 3))
cluster4 <- as.factor(cutree(tmp.out, k = 4))
tmp_clt_resW <- data.frame(tmp_clt, "クラスター数3" = cluster3, "クラスター数4" = cluster4, "店舗" = row.names(tmp_clt))
library(tidyr)
tmp_clt_resL1 <- gather(tmp_clt_resW, key = 観点, value = 評価値, -クラスター数3, -クラスター数4, -店舗)
tmp_clt_resL2 <- gather(tmp_clt_resL1, key = クラスター数, value = クラスター, -観点, -評価値, -店舗)
library(ggplot2)
P0 <- ggplot(data = tmp_clt_resL2, aes(x = 観点))
P1 <- P0 + geom_line(aes(y = 評価値, color = クラスター, linetype = クラスター, group = 店舗), stat = "identity")
(P2 <- P1 + facet_wrap( ~ クラスター数))

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
clabel <- function(x){factor(cutree(tmp.out, k = x))}
clusters <- data.frame(lapply(from:to, clabel))
names(clusters) <- from:to
CNvalidity(dat = tmp_clt, clusters = clusters)

#一要因の分散分析（全体）
tmp_aov <- data.frame(tmp, "クラスター" = cluster4)
tapply(tmp_aov[, "滞在時間"], INDEX = tmp_aov[, "クラスター"], FUN = mean)
tmp_aov.out <- aov(formula = 滞在時間 ~ クラスター, data = tmp_aov)
summary(tmp_aov.out)

#一要因の分散分析（多重比較）
TukeyHSD(tmp_aov.out)

#尺度得点化・尺度得点による説明
#データの読み込み，データフレームの確認
kbs  <-  read.csv("競馬調査.csv") #データの読み込み
head(kbs)

#データの整形と因子数の検討
kbs_fa  <-  kbs[, 1:20]
library(psych)
VSS.scree(kbs_fa)
eigen(cor(kbs_fa))$values
fa.parallel(kbs_fa, fm = "ml", fa = "pc", n.iter = 100)

#探索的因子分析の結果
library(GPArotation)
kbs_fa.out  <-  fa(kbs_fa, nfactors = 2, fm = "ml", rotate = "promax")
print(kbs_fa.out, sort = TRUE, digits = 3)

#α係数の算出
kbs_S1 <- kbs_fa[, 1:10]
alpha(kbs_S1)
kbs_S2 <- kbs_fa[, -1 * c(1:10, 18, 19)] #1から10, 18, 19列以外を取り出す
alpha(kbs_S2)

#尺度得点の算出
S1 <- rowSums(kbs_S1)
S2 <- rowSums(kbs_S2)

#階層的重回帰分析用のデータフレーム作成
kbs_hmr  <-  data.frame(kbs[, c("サザエ", "収支", "性別", "年齢")], S1, S2)
head(kbs_hmr)

#階層的重回帰分析の実行（分散説明率の増分の検定）
M1 <- lm(サザエ ~ 性別 + 年齢 + 収支, data = kbs_hmr)
(M1_R2 <- summary(M1)$r.squared)
M2 <- lm(サザエ ~ 性別 + 年齢 + 収支 + S1 + S2, data = kbs_hmr)
(M2_R2 <- summary(M2)$r.squared)
M2_R2 - M1_R2 #分散説明率の増分
anova(M1, M2)

#階層的重回帰分析の実行（AICの算出）
extractAIC(M1)
extractAIC(M2)

#投入後の重回帰分析の結果
summary(M2)
confint(M2,level = 0.95)



#測定状況の確認・多変数間の関係の検討
#データの読み込み，データフレームの確認
dsk <- read.csv("男子校調査.csv") #データの読み込み
head(dsk)

#確認的因子分析モデルの推定
dsk_model_cfa <- "
情緒 =‾ 情緒1 + 情緒2 + 情緒3 + 情緒4 + 情緒5
暴力 =‾ 暴力1 + 暴力2 + 暴力3 + 暴力4 + 暴力5
"
library(lavaan)
dsk.out_cfa <- cfa(model = dsk_model_cfa, data = dsk)
summary(dsk.out_cfa, fit.measures = TRUE, standardized = TRUE, ci = TRUE)

#検討モデルのモデル記述
dsk_model_path <- "
学業 =~ 1 * 学業1 + 学業2 + 学業3 + 学業4 + 学業5
友人 =~ 1 * 友人1 + 友人2 + 友人3 + 友人4 + 友人5
情緒 =~ 1 * 情緒1 + 情緒2 + 情緒3 + 情緒4 + 情緒5
暴力 =~ 1 * 暴力1 + 暴力2 + 暴力3 + 暴力4 + 暴力5
情緒 ~ 学業 + 友人 + BMI
暴力 ~ 学業 + 友人 + BMI
学業 ~~ 友人 + BMI
友人 ~~ BMI
情緒 ~~ 暴力
"

#検討モデルの推定結果
dsk.out_path <- lavaan(model = dsk_model_path, data = dsk, auto.var = TRUE)
summary(dsk.out_path, fit.measures = TRUE, standardized = TRUE, ci = TRUE)
