#データの読み込み、カテゴリカル変数の変換、データフレームの確認
aks <- read.csv("空き巣調査.csv") #データの読み込み
aks$空き巣01 <- ifelse(aks$空き巣 == "あり",1,0)
aks$セキュリティ01 <- ifelse(aks$セキュリティ == "加入",1,0)
aks$飼い犬01 <- ifelse(aks$飼い犬 == "あり",1,0)
head(aks)

#ロジスティック回帰分析の実行
aks.out <- glm(空き巣01 ‾ 不在時間 + 会話 + 築年数 + セキュリティ01 + 飼い犬01, family = "binomial", data = aks)

#ロジスティック回帰分析の出力表示
summary(aks.out)

#係数・切片の指数変換値の算出と解釈
exp(aks.out$coefficients)

#係数・切片に関する信頼区間の算出
confint(aks.out, level = 0.95)
exp(confint(aks.out, level = 0.95))

#標準化係数の算出
LRAstdcoef <- function(glm.out, vname){ #glm.out-関数glmによる出力オブジェクト, vname-標準化する量的な説明変数名
	#vnameに指定された変数のみからなるデータフレーム
	subdat <- (glm.out[["data"]])[ , vname]
	#subdatの各変数の標準偏差
	SDs <- apply(subdat, 2, sd)
	#標準化前の推定値
	rawcoef <- (glm.out[["coefficients"]])[vname]
	#標準化後の推定値
	stdcoef <- rawcoef * SDs
	return(stdcoef)
}
LRAstdcoef(aks.out,c("不在時間", "会話", "築年数"))

#Hosmer-Lemeshowの適合度検定
library(ResourceSelection) 
hoslem.test(x = aks.out$y, y = fitted(aks.out)) #x-目的変数の観測値, y-目的変数の予測値（確率）

#AICとBICの算出
extractAIC(aks.out) #AIC
extractAIC(aks.out, k = log(nrow(aks.out$data))) #BIC

#独立変数群の有効性の確認
aks.out_null <- glm(空き巣01‾1, family = "binomial", data = aks)
anova(aks.out_null, aks.out, test="Chisq") 

#変数選択の実行
step(aks.out_null, direction = "both", scope = ( ‾ 不在時間 + 会話 + 築年数 + セキュリティ01 + 飼い犬01))

#多重共線性の確認
library(car)
vif(aks.out)


#演習と解答
#問1解答
sks <- read.csv("資格試験.csv")
sks$試験結果01 <- ifelse(sks$試験結果 == "合格", 1, 0)
sks$祈願01 <- ifelse(sks$祈願 == "あり", 1, 0)

#問2解答
sks.out <- glm(試験結果01 ‾ 勉強時間 + 祈願01 + 年齢, family = "binomial", data = sks)
summary(sks.out)

#問3解答
exp(sks.out$coefficients)

#問4解答
LRAstdcoef(sks.out, c("勉強時間", "年齢")) #前述のLRAstdcoefの読込みが必要
exp(LRAstdcoef(sks.out, c("勉強時間", "年齢")))

#問5解答
library(ResourceSelection) 
hoslem.test(x = sks.out$y, y = fitted(sks.out)) 

#問6解答
library(car)
vif(sks.out)
