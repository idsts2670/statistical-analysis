aks <- read.csv("空き巣調査.csv") #データの読み込み
aks$空き巣01 <- ifelse(aks$空き巣=='あり', 1, 0)　#カテゴリカル変数の変換
aks$セキュリティ01 <- ifelse(aks$セキュリティ=='加入', 1, 0)
aks$飼い犬01 <- ifelse(aks$飼い犬=='あり', 1, 0)
head(aks)

#ロジスティック回帰分析の実行
aks.out <- glm(空き巣01~不在時間+会話+築年数+セキュリティ01+飼い犬01, family='binomial', data = aks)
summary(aks.out) #回帰分析の出力表示
#係数・切片を指数変換して算出
exp(aks.out$coefficients)
#係数・切片に関する信頼区間の算出
confint(aks.out, level = 0.95)
exp(confint(aks.out, level=0.95))
#標準化係数の算出
LRAstdcoef <- function(glm.out, vname){ #glm.out-関数glmによる出力オブジェクト, vname-標準化する量的な説明変数名
  #vnameに指定された変数のみからなるデータフレーム
  subdat <- (glm.out[["data"]])[ , vname]
  #subdatの各変数の標準偏差
  SDs <- apply(subdat, 2, sd)
  #標準化前の推定値
  rawcoef <- (glm.out[["coefficients"]])[vname]
  #標準化後の推定値
  stdcoef <- rawcoef * SDs #推定値に標準偏差をかけるのはなぜ
  return(stdcoef)
}
LRAstdcoef(aks.out,c("不在時間", "会話","築年数"))

#モデル評価
#Hosmer-Lemeshowの適合度検定
install.packages('ResourceSelection')
library(ResourceSelection)
hoslem.test(x=aks.out$y, y=fitted(aks.out)) #p値が0.1459より有意でないのでモデルは不適合であるとは言えず、当てはまっていると解釈する
extractAIC(aks.out) #AIC
extractAIC(aks.out, k=log(nrow(aks.out$data))) #BIC

#その他3つの有益な指標
#説明変数群の有効性の確認
aks.out_null <- glm(空き巣01~1, family = 'binomial', data=aks)
anova(aks.out_null, aks.out, test='Chisq') #P値より説明変数郡による目的変数の説明・予測が有効である
#変数選択
step(aks.out_null, direction = 'both',
     scope = ('~不在時間+会話+築年数+セキュリティ01+飼い犬01'))
#多重共線性
install.packages("car")
library(car)
install.packages('tibble')
library(tibble)
