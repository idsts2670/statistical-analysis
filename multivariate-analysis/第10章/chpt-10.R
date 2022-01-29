#男女別のクロス集計表の作成
#データの読み込み
bdat <- read.csv('自転車データ.csv')
bdat

#年代とメーカーのクロス集計表
tmpm <- table(bdat$年代, bdat$メーカー)

#男性におけるクロス集計表の作成
mm <- matrix(bdat$度数[1:9], ncol=3, nrow=3)
colnames(mm) <- colnames(tmpm)
rownames(mm) <- rownames(tmpm)
#女性におけるクロス集計表の作成
fm <- matrix(bdat$度数[10:18], ncol=3, nrow=3)
colnames(fm) <- colnames(tmpm)
rownames(fm) <- rownames(tmpm)

mm #男性におけるクロス集計表
fm #女性におけるクロス集計表

#飽和モデルの分析
fullmodel <- glm(度数~年代*性別*メーカー, data=bdat, family='poisson')
summary(fullmodel)

#独立モデル
idmodel <- glm(度数~年代+性別+メーカー, data=bdat, family='poisson')
summary(idmodel)

#飽和モデルと独立モデルの尤度比検定
anova(idmodel, fullmodel, test='Chisq')
#飽和モデルと独立モデルのAICとBIC
extractAIC(fullmodel) #飽和モデルAIC
extractAIC(idmodel) #独立モデルAIC
extractAIC(fullmodel, k=log(sum(bdat$度数))) #飽和モデルのBIC
extractAIC(idmodel, k=log(sum(bdat$度数))) #独立モデルのBIC

#修正した提案モデルの分析
bestmodel <- glm(度数~年代+性別+メーカー+(年代:性別)+(年代:メーカー), data=bdat, family='poisson')
summary(bestmodel)

#飽和モデルと提案モデルの尤度比検定
anova(bestmodel, fullmodel, test='Chisq')
#提案モデルのAICとBIC
extractAIC(bestmodel) #提案モデルAIC
extractAIC(bestmodel, k=log(sum(bdat$度数))) #提案モデルのBIC

#最良モデルの期待度数行列
xtabs(bestmodel$fitted.values~bdat$年代+bdat$メーカー+bdat$性別)

#飽和モデルの元での期待度数行列
xtabs(fullmodel$fitted.values~bdat$年代+bdat$メーカー+bdat$性別)

#基準セルの設定方法
bdat$年代 <- factor(bdat$年代, levels = c("30代", "20代", "40代"))
bdat$性別 <- factor(bdat$性別, levels = c("M", "F"))
bdat$メーカー <- factor(bdat$メーカー, levels = c("ピロリロ", "コレナゴ", "デロンザ"))


#章末問題
#問１
bdat2 <- read.csv('自転車データ練習1.csv')
bdat2
#基準セル（30代男性のチネッロユーザー）の設定
bdat2$年代 <- factor(bdat2$年代, levels = c('30代', '20代', '40代'))
bdat2$メーカー <- factor(bdat2$メーカー, levels = c('チネッロ', 'カレッラ', 'クォーク'))
bdat2$性別 <- factor(bdat2$性別, levels = c('M', 'F'))

#問2
#飽和モデルの分析
fullmodel2 <- glm(度数~年代*性別*メーカー, data=bdat2, family = "poisson")
#独立モデルの分析
idmodel2 <- glm(度数~年代+性別+メーカー, data=bdat2, family='poisson')

#問３
#逸脱度に基づく尤度比検定
anova(fullmodel2, idmodel2, test='Chisq')

#飽和モデルと独立モデルのAICとBIC
extractAIC(fullmodel2) #飽和モデルAIC
extractAIC(fullmodel2, k=log(sum(bdat$度数))) #飽和モデルのBIC
extractAIC(idmodel2) #独立モデルAIC
extractAIC(idmodel2, k=log(sum(bdat2$度数))) #独立モデルのBIC
#飽和モデルの相対的な適合度の高さが示されている(χ^2(12)=753.89, p<0.001)
#AICやBICと言った情報基準の観点からも飽和モデルの相対的な適合度が示されている

#問４
summary(fullmodel2)
#年代20代×性別F×カレッラの2次交互作用効果がp<0.05水準で有意である

#問５
#飽和モデルの元での期待度数行列
xtabs(fullmodel2$fitted.values~bdat2$年代+bdat2$メーカー+bdat2$性別)

#問６
#飽和モデルにおいて、基準セル（30代男性のチネッロユーザー）に対する対象セル（20代女性カレッラユーザー）のオッズ比から求める
(frate <- (837/1266)/(649/442))
(mrate <- (744/888)/(626/432))
(frate/mrate)
log(frate/mrate)

#問７
#20代女性のカレッラユーザーのオッズ比は0.450である。これは基準となる同性の20代チネッロユーザーオッズの0.45倍であり、基準と比較してカレッラユーザーが相対的に少ないことを示唆している。
#一方、20代男性のカレッラユーザーのオッズ比は0.578であり、女性と同様に、基準と比較してカレッラユーザーが少ないことが伺える。
#両者のオッズ比は0.77854であるから、同一基準のもとで考えれば、男性より女性の方が20代のカレッラユーザーが相対的に少ないことが伺える。





