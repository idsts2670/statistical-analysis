#男女別のクロス集計表の作成
bdat <- read.csv("自転車データ.csv")#データの読み込み

#年代とメーカーのクロス集計表
bdat$年代 <- as.character(bdat$年代)
tmpm <- table(bdat$年代,bdat$メーカー)

mm <- matrix(bdat$度数[1:9],ncol=3,nrow=3) #男性におけるクロス集計表の作成
colnames(mm) <- colnames(tmpm)
rownames(mm) <- rownames(tmpm)
mm

fm <- matrix(bdat$度数[10:18],ncol=3,nrow=3) #女性におけるクロス集計表の作成 
colnames(fm) <- colnames(tmpm)
rownames(fm) <- rownames(tmpm)
fm


#関数glmによる飽和モデルの分析
fullmodel <- glm(度数‾年代*性別*メーカー,data=bdat,family="poisson")

#飽和モデルの出力
summary(fullmodel)

#関数glmによる独立モデルの分析
idmodel <- glm(度数‾年代+性別+メーカー,data=bdat,family="poisson")

#独立モデルの出力
summary(idmodel)

#独立モデルの分散分析表
anova(idmodel,fullmodel,test="Chisq")

#飽和モデルと独立モデルのAICとBIC
extractAIC(fullmodel) #飽和モデルAIC
extractAIC(idmodel) #独立モデルAIC
extractAIC(fullmodel,k=log(sum(bdat$度数))) #飽和モデルBIC
extractAIC(idmodel,k=log(sum(bdat$度数))) #独立モデルBIC

#関数glmによる提案モデルの分析
bestmodel <- glm(度数‾年代+性別+メーカー+(年代:性別)+(年代:メーカー),data=bdat,family="poisson")

#関数glmによる提案モデルの出力
summary(bestmodel)

#飽和モデルと提案モデルの尤度比検定
anova(bestmodel,fullmodel,test="Chisq")


#最良モデルの期待度数行列
xtabs(bestmodel$fitted.values‾bdat$年代+bdat$メーカー+bdat$性別)



#飽和モデルのもとでの期待度数行列
xtabs(fullmodel$fitted.values‾bdat$年代+bdat$メーカー+bdat$性別)


#基準セルの設定
bdat$年代 <-factor(bdat$年代,levels=c("30代","20代","40代"))
bdat$性別 <-factor(bdat$性別,levels=c("M","F"))
bdat$メーカー <-factor(bdat$メーカー,levels=c("ピロリロ","コレナゴ","デロンザ"))



#演習と解答

#問1解答
bdat2 <- read.csv("自転車データ練習1.csv") 
bdat2$年代<- factor(bdat2$年代,levels=c("30代","20代","40代"))
bdat2$性別<-factor(bdat2$性別,levels=c("M","F"))
bdat2$メーカー<-factor(bdat2$メーカー,levels=c("チネッロ","カレッラ","クォーク"))


#問2解答
fullmodel2 <- glm(度数‾年代*性別*メーカー,data=bdat2,family="poisson")
indmodel2 <- glm(度数‾年代+性別+メーカー,data=bdat2,family="poisson")

#問3解答
anova(indmodel2,fullmodel2,test="Chisq")

#問4解答
summary(fullmodel2)

#問5解答
xtabs(fullmodel2$fitted.values‾年代+メーカー+性別,data=bdat2)

#問6解答
(frate <- ((837/1266)/(649/442)))#女性の比率
(mrate <- ((744/888)/(626/432)))#男性の比率
log(frate/mrate)
