#データの読み込み
sts <- read.csv("ストレス.csv")

#データフレームの確認
head(sts)

#関数lmによる重回帰分析の実行（ステップ1）
res1 <- lm(バーンアウト2‾バーンアウト1, data=sts)
summary(res1)

#関数lmによる重回帰分析の実行（ステップ2）
res2 <- lm(バーンアウト2‾バーンアウト1+ストレス+サポート, data=sts)
summary(res2)

#F値の算出
((0.4168-0.4039)/(3-1))/((1-0.4168)/(300-3-1))

#p値の算出
1-pf(3.273663, 2, 296)

#決定係数の増分の検定
anova(res1, res2)

#AICの算出
extractAIC(res1) #ステップ1の回帰モデルのAIC
extractAIC(res2) #ステップ2の回帰モデルのAIC

#BICの算出
extractAIC(res1, k=log(300)) #ステップ1の回帰モデルのBIC
extractAIC(res2, k=log(300)) #ステップ2の回帰モデルのBIC

#中心化
sts$ストレス.c <- sts$ストレス-mean(sts$ストレス) #「ストレス」の中心化
sts$サポート.c <- sts$サポート-mean(sts$サポート) #「サポート」の中心化

#交互作用項の作成
sts$交互作用 <- sts$ストレス*sts$サポート #中心化をする前の交互作用項
sts$交互作用.c <- sts$ストレス.c*sts$サポート.c #中心化をした後の交互作用項

#相関係数の確認
cor(sts[,c("ストレス", "サポート", "交互作用")]) #中心化前の相関行列
cor(sts[,c("ストレス.c", "サポート.c", "交互作用.c")]) #中心化後の相関行列

#「バーンアウト1」の中心化
sts$バーンアウト1.c <- sts$バーンアウト1-mean(sts$バーンアウト1)

#関数lmによる重回帰分析の実行（交互作用効果の検討）
res3 <- lm(バーンアウト2‾バーンアウト1.c+ストレス.c+サポート.c+ストレス.c*サポート.c, data=sts)
summary(res3)

#決定係数の増分の検定
anova(res2, res3)

#標準偏回帰係数の算出
z.sts <- as.data.frame(scale(sts)) #データの標準化とデータフレーム化
res3.z <- lm(バーンアウト2‾バーンアウト1+ストレス+サポート+ストレス*サポート, data=z.sts) #標準偏回帰係数の推定
summary(res3.z) #結果の出力

#単純傾斜分析（ソーシャル・サポートが多い場合のバーンアウトに対するストレス経験の効果）
sts$サポート.h <- sts$サポート.c-sd(sts$サポート.c)
res3.h <- lm(バーンアウト2‾バーンアウト1.c+ストレス.c+サポート.h+ストレス.c*サポート.h, data=sts)
summary(res3.h)

#単純傾斜分析（ソーシャル・サポートが少ない場合のバーンアウトに対するストレス経験の効果）
sts$サポート.l <- sts$サポート.c+sd(sts$サポート.c)
res3.l <- lm(バーンアウト2‾バーンアウト1.c+ストレス.c+サポート.l+ストレス.c*サポート.l, data=sts)
summary(res3.l)

#信頼区間の算出
confint(res3)

#変数選択
bsb <- read.csv("野球.csv") #データの読み込み
head(bsb) #データフレームの確認
library(MASS) #パッケージMASSの読み込み
base <- lm(年俸‾1, data=bsb) #切片のみのモデル
step.res <- stepAIC(base, direction="both", scope=list(upper=‾打数+安打+打点+本塁打+四球+死球+三振+打率)) #ステップワイズ法による変数選択
summary(step.res) #最終的な変数選択の結果


#演習と解答
#問1解答
ssk <- read.csv("成績.csv")
res1 <- lm(テスト成績‾性別+通塾有無+有能感, data=ssk)
summary(res1)
res2 <- lm(テスト成績‾性別+通塾有無+有能感+アスピレーション, data=ssk)
summary(res2)

#問2解答
anova(res1, res2)

#問3解答
extractAIC(res1)
extractAIC(res2)

#問4解答
ssk$アスピレーション.c <- ssk$アスピレーション-mean(ssk$アスピレーション)
res3 <- lm(テスト成績‾性別+通塾有無+有能感+アスピレーション.c+通塾有無*アスピレーション.c, data=ssk)
summary(res3)

#問5解答
ssk$アスピレーション.h <- ssk$アスピレーション.c-sd(ssk$アスピレーション.c)
res3.h <- lm(テスト成績‾性別+通塾有無+有能感+アスピレーション.h+通塾有無*アスピレーション.h, data=ssk)
summary(res3.h)

ssk$アスピレーション.l <- ssk$アスピレーション.c+sd(ssk$アスピレーション.c)
res3.l <- lm(テスト成績‾性別+通塾有無+有能感+アスピレーション.l+通塾有無*アスピレーション.l, data=ssk)
summary(res3.l)


#問6解答
library(MASS)
base <- lm(テスト成績‾1, data=ssk)
step.res <- stepAIC(base, direction="both", scope=list(upper=‾性別+通塾有無+有能感+アスピレーション+平日勉強時間+休日勉強時間+ニュース視聴+読書+外遊び))
summary(step.res)
