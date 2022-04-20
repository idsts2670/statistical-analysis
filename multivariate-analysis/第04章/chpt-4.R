#階層的重回帰分析の実行

sts <- read.csv('ストレス.csv')
head(sts)
#ステップ1の回帰分析
res1 <- lm(バーンアウト2~バーンアウト1, data=sts)
summary(res1)
#ステップ2の回帰分析
res2 <- lm(バーンアウト2~バーンアウト1+ストレス+サポート, data=sts)
summary(res2)
#F値による決定係数の増分の検定
((0.4168-0.4039)/(3-1))/((1-0.4168)/(300-3-1)) #F値の算出
1-pf(3.273663, 2, 296) #p値の算出
#関数anovaを用いた決定係数の増分の検定
anova(res1, res2)
#AICとBICの算出
extractAIC(res1) #ステップ1の回帰モデルのAIC
extractAIC(res2) #ステップ2の回帰モデルのAIC
extractAIC(res1, k=log(300)) #ステップ1の回帰モデルのBIC
extractAIC(res2, k=log(300)) #ステップ2の回帰モデルのBIC

#重回帰分析での交互作用効果の検討

#多重共線性を回避するために中心化
sts$ストレス.c <- sts$ストレス-mean(sts$ストレス) #ストレスの中心化
sts$サポート.c <- sts$サポート-mean(sts$サポート) #サポートの中心化
#中心化前後での相関係数の確認
sts$交互作用　<- sts$ストレス*sts$サポート #中心化をする前の交互作用を作成
sts$交互作用.c <- sts$ストレス.c*sts$サポート.c #中心化をした後の交互作用を作成
cor(sts[, c('ストレス', 'サポート', '交互作用')]) #中心化前の相関行列
cor(sts[, c('ストレス.c', 'サポート.c', '交互作用.c')]) #中心化後の相関行列
#交互作用効果の検討
sts$バーンアウト1.c <- sts$バーンアウト1-mean(sts$バーンアウト1) #バーンアウト1に関しても中心化をする
#全ての要因を中心化して重回帰分析
res3 <- lm(バーンアウト2~バーンアウト1.c+ストレス.c+サポート.c+ストレス.c*サポート.c, data=sts)
summary(res3)
#anovaを用いた決定係数の増分の検定
anova(res2, res3) #p値は有意であることから決定係数rの増分は有意である
#標準偏回帰係数の算出
z.sts <- as.data.frame(scale(sts)) #データの標準化とデータフレーム化
res3.z <- lm(バーンアウト2~バーンアウト1+ストレス+サポート+ストレス*サポート, data=z.sts) #標準偏回帰係数で重回帰分析
summary(res3.z) #中心化した時の偏回帰係数と同じ値になる

#単純傾斜分析

#サポートが多い場合のバーンアウトに対するストレス経験の効果
sts$サポート.h <- sts$サポート.c - sd(sts$サポート.c)
res3.h <- lm(バーンアウト2~バーンアウト1.c+ストレス.c+サポート.h+ストレス.c*サポート.h, data=sts)
summary(res3.h) #ストレスの偏回帰係数の値が低くなるが、そもそも有意性がない
#サポートが少ない場合のバーンアウトに対するストレス経験の効果
sts$サポート.l <- sts$サポート.c + sd(sts$サポート.c)
res3.l <- lm(バーンアウト2~バーンアウト1.c+ストレス.c+サポート.l+ストレス.c*サポート.l, data=sts)
summary(res3.l) #ストレスの偏回帰係数の値が高くなるし、有意性もある
#95％信頼区間の算出
confint(res3)

#重回帰分析における変数選択
bsb <- read.csv('野球.csv')
head(bsb)
#ステップワイズ法による変数選択
library(MASS)
base <- lm(年俸~1, data=bsb) #説明変数を1つも含まない(切片のみを用いた)初期モデルを設定。"~1"は切片のこと
step.res <- stepAIC(base, direction = 'both', scope=list(upper=~打数+安打+打点+本塁打+四球+死球+三振+打率))
summary(step.res) #ステップワイズ法の結果、四球と三振と本塁打と打数を説明変数としたときに、年俸を最もよく予測できることが示された

#章末演習
grades <- read.csv('成績.csv')
head(grades)
grades1 <- lm(テスト成績~性別+通塾有無+有能感, data=grades) #ステップ1の回帰分析
grades2 <- lm(テスト成績~性別+通塾有無+有能感+アスピレーション, data=grades) #ステップ2の回帰分析
anova(tests1, tests2)#関数anovaを用いた決定係数の増分の検定。有意であるとわかる
extractAIC(tests1) #ステップ1のAIC
extractAIC(tests2)　#ステップ2のAIC。こちらの方が比較的良いとわかる
grades$アスピレーション.c <- grades$アスピレーション-mean(grades$アスピレーション) #アスピレーションの中心化
grades$交互作用.c <- grades$通塾有無*grades$アスピレーション.c #中心化した後に交互作用を作成
cor(grades[, c('通塾有無', 'アスピレーション.c', '交互作用.c')]) #中心化後の相関行列
grades3 <- lm(テスト成績~性別+通塾有無+有能感+アスピレーション.c+交互作用.c, data=grades) #交互作用を組み入れて重回帰分析
summary(tests3) #アスピレーション.cの有意性はなし

#アスピレーションが多い場合の成績に対する通塾の効果
grades$アスピレーション.h <- grades$アスピレーション.c - sd(grades$アスピレーション) #アスピレーション.cから1sdをひく
grades3.h <- lm(テスト成績~性別+通塾有無+有能感+アスピレーション.h+通塾有無*アスピレーション.h, data=grades)
summary(tests3.h) #アスピレーションが高くなると通塾有無の効用は増減する
grades$アスピレーション.l <- grades$アスピレーション.c + sd(grades$アスピレーション) #アスピレーション.cに1sdをたす
grades3.l <- lm(テスト成績~性別+通塾有無+有能感+アスピレーション.l+通塾有無*アスピレーション.l, data=grades)
summary(grades3.l) #アスピレーションが低くなると通塾の効用は低減する

library(MASS)
init <- lm(テスト成績~1, data=grades) #切片のみで説明した初期モデル
step.grades <- stepAIC(init, direction = 'both', scope=list(upper=~生徒+性別+通塾有無+有能感+アスピレーション+平日勉強時間+休日勉強時間+ニュース視聴+読書+外遊び)) 
#有能感 + 性別 + 読書 + 通塾有無 + アスピレーション + 外遊び + ニュース視聴が説明変数として最適な組み合わせと判断
summary(step.grades)




























