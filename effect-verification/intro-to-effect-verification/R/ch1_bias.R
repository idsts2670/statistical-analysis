# (1) パッケージをインストールする（初回のみ）
install.packages("tidyverse")

# (2) ライブラリの読み出し
library("tidyverse")

# (3) RCTデータの読み込みと確認
email_data <- read_csv("http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv")
# 変数一覧の確認
ls(male_df)

# (4) データの準備
## 女性向けメールが配信されたデータを削除したデータを作成
male_df <- email_data %>%
  # 女性向けメールが配信されたデータを削除
  filter(segment != "Womens E-Mail") %>%
  # 介入を表すtreatment変数を追加
  mutate(treatment = ifelse(segment == "Mens E-Mail", 1, 0))

# （5）集計による比較(RCTデータのため、セレクションバイアスなしの差分)
## group_byとsummariseを使って集計
summary_by_segment <- male_df %>%
  # データのグループ化
  group_by(treatment) %>%
  # グループごとのconversionの平均
  summarise(conversion_rate = mean(conversion),
            # グループごとのspend(売上金額)の平均
            spend_mean = mean(spend),
            # グループごとのデータ数
            count = n())

#（6）t検定を行う
## (a)男性向けメールが配信されたグループの購買データを得る
mens_mail <- male_df %>%
  # treatment == 1の値のみ受け取る
  filter(treatment == 1) %>%
  # spendをベクトルとして受け取る
  pull(spend)

## (b)メールが配信されなかったグループの購買データを得る
no_mail <- male_df %>%
  filter(treatment == 0) %>%
  pull(spend)
no_mail

## (a)(b)の平均に対して有意差検定を実行する(var.equal = Tで2つのデータが同じvar(分散)を持っていると仮定する)
rct_ttest <- t.test(mens_mail, no_mail, var.equal = T)
## RCTを行なっているデータであるため、この差はメールの配信のみによって起こるものと考えられ、その差も統計的に有意であるということも確認できた
rct_ttest

#（7）セレクションバイアスのあるデータの作成
## 再現性確保のため、乱数seedを固定
set.seed(1)
## 条件に反応するサンプル量を半分にする
obs_rate_c <- 0.5
obs_rate_t <- 0.5

## バイアスのある作成
biased_data <- male_df %>%
  mutate(obs_rate_c = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), obs_rate_c, 1),
        obs_rate_t = ifelse((history > 300) | (recency < 6) | (channel == "Multichannel"), 1, obs_rate_t),
        # runifとは離散型一様分布の乱数を発生させる関数
        random_number = runif(n = NROW(male_df))) %>%
  # メールが配信されていないグループでは上記3つの条件のどれかに該当するデータを半分削除
  # メールが配信されたグループでは条件に該当していないデータを半分削除
  filter((treatment == 0 & random_number < obs_rate_c) |
            (treatment == 1 & random_number < obs_rate_t))

# (8)セレクションバイアスのあるデータで平均を比較
## group_byとsummariseを使って集計
summary_by_segment_biased <- biased_data %>%
  # メール配信の有無でグループ分け
  group_by(treatment) %>%
  summarise(conversion_rate = mean(conversion),
            speed_mean = mean(spend),
            count = n())
# （9）t検定を行う
## (a)男性向けメールが配信されたグループの購買データを得る
mens_mail_biased <- biased_data %>%
  filter(treatment == 1) %>%
  pull(spend)

## (b)メールが配信されなかったグループの購買データを得る
no_mail_biased <- biased_data %>%
  filter(treatment == 0) %>%
  pull(spend)

## (a)(b)の平均の差に対して有意差検定を実行
rct_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = T)
