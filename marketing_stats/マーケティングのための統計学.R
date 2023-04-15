# パッケージのインストール
install.packages("dplyr")
install.packages("tidyr")
library("dplyr")
library("tidyverse")

# IDデータとPOSデータに関して分析
## ID_POSデータの準備
df1 <- read.csv("ID_POSデータ(POSデータ).csv")
df2 <- read.csv("ID_POSデータ(IDデータ).csv")

## データ型を確認
sapply(df1, class)

## 単純集計表(IDデータ)
table_df2 <- as.data.frame(t(count(df2, 年代)))
colnames(table_df2) <- c(1,2,3,4,5,6,7,8,9,10)
class(table_df2)
table_df2

## ユニーク顧客数(1~15, 16~30日)
df1_u15_unique <- df1 %>%
  filter(日 <= 15) %>%
  summarise(ユニーク顧客数 = n_distinct(顧客ID))

df1_1630_unique <- df1 %>%
  filter(between(日, 16, 30)) %>%
  summarise(ユニーク顧客数 = n_distinct(顧客ID))


df1_u15 <- df1 %>%
  filter(日 <= 15) %>%
  group_by(顧客ID) %>%
  summarise( a = sum(税抜価格), b = mean(税抜単価*個数))

df1_u15 <- df1 %>%
  filter(日 <= 15) %>%
  summarise("売上" = sum(税抜価格), 
            "ユニーク顧客数" = n_distinct(顧客ID), 
            "1人あたりの購買金額" = round(sum(税抜価格)/n_distinct(顧客ID)),
            )
## 来店頻度の計算
df1_u15x <- df1 %>%
  filter(日 <= 15) %>%
  group_by(顧客ID) %>%
  summarise("来店頻度" = n_distinct(レシートNo))
来店頻度 <- mean(df1_u15x$来店頻度) # 全体の平均を出す

## df1_u15と結合
cbind(df1_u15, 来店頻度)

#あ








