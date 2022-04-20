#データの読み込み，データフレームの確認
ks <- read.csv("9都市の気象.csv", colClasses = c(rep("factor", 6), rep("numeric", 3))) #データの読み込み
head(ks)

#カテゴリカル変数の水準の確認
str(ks)

#カテゴリカル変数の水準の設定
ks$都市 <- factor(ks$都市, levels = c("札幌", "仙台", "東京", "新潟", "名古屋", "大阪", "広島", "福岡", "那覇"))
ks$年 <- factor(ks$年, levels = as.character(2012:2016))
ks$月 <- factor(ks$月, levels = as.character(1:12))
ks$日 <- factor(ks$日, levels = as.character(1:31))
ks$季節 <- factor(ks$季節, levels = c("春", "夏", "秋", "冬"))
ks$天気 <- factor(ks$天気, levels = c("快晴", "晴", "薄曇", "曇", "煙霧", "霧", "霧雨", "雨", "みぞれ", "雪", "あられ", "雷"))

#棒グラフの描画
library(ggplot2)
P1_0 <- ggplot(data = ks, mapping = aes(x = 天気))
(P1_1 <- P1_0 + geom_bar())

#棒に対応する値を得る
ggplot_build(P1_1)$data[[1]]

#棒グラフの描画（都市で層化）
(P1_2 <- P1_1 + facet_wrap( ‾ 都市, ncol = 3))

#棒グラフの描画（都市と季節で層化）
(P1_3 <- P1_1 + facet_grid(都市 ‾ 季節))

#ヒストグラムの描画
P2_0 <- ggplot(data = ks,mapping = aes(x = 風速))
(P2_1 <- P2_0 + geom_histogram(breaks = seq(0, 25, 0.5)))

#ヒストグラムの描画（都市で層化）
(P2_2 <- P2_1 + facet_wrap( ‾ 都市, ncol = 3))

#分布の平均と不偏分散に基づく標準偏差（都市で層化）
library(dplyr)
wind_MeanSD_city <- summarise(group_by(ks, 都市), Mean = mean(風速), SD = sd(風速))
print(wind_MeanSD_city)

#ヒストグラムの描画および分布の平均と不偏分散に基づく標準偏差（都市と季節で層化）
(P2_3 <- P2_1 + facet_grid(都市 ‾ 季節))
wind_MeanSD_cityseason <- summarise(group_by(ks, 都市, 季節), Mean = mean(風速), SD = sd(風速))
print(wind_MeanSD_cityseason, n = 50)

#雨だけのデータフレームの作成
ks_rain <- filter(ks, 天気 == "雨")

#1月から12月にかけての雨の日数の折れ線グラフの描画
P3_0 <- ggplot(data = ks_rain, mapping = aes(x = 月))
P3_1 <- P3_0 + geom_line(aes(group = 1), stat = "count")
(P3_2 <- P3_1 + geom_point(aes(group = 1), stat = "count"))

#1月から12月にかけての雨の日数の折れ線グラフの描画（都市で層化）
(P3_3 <- P3_2 + facet_grid(. ‾ 都市))

#1月から12月にかけての平均降水量の折れ線グラフの描画
P3_3 <- ggplot(data = ks, mapping = aes(x = 月, y = 降水量))
P3_4 <- P3_3 + stat_summary(aes(group = 1), fun.y = mean, geom = "line")
(P3_5 <- P3_4 + stat_summary(aes(group = 1), fun.y = mean, geom = "point"))

#1月から12月にかけての平均降水量の折れ線グラフの描画（都市で層化）
(P3_6 <- P3_5 + facet_grid(. ‾ 都市))

#2年分の気温データに整形
library(tidyr)
fixname <- function(x){ #変数名文字列のエンコーディングを元に戻す関数
	names(x)<-enc2native(names(x)) #データオブジェクトの変数名のエンコードし直し
	return(x)
}
ks_temp <- 
	ks %>% 
	filter(年 == "2014" | 年 == "2015") %>% #条件に合致する対象の抜き出し
	select(都市, 月, 日, 年, 季節, 気温) %>% #特定の変数の取り出し
	spread(key = 年, value = 気温, sep = "") %>% #ワイドフォーマットへの変更
	rename(気温2014 = 年2014, 気温2015 = 年2015) %>% #変数名の付け直し
	fixname() #データオブジェクトに対して関数group_byを適用するときのエラーに対処するために必要（Windows OS）

#散布図の描画
P4_0 <- ggplot(data = ks_temp, mapping = aes(x = 気温2014, y = 気温2015))
(P4_1 <- P4_0 + geom_point())

#散布図の描画（都市で層化）
(P4_2 <- P4_1 + facet_wrap(~都市))

#散布図の描画（都市と季節で層化）
(P4_3 <- P4_1 + facet_grid(都市~季節))

#相関係数（都市と季節で層化）
ks_temp1415 <- summarize(group_by(ks_temp, 都市, 季節), Cor = cor(気温2014, 気温2015))
print(ks_temp1415, n = 50)

#塗りつぶしへの変数のマッピング
P5_0 <- ggplot(data = ks, mapping = aes(x = 天気))
(P5_1 <- P5_0 + geom_bar(aes(fill = 季節), position = "stack"))
# (P5_1 <- P5_0 + geom_bar(aes(fill = 季節), position = position_stack(reverse = TRUE))) #逆順
# (P5_1 <- P5_0 + geom_bar(aes(fill = 季節, group = factor(季節, levels = c("秋", "冬", "春", "夏"))))) #任意の順番
(P5_2 <- P5_0 + geom_bar(aes(fill = 季節), position = "fill"))
(P5_3 <- P5_0 + geom_bar(aes(fill = 季節), position = "dodge"))
(P5_4 <- P5_0 + geom_bar(aes(fill = 季節), position = "identity"))

#棒の塗りつぶし色の変更（マッピングではない）
 P5_0 <- ggplot(data = ks, mapping = aes(x = 天気))
(P5_1 <- P5_0 + geom_bar(fill = "red"))

#線色と線種のマッピング
P6_0 <- ggplot(data = ks_rain, mapping = aes(x = 月))
(P6_1 <- P6_0 + geom_line(aes(group = 都市, color = 都市), stat="count") + geom_point(aes(group = 都市, color = 都市), stat = "count"))	
(P6_2 <- P6_0 + geom_line(aes(group = 都市, color = 都市, linetype = 都市), stat = "count") + geom_point(aes(group = 都市, color = 都市), stat = "count"))	

#点色と点種のマッピング
P7_0 <- ggplot(data = ks_temp,mapping = aes(x = 気温2014, y = 気温2015))
(P7_1 <- P7_0 + geom_point(aes(color = 季節, shape = 季節)))

#軸に関する設定
P8_0 <- ggplot(data = ks, mapping = aes(x = 天気)) + geom_bar(aes(fill = 季節))
P8_1 <- P8_0 + scale_y_continuous(limits = c(0,6000), breaks = seq(0, 6000, 1000))
P8_2 <- P8_1 + labs(x = "天気の種類", y = "度数")
(P8_3 <- P8_2 + theme(axis.text.x = element_text(size = 15), axis.title.y = element_text(size = 20)))

#凡例に関する設定
keys <- c("春", "夏", "秋", "冬")
mycolor <- c("plum", "tomato", "wheat", "lemonchiffon"); names(mycolor) <- keys
P8_4 <- P8_3 + scale_fill_manual(values = mycolor)
# P8_4<-P8_3 + scale_fill_manual(values = mycolor,limits = rev(keys)) #凡例項目逆順
P8_5 <- P8_4 + theme(legend.position = "bottom")
P8_6 <- P8_5 + labs(fill = "四季")
(P8_7 <- P8_6 + guides(fill = guide_legend(nrow = 1, byrow = TRUE)))
	
#集計データからの棒グラフ
ks_bar <- ks %>% 
	group_by(季節, 天気) %>% 
	summarise(度数 = n()) %>% 
	complete(季節, 天気, fill = list(度数 = 0)) %>% 
	as.data.frame() #結果を見やすいようにデータフレーム化
(P9_0 <- ggplot(ks_bar, aes(x = 天気, y = 度数)) + geom_bar(aes(fill = 季節), stat = "identity"))

#集計データからの折れ線グラフ
ks_line <- ks %>% 
	group_by(月, 都市) %>% 
	summarise(平均降水量 = mean(降水量)) %>% 
	as.data.frame()
P9_1 <- ggplot(ks_line, aes(x = 月, y = 平均降水量))
P9_2 <- P9_1 + geom_line(aes(group = 都市, color = 都市, linetype = 都市), stat = "identity")
(P9_3 <- P9_2 + geom_point(aes(group = 都市, color = 都市), stat = "identity"))
	
#文字情報を付加する（geom_text）
P10_1 <- ggplot(data = ks,mapping = aes(x = 天気)) + geom_bar()
(P10_2 <- P10_1 + geom_text(aes(label = ..count..), stat = "count", vjust = -0.5))

#分布の概形を調べる（geom_density，geom_vline）
ks_mean_temp <- ks %>% 
	group_by(季節,都市) %>% 
	summarise(平均気温 = mean(気温)) %>% 
	as.data.frame()
P10_3 <- ggplot(data = ks, mapping = aes(x = 気温)) + geom_density(aes(linetype = 季節, color = 季節))
P10_4 <- P10_3 + geom_vline(data = ks_mean_temp, aes(xintercept = 平均気温, color = 季節), linetype = "twodash") #linetypeの他の種類は"solid","longdash","dotted","dotdash","dashed","blank"
(P10_5 <- P10_4 + facet_wrap( ‾ 都市))

#データのばらつきを詳細に調べる（geom_jitter）
P10_6 <- ggplot(data = ks,mapping = aes(x = 都市, y = 風速)) + geom_jitter(aes(color = 季節, group = 季節), position = position_jitterdodge(dodge.width = 0.6), alpha = 1 / 5) #position_jitterdodge(dodge.width = 0.6)で打点のばらつき幅を指定
(P10_7 <- P10_6 + stat_summary(aes(x = 都市, y = 風速, group = 季節), color = "white", fun.y = median, geom = "point", shape = 4, position = position_dodge(width = 0.6))) #position_dodge(width = 0.6)でdodgeの幅を指定


#演習と解答
#問1解答
rtks <- read.csv("6都市の気象.csv", colClasses = c(rep("factor", 6), rep("numeric", 3)))
rtks$都市 <- factor(rtks$都市, levels = c("鹿児島", "高松", "金沢", "長野", "横浜", "青森"))
rtks$月  <- factor(rtks$月, levels = as.character(1:12))
rtks$日  <- factor(rtks$日, levels = as.character(1:31))
rtks$季節 <- factor(rtks$季節, levels = c("春", "夏", "秋", "冬"))
rtks$午前天気 <- factor(rtks$午前天気, levels = c("快晴", "晴", "薄曇", "曇", "煙霧", "霧", "霧雨", "雨", "みぞれ", "雪", "あられ", "雷"))
rtks$午後天気 <- factor(rtks$午後天気, levels = c("快晴", "晴", "薄曇", "曇", "煙霧", "霧", "霧雨", "雨", "みぞれ", "雪", "あられ", "雷"))

#問2解答
library(ggplot2)
ggplot(data = rtks, aes(x = 午後天気)) + geom_bar()

#問3解答
ggplot(data = rtks, aes(x = 季節, group = 1)) + stat_summary(aes(y = 気温), fun.y = mean, geom = "line") + stat_summary(aes(y = 気温), fun.y = mean, geom = "point")

#問4解答
library(dplyr)
rtks2 <- rtks %>% 
	group_by(季節) %>% 
	summarise(平均気温 = mean(気温)) %>% 
	as.data.frame()

#問5解答
ggplot(data = rtks2, aes(x = 季節, y = 平均気温, group = 1)) + geom_line(stat = "identity") + geom_point(stat = "identity")

#問6解答
ggplot(data = rtks, aes(x = 午後天気)) + geom_bar(aes(fill = 季節)) + facet_grid( ‾ 都市)
