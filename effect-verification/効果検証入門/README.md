# cibook

## 効果検証入門

技術評論社発行の書籍のサンプルコードです。

https://gihyo.jp/book/2020/978-4-297-11117-5

データ元

http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv

## 用語紹介
セレクションバイアス
> 比較しているグループの潜在的な傾向(その他の要素)が違うことによって発生するバイアス。eg:メール配信の分析

RCT(無作為比較試験, Randomized Controlled Trial)
> 施策の割り当てをランダムにし、介入が行われたグループと介入が行われていないグループの比較を可能にする。"因果推論の根本問題"を限りなく解消する。eg:ABテスト

ポテンシャルアウトカムフレームワーク(Potential Outcome Framework)
> 介入が行われた結果γ1と行われなかった結果γ2があると考え、その差に介入の本当の効果があるという考え

ポテンシャルアウトカム(Potential Outcome)
> 実際には観測されない側の結果のこと

<!-- インライン表示 -->
$ τ = E\lbrack(γ1)\rbrack - E\lbrack(γ0)\rbrack


