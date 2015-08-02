
とりあえず試してみて利点を探る。

２．１

calcFibs普通に書いたパターンとtailrecパターン

Recursive.calcFibs


tailrec
わかりにくいから嫌い
パターンとしては、順番に積み上げていくイメージ
普通は求めたいｎから順番に小さく崩していくのだが、
一番小さいｎを先に持ってきてから順番に計算結果を積み上げていくイメージ
今回のケースならgo(m,0,1)の部分。
例にあったケースで言うとgo(n,1)の部分。


２．２

Recursive.isSorted


2.3

Recursive.curry

2.4

Recursive.uncurry

2.5

Recursive.compose
