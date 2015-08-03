
代数的データ型
直和・直積

Listを作る。

まずはListの定義から。（単方向リスト）

０個以上の要素を持つもの
要素間は直列につながっている。

１つの要素を持つListは、「一つの要素」と「空List」に分解できる。
２つの要素は「１つの要素」と「一つの要素を持つList」に分解できる。
関数型の話なので、それぞれの要素、Listは不変である。
（１つの要素を持つListと、ひとつの要素は同じではない？）

まずはこんな感じ。

```
sealed trait List[A]
case object Nil[A] extends List[Nothing]
case class Cons[A](head:A, tail List[A]) extends List[A]
```

Nil(空リスト、あるいはnullポインタ的なもの)は要素を持たないので、
どんな型のListであっても同値なはず。（ここはJavaと違うところ)
そこでcase objectを使い、具象型として基底クラスであるNothingを使う。
Consは一つ以上の要素を持っている場合のインスタンス。
これはcase classを使い、headをもたせる。
この２つでどんなListにも対応できる。例えば
List(1,2,3)は Cons(1, Cons(2, Cons(3, Nil)))
空リストは Nil
といった感じ。

さて、コレがうまく行くかと思いきや、Cons(1, Cons(2, Cons(3, Nil)))はエラーになる。
ConsはList[Int]だが、NilはList[Nothing]だから型が合わない。
Javaではここが限界だったが、Scalaでは共変(covariant)を使って解決できる。
List[+A]
コレを使うとAの部分型BのList[B]をList[A]の部分型として扱うことができる。
つまり
val list:List[A] = Nil
ができる。
(NilはListなので、val a:A = Nilとはならないため型チェックに問題はない。)

Companion objectに総称型をつけてもいいけど、どうせメソッド側でつけなくてはいけないので不要
object List {
	def apply[A](as: A*): List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*)
	}
}

applyを末尾再帰にしてみよう思ったけど、not enough arguments for method toArray って怒られた。
原因はおそらくtoArrayの時に
よくわからないし、そもそもArrayに依存するとややこしくなるのでやめた。。。

	def apply_2[A](as: A*): MyList[A] = {
		@tailrec
		def go[B](acc:MyList[B], bs: Array[B]): MyList[B] = {
			if (bs.isEmpty) Nil
			else go(Cons(bs.last, acc), bs.init)
		}
		if(as.isEmpty) Nil
		else go(Cons(as.head, Nil), as.toArray)
	}

3.5

末尾再帰すると、Listの末尾に追加していく形になりn^2/2の計算量がかかりかねないのでダメ。

3.6

単方向リストだから

dropWhile 引数を分割することで型推論が働く。

3.7

foldRight四季を全て展開してから計算する。

3.8

畳み込みではなくmap処理の形になる。

3.9

3.21
意味ないのでやらない。

3.22


3.29
型をLeafからMyTreeに明示的に戻さないといけない。