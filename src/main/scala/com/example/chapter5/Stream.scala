package com.example.chapter5


trait MyStream[+A]{
	import MyStream._

	def toList: List[A] = {
		@annotation.tailrec
		def go(s: MyStream[A], acc: List[A]): List[A] = s match {
			case Cons(h,t) => go(t, h :: acc)
			case _ => acc
		}
		go(this, List()).reverse
	}

	def take(n: Int): MyStream[A] = this match {
		case Cons(h, t) if n > 1 => cons(h, t.take(n - 1))
		case Cons(h, _) if n == 1 => cons(h, empty)
		case _ => empty
	}

	@annotation.tailrec
	final def drop(n: Int): MyStream[A] = this match {
		case Cons(_, t) if n > 0 => t.drop(n - 1)
		case _ => this
	}

	def takeWhile(f: A => Boolean): MyStream[A] = this match {
		case Cons(h,t) if f(h) => cons(h, t.takeWhile(f))
		case _ => empty
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B = {
		@annotation.tailrec
		def go(s: MyStream[A], acc: B): B = s match {
			case Cons(h, t) => go(t, f(h, acc))
			case _ => acc
		}
		go(this, z)
	}

	def exists(p: A => Boolean): Boolean =
		foldRight(false)((a, b) => p(a) || b)

	def forAll(f: A => Boolean): Boolean =
		foldRight(true)((a,b) => f(a) && b)

	def takeWhile_1(f: A => Boolean): MyStream[A] =
		foldRight(empty[A])((h,t) => if(f(h)) cons(h,t) else empty)

	def headOption: Option[A] =
		this match {
			case Cons(h,_) => Some(h)
			case _ => None
		}

	def map[B](f: A => B): MyStream[B] =
		foldRight(empty[B])((h,t) => cons(f(h), t))

	def filter(f: A => Boolean): MyStream[A] =
		foldRight(empty[A])((h,t) => if (f(h)) cons(h, t) else t)

	def append[B>:A](s: => MyStream[B]): MyStream[B] =
		foldRight(s)((h,t) => cons(h,t))

	def flatMap[B](f: A => MyStream[B]): MyStream[B] =
		foldRight(empty[B])((h,t) => f(h) append t)

	def mapViaUnfold[B](f: A => B): MyStream[B] =
		unfold(this) {
			case Cons(h,t) => Some((f(h), t))
			case _ => None
		}

	def takeViaUnfold(n: Int): MyStream[A] =
		unfold((this,n)) {
			case (Cons(h,t), 1) => Some((h, (empty, 0)))
			case (Cons(h,t), n) if n > 1 => Some((h, (t, n-1)))
			case _ => None
		}

	def takeWhileViaUnfold(f: A => Boolean): MyStream[A] =
		unfold(this) {
			case Cons(h,t) if f(h) => Some((h, t))
			case _ => None
		}

	def zipWith[B,C](s2: MyStream[B])(f: (A,B) => C): MyStream[C] =
		unfold((this, s2)) {
			case (Cons(h1,t1), Cons(h2,t2)) =>
				Some((f(h1, h2), (t1, t2)))
			case _ => None
		}

	def zip[B](s2: MyStream[B]): MyStream[(A,B)] =
		zipWith(s2)((_,_))


	def zipAll[B](s2: MyStream[B]): MyStream[(Option[A],Option[B])] =
		zipWithAll(s2)((_,_))

	def zipWithAll[B, C](s2: MyStream[B])(f: (Option[A], Option[B]) => C): MyStream[C] =
		unfold((this, s2)) {
			case (Empty, Empty) => None
			case (Cons(h, t), Empty) => Some(f(Some(h), Option.empty[B]) -> (t, empty[B]))
			case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h)) -> (empty[A] -> t))
			case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1), Some(h2)) -> (t1 -> t2))
		}

	def startsWith[B>:A](s: MyStream[B]): Boolean =
		zipAll(s).takeWhile(_._2.isDefined) forAll {
			case (h,h2) => h == h2
		}

	/*
	The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
	*/
	def tails: MyStream[MyStream[A]] =
		unfold(this) {
			case Empty => None
			case s => Some((s, s drop 1))
		} append MyStream(empty)

	def hasSubsequence[B>:A](s: MyStream[B]): Boolean =
		tails exists (_ startsWith s)

	/*
	The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
	The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
	*/
	def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] =
		foldRight((z, MyStream(z)))((a, p0) => {
			// p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
			lazy val p1 = p0
			val b2 = f(a, p1._1)
			(b2, cons(b2, p1._2))
		})._2

	@annotation.tailrec
	final def find(f: A => Boolean): Option[A] = this match {
		case Empty => None
		case Cons(h, t) => if (f(h)) Some(h) else t.find(f)
	}
}

case object Empty extends MyStream[Nothing]
case class Cons[+A](h:  => A, t:  => MyStream[A]) extends MyStream[A]

object MyStream {
	def cons[A](x: => A, xs: => MyStream[A]): MyStream[A] = {
		lazy val head = x
		lazy val tail = xs
		Cons(head, tail)
	}

	def empty[A]: MyStream[A] = Empty

	def apply[A](as: A*): MyStream[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] =
		f(z) match {
			case Some((h,s)) => cons(h, unfold(s)(f))
			case None => empty
		}
}