package com.example.chapter3

import scala.annotation.tailrec

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head:A, tail: MyList[A]) extends MyList[A]

object MyList {

	def apply[A](as: A*): MyList[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail:_*))
	}

	def apply_2[A](as: A*): MyList[A] = {
		@tailrec
		def go[B](acc:MyList[B], bs: B*): MyList[B] = {
			if (bs.isEmpty) Nil
			else go(Cons(bs.last, acc), bs.init:_*)
		}
		if(as.isEmpty) Nil
		else go[A](apply(as.head), as:_*)
	}

	def toString[A](xs: MyList[A]): String = {
		@tailrec
		def go[B](as: MyList[B], acc: String): String = {
			as match{
				case Nil => acc + "]"
				case Cons(b, bs) => go(bs, s"$acc, $b")
			}
		}
		xs match{
			case Nil => "Empty"
			case Cons(b, bs) => go(bs, s"MyList[$b")
		}
	}

	def tail[A](xs: MyList[A]): MyList[A] = {
		xs match{
			case Nil => throw new IndexOutOfBoundsException()
			case Cons(a, as) => as
		}
	}

	def setHead[A](xs: MyList[A], newHead: A): MyList[A] = {
		xs match{
			case Nil => throw new IndexOutOfBoundsException()
			case Cons(a, as) => Cons(newHead, as)
		}
	}

	@tailrec
	def drop[A](xs: MyList[A], num:Long): MyList[A] = {
		if (num <= 0) xs
		else xs match {
			case Nil => throw new IndexOutOfBoundsException()
			case Cons(a, as) => drop(as, num - 1)
		}
	}

	def dropWhile[A](xs: MyList[A])(f: A => Boolean): MyList[A] = {
		xs match {
			case Cons(a, as) if f(a) => dropWhile(as)(f)
			case Cons(a, as) => Cons(a, dropWhile(as)(f))
			case Nil => Nil
		}
	}

	def init[A](xs: MyList[A]): MyList[A] = {
		xs match {
			case Nil => throw new IndexOutOfBoundsException()
			case Cons(a, Nil) => Nil
			case Cons(a, as) => Cons(a, init(as))
		}
	}

	def foldRight[A, B](xs: MyList[A], z:B)(f:(A,B) => B): B = {
		xs match {
			case Nil => z
			case Cons(a, as) => f(a, foldRight(as, z)(f))
		}
	}

	def length[A](xs: MyList[A]): Long = {
		foldRight(xs, 0)((l, r) => r+1)
	}

	@tailrec
	def foldLeft[A, B](z:B, xs: MyList[A])(f:(B,A) => B): B = {
		xs match {
			case Nil => z
			case Cons(a, as) => foldLeft(f(z, a), as)(f)
		}
	}

	def length_2[A](xs: MyList[A]): Long = {
		foldLeft(0, xs)((l, r) => l+1)
	}

	def reverse[A](xs: MyList[A]): MyList[A] = {
		foldLeft(Nil:MyList[A], xs)((l, r) => Cons(r, l))
	}

	def foldRight_2[A, B](xs: MyList[A], z:B)(f:(A,B) => B): B = {
		foldLeft(z, reverse(xs))((b,a) => f(a,b))
	}

	def append[A](l: MyList[A], r: MyList[A]): MyList[A] = {
		foldRight_2(l, r)((a,b) => Cons(a, b))
	}

	def flatten[A](xs: MyList[MyList[A]]): MyList[A] = {
		foldRight_2(xs, Nil:MyList[A])((a,b) => MyList.append(a, b))
	}

	def map[A,B](xs: MyList[A])(f:A => B): MyList[B] = {
		foldRight_2(xs, Nil:MyList[B])((a,b) => Cons(f(a), b))
	}

	def filter[A](xs: MyList[A])(f:A => Boolean): MyList[A] = {
		foldRight_2(xs, Nil:MyList[A])((a,b) => if(f(a)) Cons(a, b) else b)
	}

	def flatMap[A,B](xs: MyList[A])(f:A => MyList[B]): MyList[B] = {
		val temp = map(xs)(f)
		flatten(temp)
	}

	def zipWith[A,B,C](as: MyList[A], bs: MyList[B])(f: (A,B) => C): MyList[C] = {
		(as,bs) match {
			case (Nil, _) => Nil
			case (_, Nil) => Nil
			case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
		}
	}
}