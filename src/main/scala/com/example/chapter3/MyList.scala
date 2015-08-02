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

	def dropWhile[A](xs: MyList[A], f: A => Boolean): MyList[A] = {
		xs match {
			case Cons(a, as) if f(a) => dropWhile(as, f)
			case Cons(a, as) => Cons(a, dropWhile(as, f))
			case Nil => Nil
		}
	}

}