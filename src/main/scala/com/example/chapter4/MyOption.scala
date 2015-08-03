package com.example.chapter4

sealed trait MyOption[+A]{
	def map[B](f: A => B):MyOption[B] = {
		this match {
			case None => None
			case Some(a) => Some(f(a))
		}
	}
	def flatMap[B](f: A => MyOption[B]):MyOption[B] = {
		this match {
			case None => None
			case Some(a) => f(a)
		}
	}
	def getOrElse[B>:A](default: => B):B = {
		this match {
			case None => default
			case Some(a) => a
		}
	}
	def orElse[B>:A](default: => MyOption[B]):MyOption[B] = {
		this match {
			case None => default
			case Some(a) => Some(a)
		}
	}
	def filter(f: A => Boolean):MyOption[A] = {
		this match {
			case None => None
			case Some(a) if f(a) => Some(a)
			case Some(a) => None
		}
	}
}
case class Some[A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {

	def mean(xs: Seq[Double]): MyOption[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	def variance(xs: Seq[Double]): MyOption[Double] = {
		for{
			m <- mean(xs)
			res <- mean(xs.map(x => math.pow(x - m, 2)))
		} yield res
	}

	def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f:(A,B)=>C): MyOption[C] = {
		for{
			a_ <- a
			b_ <- b
		} yield f(a_, b_)
	}

	def sequence[A](xs: List[MyOption[A]]): MyOption[List[A]] = {
		xs match {
			case Nil => Some(Nil)
			case a :: as => for{
				a_ <- a
				as_ <- sequence(as)
			}yield a_ :: as_
		}
	}

}