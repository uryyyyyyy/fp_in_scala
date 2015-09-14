package com.example.chapter4

sealed trait MyEither[+E,+A] {
	def map[B](f: A => B): MyEither[E, B] = {
		this match {
			case Right(a) => Right(f(a))
			case Left(e) => Left(e)
		}
	}

	def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
		this match {
			case Left(e) => Left(e)
			case Right(a) => f(a)
		}

	def orElse[EE >: E, AA >: A](b: => MyEither[EE, AA]): MyEither[EE, AA] =
		this match {
			case Left(_) => b
			case Right(a) => Right(a)
		}

	def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
		for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](value: E) extends MyEither[E,Nothing]
case class Right[+A](value: A) extends MyEither[Nothing,A]

object MyEither{
	def traverse[E,A,B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
		es match {
			case Nil => Right(Nil)
			case h::t => for{
				a <- f(h)
				ax <- traverse(t)(f)
			}yield a :: ax
		}

	def sequence[E,A](es: List[MyEither[E,A]]): MyEither[E,List[A]] =
		traverse(es)(x => x)
}