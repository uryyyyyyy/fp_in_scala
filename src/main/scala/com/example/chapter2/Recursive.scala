package com.example.chapter2

import scala.annotation.tailrec

object Recursive {

	def calcFibs(m:Long):Long = {
		@tailrec
		def go(n:Long, acc_1:Long, acc_2:Long):Long = {
			if (n <= 1) acc_1
			else go(n-1, acc_2, acc_1 + acc_2)
		}
		go(m, 0, 1)
	}

	def calcFibs_2(m:Long):Long = {
		if(m <= 0) 0
		else if (m == 1) 1
		else calcFibs(m-1) + calcFibs(m-2)
	}

	def isSorted[A](xs:Array[A], f:(A, A) => Boolean):Boolean = {
		@tailrec
		def go(bs:Array[A], a:A, b:Boolean):Boolean = {
			if (!b) return false

			if (bs.length == 0) b
			else go(bs.tail, bs.head, f(a, bs.head))
		}

		if (xs.length == 0) true
		else go(xs.tail, xs.head, true)
	}

	def isSorted_2[A](xs:Array[A], f:(A, A) => Boolean):Boolean = {
		@tailrec
		def go(n:Int):Boolean = {
			if (n<=1) true
			else if (!f(xs(n-2), xs(n-1))) false
			else go(n-1)
		}

		go(xs.length)
	}

	def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
		a:A => b:B => f(a, b)
	}

	def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
		(a:A, b:B)  => f(a)(b)
	}

	def compose[A,B,C](f: B => C, g:A => B): A => C = {
		a:A => f(g(a))
	}

}
