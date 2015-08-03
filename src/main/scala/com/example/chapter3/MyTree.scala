package com.example.chapter3

sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

	def size[A](xs: MyTree[A]): Long = {
		xs match {
			case Branch(l, r) => size(l)+size(r) + 1
			case Leaf(_) => 1
		}
	}

	def depth[A](xs: MyTree[A], v:A): Long = {
		xs match {
			case Branch(l, r) => Math.max(depth(l, v), depth(r, v)) + 1
			case Leaf(value) if v == value => 1
		}
	}

	def depth[A](xs: MyTree[A]): Long = {
		xs match {
			case Branch(l, r) => Math.max(depth(l), depth(r)) + 1
			case Leaf(_)  => 1
		}
	}

	def map[A,B](t: MyTree[A])(f: A => B): MyTree[B] = {
		t match {
			case Leaf(a) => Leaf(f(a))
			case Branch(l,r) => Branch(map(l)(f), map(r)(f))
		}
	}

	def fold[A,B](t: MyTree[A])(f: A => B)(g: (B,B) => B): B = {
		t match {
			case Leaf(a) => f(a)
			case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
		}
	}

	def map_2[A,B](t: MyTree[A])(f: A => B): MyTree[B] = {
		fold(t)(a => Leaf(f(a)): MyTree[B])((l, r) => Branch(l,r))
	}

}