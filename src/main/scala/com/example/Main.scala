package com.example

object Main {

	def main(args: Array[String]): Unit ={
		val i = calc(10)
		println(i)
	}

	def calc(i: Int): Int ={
		i * i
	}

}