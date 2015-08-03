package com.example.chapter4

import org.scalatest.FunSuite

class MyOption$Test extends FunSuite {

	test("testMean") {
		val r = MyOption.mean(Seq(1.4,2.4,5.2))
		assert(r == Some(3.0))
		val r2 = MyOption.mean(Seq())
		assert(r2 == None)
	}

	test("testVariance") {
		val r = MyOption.variance(Seq(1.4,2.4,5.2))
		assert(r == Some(2.5866666666666673))
		val r2 = MyOption.variance(Seq())
		assert(r2 == None)
	}

	test("testSequence") {
		val r = MyOption.sequence(List(Some(1), Some(2), Some(3)))
		assert(r == Some(List(1,2,3)))
		val r2 = MyOption.sequence(List(Some(1), None, Some(3)))
		assert(r2 == None)
		val r3 = MyOption.sequence(List())
		assert(r3 == Some(Nil))
	}


}
