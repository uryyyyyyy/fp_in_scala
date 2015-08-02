package com.example.chapter2

import java.time.{Duration, ZonedDateTime}

import org.scalatest.FunSuite


class Recursive$Test extends FunSuite {

	test("testCalcFibs") {
		val r = Recursive.calcFibs(5)
		assert(r == 3)

		val r2 = Recursive.calcFibs(10)
		assert(r2 == 34)
	}

	test("testCalcFibs_tailRec") {
		val start = ZonedDateTime.now()
		val r = Recursive.calcFibs(2000000000)
		val end = ZonedDateTime.now()
		val duration = Duration.between(start, end)
		println(duration.getNano / 1000)
	}

	test("testCalcFibs_2") {
		val r = Recursive.calcFibs_2(5)
		assert(r == 3)

		val r2 = Recursive.calcFibs_2(10)
		assert(r2 == 34)
	}

	test("testCalcFibs_2_tailRec") {
		val start = ZonedDateTime.now()
		val r = Recursive.calcFibs_2(2000000000)
		val end = ZonedDateTime.now()
		val duration = Duration.between(start, end)
		println(duration.getNano / 1000)
	}

	test("testIsSorted") {
		val r = Recursive.isSorted(Array(1, 2, 10, 100), (m:Int, n:Int) => m < n)
		assert(r)

		val r2 = Recursive.isSorted(Array(1, 2, 100, 12), (m:Int, n:Int) => m < n)
		assert(!r2)
	}

	test("testIsSorted_2") {
		val r = Recursive.isSorted_2(Array(1, 2, 10, 100), (m:Int, n:Int) => m < n)
		assert(r)

		val r2 = Recursive.isSorted_2(Array(1, 2, 100, 12), (m:Int, n:Int) => m < n)
		assert(!r2)
	}

}
