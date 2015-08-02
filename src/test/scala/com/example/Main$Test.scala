package com.example

import org.scalatest.FunSuite


class Main$Test extends FunSuite {

	test("testCalc") {
		assert(Main.calc(10) == 100)
	}

	test("testCalc2") {
		assert(Main.calc(8) == 100)
	}

}
