package com.example.chapter4

import org.scalatest.FunSuite

class MyEither$Test extends FunSuite {

	test("testTraverse") {
		val list = List(1,2,3)
		val f = {a:Int => Right(a)}
		val ss = MyEither.traverse(list)(f)
		println(ss)
	}

}
