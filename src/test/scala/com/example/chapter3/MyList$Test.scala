package com.example.chapter3

import java.time.{Duration, ZonedDateTime}

import org.scalatest.FunSuite

class MyList$Test extends FunSuite {

	test("testMatchClause") {
		val x = MyList(1,2,3,4,5) match{
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
			case _ => 101
		}
		println(x)
	}

	test("testApply") {
		val start = ZonedDateTime.now()
		val r = MyList.apply(1 to 10000000)
		val end = ZonedDateTime.now()
		val duration = Duration.between(start, end)
		println(duration.getNano)
	}

	test("testApply_2") {
		val start = ZonedDateTime.now()
		val r = MyList.apply_2(1 to 10000000)
		val end = ZonedDateTime.now()
		val duration = Duration.between(start, end)
		println(duration.getNano)
	}

	test("testToString") {
		val r = Nil
		assert(MyList.toString(r) == "Empty")
		val r2 = MyList(1,2,3,4,5)
		assert(MyList.toString(r2) == "MyList[1, 2, 3, 4, 5]")
		val r3 = MyList("a", "b", "c", "d")
		assert(MyList.toString(r3) == "MyList[a, b, c, d]")
	}

	test("testTail") {
		val xs = MyList(1,2,3,4,5)
		val tail = MyList.tail(xs)
		assert(tail == MyList(2,3,4,5))
		intercept[IndexOutOfBoundsException] {
			MyList.tail(Nil)
		}
	}

	test("testSetHead") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.setHead(xs, 100)
		assert(r == MyList(100,2,3,4,5))
		intercept[IndexOutOfBoundsException] {
			MyList.setHead(Nil, 100)
		}
	}

	test("testDrop") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.drop(xs, 2)
		assert(r == MyList(3,4,5))
		val r2 = MyList.drop(xs, 5)
		assert(r2 == Nil)
		intercept[IndexOutOfBoundsException] {
			MyList.drop(xs, 6)
		}
	}

	test("testDropWhile") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.dropWhile(xs, (i:Int) => (i % 2) == 0)
		assert(r == MyList(1,3,5))
		val r2 = MyList.dropWhile(xs, (i:Int) => i < 100)
		assert(r2 == Nil)
		val r3 = MyList.dropWhile(Nil, (i:Int) => i < 100)
		assert(r3 == Nil)
	}

}
