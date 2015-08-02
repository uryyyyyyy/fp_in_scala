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
		val r = MyList.dropWhile(xs)(i => (i % 2) == 0)
		assert(r == MyList(1,3,5))
		val r2 = MyList.dropWhile(xs)(i => i < 100)
		assert(r2 == Nil)
		val r3 = MyList.dropWhile(r2)(i => i < 100)
		assert(r3 == Nil)
	}

	test("testInit") {
		val xs = MyList(1,2)
		val r = MyList.init(xs)
		assert(r == MyList(1))
		val r2 = MyList.init(r)
		assert(r2 == Nil)
		intercept[IndexOutOfBoundsException] {
			MyList.init(Nil)
		}
	}

	test("testFoldRight") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.foldRight(xs, "end")((a,b) => a.toString + b)
		assert(r == "12345end")
		val r2 = MyList.foldRight(Nil, "end")((a,b) => a.toString + b)
		assert(r2 == "end")
	}

	test("testLength") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.length(xs)
		assert(r == 5)
		val r2 = MyList.length(Nil)
		assert(r2 == 0)
	}

	test("testFoldLeft") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.foldLeft("begin", xs)((l, r) => l + r.toString)
		assert(r == "begin12345")
		val r2 = MyList.foldLeft("begin", Nil)((l, r) => l + r.toString)
		assert(r2 == "begin")
	}

	test("testLength_2") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.length_2(xs)
		assert(r == 5)
		val r2 = MyList.length_2(Nil)
		assert(r2 == 0)
	}

	test("testReverse") {
		val xs = MyList(1,2,3,4,5)
		val r = MyList.reverse(xs)
		assert(r == MyList(5,4,3,2,1))
		val r2 = MyList.reverse(Nil)
		assert(r2 == Nil)
	}

	test("testAppend") {
		val as = MyList(1,2)
		val bs = MyList(3,4)
		val r = MyList.append(as, bs)
		assert(r == MyList(1,2,3,4))
		val r2 = MyList.append(as, Nil)
		assert(r2 == as)
		val r3 = MyList.append(Nil, bs)
		assert(r3 == bs)
	}

	test("testFlatten") {
		val as = MyList(1,2)
		val bs = MyList(3,4)
		val r = MyList.flatten(MyList(as, bs))
		assert(r == MyList(1,2,3,4))
		val r1 = MyList.flatten(MyList(bs, as))
		assert(r1 == MyList(3,4,1,2))
		val r2 = MyList.flatten(MyList(as, Nil))
		assert(r2 == as)
		val r3 = MyList.flatten(MyList(Nil, bs))
		assert(r3 == bs)
		val r4 = MyList.flatten(Nil)
		assert(r4 == Nil)
	}

	test("testMap") {
		val as = MyList(1,2,3,4)
		val r = MyList.map(as)(i => i +1)
		assert(r == MyList(2,3,4,5))
		val r2 = MyList.map(as)(i => i.toString)
		assert(r2 == MyList("1","2","3","4"))
		val r3 = MyList.map(Nil)(i => i.toString)
		assert(r3 == Nil)
	}

	test("testFilter") {
		val as = MyList(1,2,3,4)
		val r = MyList.filter(as)(i => (i %2) == 0)
		assert(r == MyList(2,4))
		val r3 = MyList.filter(Nil)(i => i.toString.length == 0)
		assert(r3 == Nil)
	}

	test("testFlatMap") {
		val as = MyList(1,2,3,4)
		val r = MyList.flatMap(as)(i => MyList(i,i))
		assert(r == MyList(1,1,2,2,3,3,4,4))
		val r3 = MyList.flatMap(Nil)(i => MyList(i,i))
		assert(r3 == Nil)
	}

	test("testZipWith") {
		val as = MyList(1,2)
		val bs = MyList(3,4)
		val r = MyList.zipWith(as, bs)((a, b) => a + b)
		assert(r == MyList(4,6))
		val r3 = MyList.zipWith(as, Nil)((a, b) => s"$a, $b")
		assert(r3 == Nil)
	}

}
