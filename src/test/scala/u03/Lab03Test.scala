package u03

import org.junit.*
import org.junit.Assert.*
import Lab03.*
import u02.Optionals.*
import u02.AlgebraicDataTypes.*

class Lab03Test:
  import List.*
  import Person.*
  import Stream.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val tail = Cons(40 , Nil() )
  val people = Cons(Person.Student("Mario", 2000), Cons(Person.Teacher("Luigi", "Storia"), Cons(Person.Teacher("Alessia", "Matematica"), Nil())))
  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
  val n: Int = 0
  val fibs : Stream[Int] = fib(n)



  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), List.map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), List.map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), List.filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), List.filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), List.drop(l, 1))
    assertEquals(Cons(30, Nil()), List.drop(l, 2))
    assertEquals(Nil(), List.drop(l, 5))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMapSecondVersion() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapSecondVersion(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapSecondVersion(l)(_+""))

  @Test def testFilterSecondVersion() =
    assertEquals(Cons(20, Cons(30, Nil())), filterSecondVersion(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filterSecondVersion(l)(_!=20))

  @Test def testMax() =
    assertEquals(Option.Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Option.None(), max(Nil()))
    assertEquals(Option.Some(70), max(Cons(30, Cons(50, Cons(25, Cons(70, Nil()))))))

  @Test def testCourses() =
    assertEquals(Cons("Storia", Cons("Matematica", Nil())), fromPersonToCourses(people))

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst)(0)( _ - _ ))
    assertEquals(16, foldLeft(lst)(0)( _ + _ ))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(lst)(0)( _ - _ ))
    assertEquals(16, foldRight(lst)(0)( _ + _ ))

  @Test def testDropStream() =
    assertEquals(Lists.List.Cons(6, Lists.List.Cons(7, Lists.List.Cons(8, Lists.List.Cons(9, Lists.List.Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstantStream() =
    assertEquals(Lists.List.Cons("x", Lists.List.Cons("x", Lists.List.Cons("x", Lists.List.Cons("x", Lists.List.Cons("x", Lists.List.Nil()))))), Stream.toList(Stream.take(constant("x"))(5)))

  @Test def testFibonacci() =
    assertEquals(Lists.List.Cons(0, Lists.List.Cons(1, Lists.List.Cons(1, Lists.List.Cons(2, Lists.List.Cons(3, Lists.List.Cons(5, Lists.List.Cons(8, Lists.List.Cons(13, Lists.List.Nil())))))))), Stream.toList(Stream.take(fibs)(8)))
