package u03

import scala.annotation.tailrec
import u02.Optionals.*
import u02.AlgebraicDataTypes.*
import u03.Lab03.List.*


object Lab03 extends App :
  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    @tailrec
    def get[E](list: List[E], index: Int): Option[E] = (list, index) match
      case (Nil(), _) => Option.None()
      case (Cons(h, t), 0) => Option.Some(h)
      case (Cons(_, t), i) => get(t, i - 1)

    def add[E](list: List[E], index:Int, element: E): List[E] = (list, index) match {
      case (l, 0) => Cons(element, l)
      case (Nil(), _) => Nil()
      case (Cons(h, t), i) => Cons(h, add(t, i-1, element))
    }

    /*def add[E](list: List[E], index:Int, element: E): Option[List[E]] = (list, index) match
      case (l, 0) => Option.Some(Cons(element, l))
      case (Nil(), _) => Option.None()
      case (Cons(h, t), i) => map(add(t, i-1, element))(l => Cons(h, l))*/
        /*case Option.Some(l) => Option.Some(Cons(h, l))
        case Option.None() => Option.None()*/


    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def drop[A](l: List[A], n: Int): List[A] = (l,n) match
      case (l, 0) => l
      case (Nil(), _) => Nil()
      case (Cons(h, t), n) => drop(t, n-1)

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, Nil()), r) => Cons(h, r)
      case (Cons(h, t), r) => Cons(h, append(t, r))

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    def mapSecondVersion[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => flatMap(l)(h => Cons(mapper(h), Nil()))
      case Nil() => Nil()

    def filterSecondVersion[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => flatMap(l1)(h => Cons(h, Nil()))
      case Cons(h, t) if !pred(h) => filterSecondVersion(t)(pred)
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => t match
        case Cons(h3, Nil()) if h > h3 => Option.Some(h)
        case Cons(h3, Nil()) if h <= h3 => Option.Some(h3)
        case Cons(h2, t2) if h > h2 => max(Cons(h, t2))
        case Cons(h2, t2) if h <= h2 => max(t)
      case Nil() => Option.None()

    def fromPersonToCourses(list: List[Person]): List[String] =
      flatMap(list)(p => p match
        case Person.Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
      )

    def foldLeft[A, B](list: List[A])(default: B)(acc: (B, A) => B): B = list match
      case Nil() => default
      case Cons(h, t) => foldLeft(t)(acc(default, h))(acc)

    def foldRight[A, B](list: List[A])(default: B)(acc: (A, B) => B): B = list match
        case Cons(h, t) => acc(h, foldRight(t)(default)(acc))
        case Nil() => default


  //object Part2:


  import List.*
  import Option.*
  import Person.*

/*
object AlgebraicDataTypes:

    enum Person: // a sum type defined by enumerating various cases
      case Student(name: String, year: Int)
      case Teacher(name: String, course: String)

    def name(p: Person): String = p match
      case Person.Student(n, _) => n
      case Person.Teacher(n, _) => n

    println(name(Person.Student("mario", 2015)))

    enum WeekDay: // a Java-like enumeration
      case Monday, Tuesday, Wednesday, Thursday, Friday

    def preferredDay(d: WeekDay): Boolean =
      d == WeekDay.Friday // could have used matching instead

    // A LinkedList of Int
    enum IntList: // a recursive type
      case Cons(head: Int, tail: IntList)
      case Nil

    def sum(l: IntList): Int = l match
      case IntList.Cons(h, t) => h + sum(t)
      case _ => 0
*/
