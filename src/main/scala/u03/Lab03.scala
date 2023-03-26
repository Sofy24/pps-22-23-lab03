package u03

import scala.annotation.tailrec
import u02.Optionals.*
import u02.AlgebraicDataTypes.*
import u03.Lab03.List.*


object Lab03 extends App :
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

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


    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    //Task 1
    //A)
    def drop[A](l: List[A], n: Int): List[A] = (l,n) match
      case (l, 0) => l
      case (Nil(), _) => Nil()
      case (Cons(h, t), n) => drop(t, n-1)

    //B)
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, Nil()), r) => Cons(h, r)
      case (Cons(h, t), r) => Cons(h, append(t, r))

    //C)
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h), flatMap(t)(f))

    //D)
    def mapSecondVersion[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => flatMap(l)(h => Cons(mapper(h), Nil()))
      case Nil() => Nil()

    def filterSecondVersion[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => flatMap(l1)(h => Cons(h, Nil()))
      case Cons(h, t) if !pred(h) => filterSecondVersion(t)(pred)
      case Nil() => Nil()

    //Task 2
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => t match
        case Cons(h3, Nil()) if h > h3 => Option.Some(h)
        case Cons(h3, Nil()) if h <= h3 => Option.Some(h3)
        case Cons(h2, t2) if h > h2 => max(Cons(h, t2))
        case Cons(h2, t2) if h <= h2 => max(t)
      case Nil() => Option.None()

    //Task 3
    def fromPersonToCourses(list: List[Person]): List[String] =
      flatMap(list)(p => p match
        case Person.Teacher(_, course) => Cons(course, Nil())
        case _ => Nil()
      )

    //Task 4
    def foldLeft[A, B](list: List[A])(default: B)(acc: (B, A) => B): B = list match
      case Nil() => default
      case Cons(h, t) => foldLeft(t)(acc(default, h))(acc)

    def foldRight[A, B](list: List[A])(default: B)(acc: (A, B) => B): B = list match
        case Cons(h, t) => acc(h, foldRight(t)(default)(acc))
        case Nil() => default



  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    //Task 5
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 1 => drop(tail())(n - 1)
      case (Cons(head, tail), _) => tail()

    //Task 6
    def constant[A](k: A): Stream[A] =
      Stream.cons(k, Stream.constant(k))

    //Task 7
    def fibo(n: Int): Int = n match
      case 0 => 0
      case 1 => 1
      case _ => fibo(n - 1) + fibo(n - 2)
    
    def fib(n: Int): Stream[Int] =
      Stream.cons(fibo(n), fib(n + 1))

  end Stream

  import List.*
  import Option.*
  import Person.*

