package u03

import scala.annotation.tailrec
import u02.Optionals.*


object Lists extends App :
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

    /*def mapSecondVersion[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(mapper)*/

    /*def filterSecondVersion[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) => flatMap(t)(pred(h))*/



    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => t match
        case Cons(h3, Nil()) if h > h3 => Option.Some(h)
        case Cons(h3, Nil()) if h <= h3 => Option.Some(h3)
        case Cons(h2, t2) if h > h2 => max(Cons(h, t2))
        case Cons(h2, t2) if h <= h2 => max(t)
      case Nil() => Option.None()



  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*
  import Option.*


  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52

