package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def takeOld(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons[A](h, () => t().takeOld(n - 1))
      case _ => Empty
    }

  def take(n: Int): Stream[A] =
    unfold((n, this))(s => s._2 match {
      case Cons(h, t) if s._1 > 0 => Some((h(), (s._1 - 1, t())))
      case _ => None
    })

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) => if (n > 1) t().drop(n - 1) else t()
      case _ => Empty
    }

  def takeWhileMatch(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons[A](h, () => t().takeWhileOld(p))
      case _ => Empty
    }

  def takeWhileOld(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Stream.empty)((a, b) =>
      if (p(a)) Cons[A](() => a, () => b.takeWhileOld(p)) else b
    )

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this)(_ match {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    })

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Empty => false
      case _ => foldRight(true)((a, b) => p(a) && b)
    }

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a).orElse(b))

  def mapOld[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => Cons(() => f(a), () => b))

  def map[B](f: A => B): Stream[B] =
    unfold(this)(_ match {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) =>
      if (f(a)) Cons(() => a, () => b) else b
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Cons(() => a, () => b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => f(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = ???

  def toList: List[A] =
    foldRight[List[A]](Nil)((a: A, b) => a :: b)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2))(s => ???)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val onesOld: Stream[Int] = Stream.cons(1, onesOld)

  val ones: Stream[Int] =
    unfold(1)(_ => Option((1, 1)))

  def constantOld[A](a: A): Stream[A] =
    cons(a, constantOld(a))

  def constant[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  def fromOld(n: Int): Stream[Int] =
    cons(n, fromOld(n + 1))

  def from(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  val fibsOld: Stream[Int] = {
    def loop(a: => Int, b: => Int): Stream[Int] =
      Stream.cons(a, loop(b, a + b))

    loop(0, 1)
  }

  val fibs: Stream[Int] =
    unfold((0, 1))(t => Some((t._1, (t._2, t._1 + t._2))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some(t) => Stream.cons(t._1, unfold(t._2)(f))
      case _ => Empty
    }

  def zipWith[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = ???

}
