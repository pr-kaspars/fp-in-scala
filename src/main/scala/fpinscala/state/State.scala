package fpinscala.state

import java.lang.Math._

import scala.Int.MaxValue

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    Some(rng.nextInt)
      .map(t => (abs(t._1) & MaxValue, t._2))
      .head

  // Exercise 6.2
  def doubleOld(rng: RNG): (Double, RNG) =
    Some(nonNegativeInt(rng))
      .map(t => ((abs(t._1) - floorDiv(t._1, MaxValue)) / MaxValue.toDouble, t._2))
      .head

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    Some(rng.nextInt)
      .map(t => ((t._1, double(t._2))))
      .map(t => ((t._1, t._2._1), t._2._2))
      .head

  // Exercise 6.3
  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    Some(intDouble(rng))
      .map(t => (t._1.swap, t._2))
      .head

  // Exercise 6.3
  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    Some(1.until(3).foldLeft(List(double(rng)))((l, _) => double(l.head._2) :: l).reverse)
      .map(_ match { case List(a, b, c) => ((a._1, b._1, c._1), c._2) })
      .head

  // Exercise 6.4
  def intsOld(count: Int)(rng: RNG): (List[Int], RNG) =
    Some(1.until(count).foldLeft(List(rng.nextInt))((l, _) => l.head._2.nextInt :: l).unzip)
      .map(t => (t._1, t._2.head))
      .head

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  // Exercise 6.5
  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => (i - floorDiv(i, MaxValue)) / MaxValue.toDouble)(rng)

  // Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, ga) = ra(rng)
      val (b, gb) = rb(ga)
      (f(a, b), gb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    r =>
      Option(fs.foldRight[(List[A], RNG)]((Nil, r))((a, b) =>
        Option(a(b._2)).map(t => (t._1 :: b._1, t._2)).head)
      ).map(t => (t._1.reverse, t._2)).head


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
