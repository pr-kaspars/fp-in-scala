package fpinscala.state

import fpinscala.state.RNG.{double, doubleInt, doubleOld, intDouble, nonNegativeInt}
import org.scalatest.{FlatSpec, Matchers}

class RNGTest extends FlatSpec with Matchers {

  case class TestRng(list: List[Int]) extends RNG {
    override def nextInt: (Int, RNG) = (list.head, TestRng(list.tail))
  }

  "nonNegativeInt" should "return identity for positive vales" in {
    val rng = TestRng(List(10))
    nonNegativeInt(rng)._1 shouldBe 10
  }

  it should "return identity if values is zero" in {
    val rng = TestRng(List(0))
    nonNegativeInt(rng)._1 shouldBe 0
  }

  it should "return absolute value of negative number" in {
    val rng = TestRng(List(-999))
    nonNegativeInt(rng)._1 shouldBe 999
  }

  it should "return zero if value is min int" in {
    val rng = TestRng(List(Int.MinValue))
    nonNegativeInt(rng)._1 shouldBe 0
  }

  "doubleOld" should "return zero" in {
    val rng = TestRng(List(0))
    doubleOld(rng)._1 shouldBe 0
  }

  it should "return value smaller than one" in {
    val rng = TestRng(List(Int.MaxValue))
    doubleOld(rng)._1 < 1 shouldBe true
  }

  it should "return value greater or equal to zero" in {
    val rng = TestRng(List(Int.MinValue))
    doubleOld(rng)._1 >= 0 shouldBe true
  }

  "double" should "return zero" in {
    val rng = TestRng(List(0))
    double(rng)._1 shouldBe 0
  }

  it should "return value smaller than one" in {
    val rng = TestRng(List(Int.MaxValue))
    double(rng)._1 < 1 shouldBe true
  }

  it should "return value greater or equal to zero" in {
    val rng = TestRng(List(Int.MinValue))
    double(rng)._1 >= 0 shouldBe true
  }

  "intDouble" should "return tuple of random numbers" in {
    val rng = TestRng(List(1, 8))
    intDouble(rng)._1 shouldBe(1, 8 / Int.MaxValue.toDouble)
  }

  "doubleInt" should "return tuple of random numbers" in {
    val rng = TestRng(List(1, 8))
    doubleInt(rng)._1 shouldBe(1, 8 / Int.MaxValue.toDouble)
  }


}
