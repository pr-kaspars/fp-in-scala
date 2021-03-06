package fpinscala.state

import fpinscala.state.RNG._
import org.scalatest.{FlatSpec, Matchers}

import scala.Int.{MaxValue, MinValue}

class RNGTest extends FlatSpec with Matchers {

  val d: Double = MaxValue.toDouble

  case class TestRng(list: List[Int]) extends RNG {
    override def nextInt: (Int, RNG) = (list.head, TestRng(list.tail))
  }

  "nonNegativeInt" should "return identity for positive vales" in {
    val result = nonNegativeInt(TestRng(List(10)))
    result._1 shouldBe 10
    result._2 shouldBe TestRng(Nil)
  }

  it should "return identity if values is zero" in {
    val result = nonNegativeInt(TestRng(List(0)))
    result._1 shouldBe 0
    result._2 shouldBe TestRng(Nil)
  }

  it should "return absolute value of negative number" in {
    val result = nonNegativeInt(TestRng(List(-999)))
    result._1 shouldBe 999
    result._2 shouldBe TestRng(Nil)
  }

  it should "return zero if value is min int" in {
    val result = nonNegativeInt(TestRng(List(MinValue)))
    result._1 shouldBe 0
    result._2 shouldBe TestRng(Nil)
  }

  "doubleOld" should "return zero" in {
    val result = doubleOld(TestRng(List(0)))
    result._1 shouldBe 0
    result._2 shouldBe TestRng(Nil)
  }

  it should "return value smaller than one" in {
    val result = doubleOld(TestRng(List(MaxValue)))
    result._1 < 1 shouldBe true
    result._2 shouldBe TestRng(Nil)
  }

  it should "return value greater or equal to zero" in {
    val result = doubleOld(TestRng(List(MinValue)))
    result._1 >= 0 shouldBe true
    result._2 shouldBe TestRng(Nil)
  }

  "double" should "return zero" in {
    val result = double(TestRng(List(0)))
    result._1 shouldBe 0
    result._2 shouldBe TestRng(Nil)
  }

  it should "return value smaller than one" in {
    val result = double(TestRng(List(MaxValue)))
    result._1 < 1 shouldBe true
    result._2 shouldBe TestRng(Nil)
  }

  it should "return value greater or equal to zero" in {
    val result = double(TestRng(List(MinValue)))
    result._1 >= 0 shouldBe true
    result._2 shouldBe TestRng(Nil)
  }

  "intDouble" should "return tuple of random numbers" in {
    val result = intDouble(TestRng(List(1, 8)))
    result._1 shouldBe (1, 8 / d)
    result._2 shouldBe TestRng(Nil)
  }

  "doubleInt" should "return tuple of random numbers" in {
    val result = doubleInt(TestRng(List(1, 8)))
    result._1 shouldBe (8 / d, 1)
    result._2 shouldBe TestRng(Nil)
  }

  "double3" should "return tuple of three doubles" in {
    val result = double3(TestRng(List(100, 300, 800)))
    result._1 shouldBe (100 / d, 300 / d, 800 / d)
    result._2 shouldBe TestRng(Nil)
  }

}
