package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  "toList" should "return an empty list" in {
    Stream.empty.toList shouldBe Nil
  }

  it should "return list with all elements in the stream" in {
    Stream(1, 2, 3, 4).toList shouldBe List(1, 2, 3, 4)
  }

  "take" should "return empty" in {
    Stream.empty.take(3) shouldBe Empty
  }

  it should "return Stream of correct length" in {
    Stream(1, 2, 3, 4, 5).take(3).toList shouldBe List(1, 2, 3)
  }

  it should "return Stream of max available length" in {
    Stream(1, 2, 3).take(5).toList shouldBe List(1, 2, 3)
  }

  "drop" should "return empty" in {
    Stream.empty.drop(4) shouldBe Empty
  }

  it should "return empty after removing all elements" in {
    Stream(1, 2, 3).drop(5) shouldBe Empty
  }

  it should "return remaining elements" in {
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldBe List(4, 5)
  }
}
