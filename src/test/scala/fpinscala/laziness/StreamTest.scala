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

  "takeWhileMatch" should "return empty" in {
    Stream.empty.takeWhileMatch((_: Int) => true) shouldBe Empty
  }

  it should "return empty when predicate is false" in {
    Stream(1, 2, 3).takeWhileMatch(_ => false) shouldBe Empty
  }

  it should "return beginning of the stream" in {
    Stream(1, 2, 3, 4).takeWhileMatch(_ < 3).toList shouldBe List(1, 2)
  }

  "takeWhile" should "return empty" in {
    Stream.empty.takeWhile((_: Int) => true) shouldBe Empty
  }

  it should "return empty when predicate is false" in {
    Stream(1, 2, 3).takeWhile(_ => false) shouldBe Empty
  }

  it should "return beginning of the stream" in {
    Stream(1, 2, 3, 4).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }


  "forAll" should "return false if the stream is empty" in {
    Stream.empty.forAll((_: Int) => true) shouldBe false
  }

  it should "return false if no elements match" in {
    Stream(2, 4, 6, 8).forAll(_ % 5 == 0) shouldBe false
  }

  it should "return true if no elements match" in {
    Stream(2, 4, 6, 8).forAll(_ % 2 == 0) shouldBe true
  }

  "headOption" should "return None when empty stream" in {
    Stream.empty.headOption shouldBe None
  }

  it should "return head item" in {
    Stream(1, 2, 3).headOption shouldBe Some(1)
  }
}
