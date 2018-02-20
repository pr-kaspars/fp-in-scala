package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  val l5 = List(1, 2, 3, 4, 5)

  "tail" should "return list without its first element" in {
    List.tail(l5) should be(List(2, 3, 4, 5))
  }

  it should "return empty list" in {
    List.tail(Nil) should be(Nil)
  }

  "setHead" should "replace the first element" in {
    List.setHead(l5, 6) should be(List(6, 2, 3, 4, 5))
  }
}
