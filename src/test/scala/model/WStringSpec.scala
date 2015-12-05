package model

import org.scalatest.{Matchers, FlatSpec}

class WStringSpec extends FlatSpec with Matchers {

  "pos" should "return a position of character by Id" in {
    import WChars._
    val wstring = WString(SiteId("A"), chars = Vector(A, B))

    wstring.pos(Beginning) should equal (0)
    wstring.pos(Ending) should equal (wstring.chars.length)
    wstring.pos(CharId("A", OperationClock(1))) should equal (1)
  }

  "subseq" should "return sub sequence of chars from start index to end index both are not included" in {
    import WChars._
    val wstring = WString(SiteId("A"), chars = Vector(A, B, C, D, F))

    val sub = wstring.subseq(CharId("A", OperationClock(0)), CharId("A", OperationClock(4)))
    sub.size should equal (3)
    sub should equal (Vector(B, C, D))
  }

  "integrateIns" should "place character to the right position" in {
    import WChars._
    val wstring = WString(SiteId("A"), chars = Vector(A, B, C, D, F))
  }

  // private

  private object WChars {
    val A = WChar(CharId("A", OperationClock(0)), 'a', Beginning, Ending, isVisible = true)
    val B = WChar(CharId("A", OperationClock(1)), 'b', Beginning, Ending, isVisible = true)
    val C = WChar(CharId("A", OperationClock(2)), 'c', Beginning, Ending, isVisible = true)
    val D = WChar(CharId("A", OperationClock(3)), 'd', Beginning, Ending, isVisible = true)
    val F = WChar(CharId("A", OperationClock(4)), 'f', Beginning, Ending, isVisible = true)
  }

}
