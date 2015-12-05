package model

import org.scalatest.{Matchers, FlatSpec}

class WStringSpec extends FlatSpec with Matchers {

  "pos" should "return a position of character by Id" in {
    import WChars._
    val wstring = WString(SiteId("A"), chars = Vector(A, B))

    wstring.pos(Beginning) should equal(0)
    wstring.pos(Ending) should equal(wstring.chars.length)
    wstring.pos(CharId("A", OperationClock(1))) should equal(1)
  }

  "subseq" should "return sub sequence of chars from start index to end index both are not included" in {
    import WChars._
    val wstring = WString(SiteId("A"), chars = Vector(A, B, C, D, F))

    val sub = wstring.subseq(A.id, F.id)
    sub.size should equal(3)
    sub should equal(Vector(B, C, D))
  }

  "text" should "return text representation of WString" in {
    import WChars._

    val abcdf = WString(SiteId("A"), chars = Vector(A, B, C, D, F))
    abcdf.text should equal("abcdf")

    val empty = WString(SiteId("A"))
    empty.text should be("")
  }

  "integrateIns" should "place characters to the right position" in {
    import WChars._

    /** Let's say we have a text entered by SiteId("A")
      * ABCDF <- SideId("B") connected at this moment and pressed enter and entered EJG
      * EJG
      */

    val wstring = WString(SiteId("A"), chars = Vector(A, B, C, D, F))

    val newLine = WChar(CharId("B", OperationClock(0)), '\n', F.id, Ending, isVisible = true)
    val E = WChar(CharId("B", OperationClock(1)), 'e', newLine.id, Ending, isVisible = true)
    val J = WChar(CharId("B", OperationClock(2)), 'j', E.id, Ending, isVisible = true)
    val G = WChar(CharId("B", OperationClock(3)), 'g', F.id, Ending, isVisible = true)

    /** SiteId("A") continue typing into 1st line */
    val Y = WChar(CharId("A", OperationClock(5)), 'y', F.id, newLine.id, isVisible = true)

    val t = wstring.integrateIns(newLine, F.id, Ending)
      .integrateIns(E, newLine.id, Ending)
      .integrateIns(J, E.id, Ending)
      .integrateIns(G, J.id, Ending)
      .integrateIns(Y, F.id, newLine.id)

    println(t.text)

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
