package model

import org.scalatest.{Matchers, FlatSpec}

class WStringSpec extends FlatSpec with Matchers {

  "pos" should "return a position of character by Id" in {
    import WChars._
    val wstring = WString(SiteId("A"), OperationClock(2), chars = Vector(A, B))

    wstring.pos(Beginning) should equal (0)

    wstring.pos(Ending) should equal (wstring.chars.length)
    wstring.pos(CharId("A", OperationClock(1))) should equal (1)
  }

  "subseq" should "return sub sequence of chars from start index to end index both are not included" in {
    import WChars._

    val sub = ABCDF.subseq(A.id, F.id)
    sub.size should equal (3)
    sub should equal (Vector(B, C, D))
  }

  "text" should "return text representation of WString" in {
    import WChars._

    ABCDF.text should equal ("abcdf")

    val empty = WString(SiteId("A"), OperationClock(0))
    empty.text should be ("")
  }

  "integrateIns" should "place just signle WChar at right pisition" in {
    import WChars._
    val wstring = WString(SiteId("A"), OperationClock(2), chars = Vector(A, B))
    val acb = wstring.integrateIns(C, A.id, B.id)

    acb.chars should equal (Vector(A, C, B))
  }

  it should "place characters to the right position" in {
    import WChars._

    /** Let's say we have a text entered by SiteId("A")
      * ABCDF <- SideId("B") connected at this moment and pressed enter and entered EJG
      * EJG
      */


    val newLine = WChar(CharId("B", OperationClock(0)), '\n', F.id, Ending, isVisible = true)
    val E = WChar(CharId("B", OperationClock(1)), 'e', newLine.id, Ending, isVisible = true)
    val J = WChar(CharId("B", OperationClock(2)), 'j', E.id, Ending, isVisible = true)
    val G = WChar(CharId("B", OperationClock(3)), 'g', F.id, Ending, isVisible = true)

    /** SiteId("A") continue typing into 1st line */
    val Y = WChar(CharId("A", OperationClock(5)), 'y', F.id, newLine.id, isVisible = true)

    val t = ABCDF.integrateIns(newLine, F.id, Ending)
      .integrateIns(E, newLine.id, Ending)
      .integrateIns(J, E.id, Ending)
      .integrateIns(G, J.id, Ending)
      .integrateIns(Y, F.id, newLine.id)

    t.text should equal ("abcdfy\nejg")
  }

  "integrateDel" should "set for WChar isVisible = false" in {
    import WChars._

    val acdf = ABCDF.integrateDel(B)

    acdf.text should be ("acdf")
    acdf.chars.find(_.id == B.id).foreach(_.isVisible should equal (false))
  }

  it should "delete WChars from the beginning of the WString" in {
    import WChars._

    val bcdf = ABCDF.integrateDel(A)
    bcdf.text should equal ("bcdf")
  }

  it should "delete WChars from the end if the String" in {
    import WChars._

    val bcdf = ABCDF.integrateDel(F)
    bcdf.text should equal ("abcd")
  }

  "contains" should "check if WChar with that id contains in WString" in {
    import WChars._
    val wstring = WString(SiteId("A"), OperationClock(3), chars = Vector(A, B, C))

    wstring.contains(A.id) should equal (true)
    wstring.contains(D.id) should equal (false)
  }

  "isExecutable" should "check whether its possible to integrate operation into WString" in {
    import WChars._
    val wstring = WString(SiteId("A"), OperationClock(3), chars = Vector(A, B, C))

    val F = WChar(CharId("B", OperationClock(0)), 'f', B.id, C.id)

    val R = WChar(CharId("B", OperationClock(1)), 'r', B.id, C.id)
    val S = WChar(CharId("B", OperationClock(2)), 's', R.id, Ending)

    wstring.isExecutable(InsertOp(F)) should equal (true)
    wstring.isExecutable(InsertOp(S)) should equal (false)

    wstring.isExecutable(DeleteOp(B)) should equal (true)
    wstring.isExecutable(DeleteOp(S)) should equal (false)
  }

  "ithVisible" should "return i-th visible element in the WString" in {
    import WChars._

    val ac = ABCDF.integrateDel(B)

    ac.ithVisible(0) should equal (A)
    ac.ithVisible(1) should equal (C)
    ac.ithVisible(3) should equal (F)
  }

  // private

  private object WChars {
    val A = WChar(CharId("A", OperationClock(0)), 'a', Beginning, Ending, isVisible = true)
    val B = WChar(CharId("A", OperationClock(1)), 'b', Beginning, Ending, isVisible = true)
    val C = WChar(CharId("A", OperationClock(2)), 'c', Beginning, Ending, isVisible = true)
    val D = WChar(CharId("A", OperationClock(3)), 'd', Beginning, Ending, isVisible = true)
    val F = WChar(CharId("A", OperationClock(4)), 'f', Beginning, Ending, isVisible = true)

    val ABCDF = WString(SiteId("A"), OperationClock(5), chars = Vector(A, B, C, D, F))
  }

}
