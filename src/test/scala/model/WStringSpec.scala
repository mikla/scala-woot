package model

import org.scalatest.{Matchers, FlatSpec}

class WStringSpec extends FlatSpec with Matchers {

  "pos" should "return a position of character by Id" in {

    val oc = OperationClock(0)

    val wstring = WString(SiteId("A"), chars = Vector(
      WChar(CharId("A", oc.next), 'a', Beginning, Ending, isVisible = true)
    ))

    println(wstring.pos(CharId("A", OperationClock(1))))
    println(wstring.pos(Beginning))

  }

}
