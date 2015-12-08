package model

import model.Helper._
import org.scalatest.{Matchers, FlatSpec}

class HelperSpec extends FlatSpec with Matchers {

  "trim" should "remove characters that have a previous or next character in chars" in {
    /* We are removing character among which it's not possible to insert new character, because these
     * characters connected by precedence relation. Example:
     * A.next = B.
     * B.prev = A.
     * If we have such example it's not possible to insert something between A and B.
     */

    val A = WChar(CharId("A", OperationClock(0)), 'a', Beginning, Ending, isVisible = true)
    val B = WChar(CharId("A", OperationClock(1)), 'b', A.id, Ending, isVisible = true)
    val C = WChar(CharId("A", OperationClock(2)), 'c', B.id, Ending, isVisible = true)
    val D = WChar(CharId("A", OperationClock(3)), 'd', C.id, Ending, isVisible = true)

    val chars = Vector(A, B, C, D)

    trim(chars).head should be (A.id)
  }

}
