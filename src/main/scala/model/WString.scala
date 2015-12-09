package model

import model.Helper._

case class WString(siteId: SiteId,
                   chars: Vector[WChar] = Vector.empty) {

  lazy val visible = chars.filter(_.isVisible)

  // The visible text
  lazy val text: String = visible.map(_.char).mkString

  /** Returns the element at position `pos`.
    * We state that the first element is at position 0.
    */
  def indexOf(pos: Int): WChar = ???

  def pos(id: Id): Int = id match {
    case Beginning => 0
    case Ending => chars.length
    case _ => chars.indexWhere(_.id == id)
  }

  def insert(c: WChar, position: Int): WString = {
    val (before, after) = chars splitAt position
    copy(chars = (before :+ c) ++ after)
  }

  /** Returns the part of the `WString` between elements `start` end `end`, both are not included. */
  def subseq(start: Id, end: Id): Vector[WChar] = {
    val from = start match {
      case Beginning => 0
      case id => pos(id) + 1
    }

    val until = end match {
      case Ending => chars.length
      case id => pos(id)
    }

    chars.slice(from, until)
  }

  /** Returns `true` if `c` can be found is `WString` by its `id` */
  def contains(id: Id): Boolean = chars.exists(_.id == id)

  /** Returns the i-th visible character of `WString`. */
  def ithVisible(i: Int): WChar = ???

  /** Placing `c` among all the characters between `prev` and `next`. These characters can be previously deleted
    * characters or characters inserted by concurrent operations.
    */
  def integrateIns(c: WChar, prev: Id, next: Id): WString = {
    val sub: Vector[WChar] = subseq(prev, next)
    if (sub.isEmpty) insert(c, pos(next))
    else {
      val L = prev +: trim(sub) :+ next
      var i = 1
      while ((i < L.length) && (L(i) < c.id)) i = i + 1
      integrateIns(c, L(i - 1), L(i))
    }
  }

  /** Delete `WChar` and return new `WString`. */
  def integrateDel(c: WChar): WString = {
    val cPos = chars.indexWhere(_.id == c.id)
    val replacement = c.copy(isVisible = false)
    val (before, after) = chars splitAt cPos
    copy(chars = (before :+ replacement) ++ (after drop 1))
  }

  /** Integrate remote operation */
  def integrateOperation(op: Operation): WString = {
    // If isExecutable(op)
    op match {
      case InsertOp(c) =>
        integrateIns(c, c.prev, c.next)
      case DeleteOp(c) =>
        integrateDel(c)
    }
  }

  def insert(c: Char, pos: Int): (Operation, WString) = {
    // get ithVisibe
    // create WChar
    // val wstr = integrateIns(WChar)
    // (InsertOp(Wchar), wstr.copy(tick = newTick))

    ???
  }

  def delete(pos: Int): (Operation, WString) = {
    // get ithVisible WChar
    // wstr = integrateDel(WChar)
    // (DeleteOp(Wchar), wstr.copy(tick = newTick))

    ???
  }

  def isExecutable(op: Operation): Boolean = op match {
    case InsertOp(c) => canIntegrate(c.prev) && canIntegrate(c.next)
    case DeleteOp(c) => contains(c.id)
  }

  private def canIntegrate(id: Id): Boolean = id == Beginning || id == Ending || contains(id)

}
