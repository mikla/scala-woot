package model

class WString(siteId: SiteId,
              chars: Vector[WChar] = Vector.empty) {

  def length = ???

  def indexOf(pos: Int): WChar = ???

  def pos(c: Id): Int = ???

  def insert(c: WChar, position: Int): WString = ???

  /** Returns the part of the `WString` between elements `start` end `end`, both are not included. */
  def subseq(start: Id, end: Id): Vector[WChar] = ???

  def contains(c: WChar): Boolean = ???

  /** The sequence of visible `WChar`s. */
  def value: String = ???

  /** Returns the i-th visible character of `WString`. */
  def ithVisible(i: Int): WChar = ???

  def integrateIns(c: WChar, prev: Id, next: Id): WString = {
    val sub: Vector[WChar] = subseq(prev, next)
    if (sub.isEmpty) insert(c, pos(next))
    else {
      val L = prev +: trim(sub, prev, next) :+ next
      var i = 1
      while ((i < L.length) && (L(i) < c.id)) i = i + 1
      integrateIns(c, L(i - 1), L(i))
    }
  }

  /** Building an `L` vector of chars. Based on algorithm described in `RT groupware without OT`, page 11.
    */
  def trim(chars: Vector[WChar], perv: Id, next: Id): Vector[Id] = ???

}
