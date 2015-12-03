package model

class WString(siteId: SiteId,
              chars: Vector[WChar] = Vector.empty) {

  def length = ???

  def indexOf(pos: Int): WChar = ???

  def pos(c: WChar): Int = ???

  def insert(c: WChar, position: Int): WString = ???

  /** Returns the part of the `WString` between elements `start` end `end`, both are not included. */
  def subseq(start: WChar, end: WChar): Vector[WChar] = ???

  def contains(c: WChar): Boolean = ???

  /** The sequence of visible `WChar`s. */
  def value: String = ???

  /** Returns the i-th visible character of `WString`. */
  def ithVisible(i: Int): WChar = ???

}
