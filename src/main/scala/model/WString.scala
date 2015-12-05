package model

case class WString(siteId: SiteId,
                   chars: Vector[WChar] = Vector.empty) {

  def length = ???

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

  /** Returns `true` if `c` can be found is `WString`. */
  def contains(c: WChar): Boolean = ???

  /** The sequence of visible `WChar`s. */
  def value: String = ???

  /** Returns the i-th visible character of `WString`. */
  def ithVisible(i: Int): WChar = ???

  /** Placing `c` among all the characters between `prev` and `next`. These characters can be previously deleted
    * characters or characters inserted by concurrent operations.
    */
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

  /** Remove the characters that have a previous or next character is `chars` */
  def trim(chars: Vector[WChar], perv: Id, next: Id): Vector[Id] = for {
    c <- chars
    if chars.forall(x => x.id != c.next && x.id != c.prev)
  } yield c.id

}
