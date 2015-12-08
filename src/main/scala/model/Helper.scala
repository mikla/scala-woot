package model

object Helper {

  /** Remove the characters that have a previous or next character is `chars` */
  def trim(chars: Vector[WChar]): Vector[Id] = for {
    c <- chars
    if chars.forall(x => x.id != c.next && x.id != c.prev)
  } yield c.id

}
