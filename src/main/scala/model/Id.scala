package model

trait Id {
  def <(that: Id): Boolean
}

object Beginning extends Id {
  override def <(that: Id): Boolean = true
}

object Ending extends Id {
  override def <(that: Id): Boolean = false
}

case class SiteId(value: String) extends AnyVal {
  def <(that: SiteId) = this.value < that.value
}

case class CharId(siteId: String, operationClock: OperationClock) extends Id {
  override def <(that: Id): Boolean = that match {
    case Beginning => false
    case CharId(sid, clock) => (siteId < sid) || (siteId == sid && operationClock < clock)
    case Ending => true
  }
}