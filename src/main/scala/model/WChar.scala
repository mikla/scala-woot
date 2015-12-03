package model

case class OperationClock(value: Int) extends AnyVal {
  def next: OperationClock = OperationClock(value + 1)
  def <(that: OperationClock): Boolean = this.value < that.value
}

case class WChar(id: CharId, char: Char, prev: Id, next: Id, isVisible: Boolean = true)
