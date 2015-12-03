package model

sealed trait Operation {
  def wchar: WChar
}

case class InsertOp(override val wchar: WChar) extends Operation
case class DeleteOp(override val wchar: WChar) extends Operation
