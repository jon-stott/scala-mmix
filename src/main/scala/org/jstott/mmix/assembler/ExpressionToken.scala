package org.jstott.mmix.assembler

import org.jstott.mmix.{ MmixByte, MmixOcta, MmixTetra, MmixWyde }

import scala.util.matching.Regex

sealed trait ExpressionToken

sealed trait UnaryOperatorToken

case object AffirmationToken extends UnaryOperatorToken {
  override def toString: String = ""
}
case object NegationToken extends UnaryOperatorToken {
  override def toString: String = "-"
}
case object ComplementationToken extends UnaryOperatorToken {
  override def toString: String = "~"
}
case object RegisterizationToken extends UnaryOperatorToken {
  override def toString: String = "$"
}

sealed trait PrimaryToken extends ExpressionToken {
  def unaryOperator: UnaryOperatorToken = AffirmationToken

  def toMmixByte: MmixByte
  def toMmixWyde: MmixWyde
  def toMmixTetra: MmixTetra
  def toMmixOcta: MmixOcta

  final def isRegister: Boolean = unaryOperator == RegisterizationToken
}

case class SymbolToken(s: String, override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken {
  override def toMmixByte: MmixByte = ???
  override def toMmixWyde: MmixWyde = ???
  override def toMmixTetra: MmixTetra = ???
  override def toMmixOcta: MmixOcta = ???
  override def toString: String = unaryOperator.toString + s
}
object SymbolToken {
  val LocalSymbolPattern: Regex = "^(\\d)([BFH])$".r
}
case class ConstantToken(n: BigInt, override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken {
  override def toMmixByte: MmixByte = MmixByte(n.shortValue)
  override def toMmixWyde: MmixWyde = MmixWyde(n.intValue)
  override def toMmixTetra: MmixTetra = MmixTetra(n.longValue)
  override def toMmixOcta: MmixOcta = MmixOcta(n.longValue)
  override def toString: String = unaryOperator.toString + n.toString
}
case class StringToken(chars: List[BigInt], override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken {
  override def toMmixByte: MmixByte = ???
  override def toMmixWyde: MmixWyde = ???
  override def toMmixTetra: MmixTetra = ???
  override def toMmixOcta: MmixOcta = ???
  override def toString: String = "\"" + chars.map(_.charValue).mkString + "\""
}
case object CurrentLocationToken extends PrimaryToken {
  override def toMmixByte: MmixByte = ???
  override def toMmixWyde: MmixWyde = ???
  override def toMmixTetra: MmixTetra = ???
  override def toMmixOcta: MmixOcta = ???
  override def toString: String = "@"
}
case class ParenthesisedExpression(expression: List[ExpressionToken], override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken {
  override def toMmixByte: MmixByte = ???
  override def toMmixWyde: MmixWyde = ???
  override def toMmixTetra: MmixTetra = ???
  override def toMmixOcta: MmixOcta = ???
  override def toString: String = "(" + unaryOperator.toString + expression.mkString + ")"
}

sealed trait BinaryOperatorToken extends ExpressionToken {
  def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken
}

sealed trait StrongBinaryOperatorToken extends BinaryOperatorToken

case object MultiplyToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ConstantToken(n1 * n2)
  }
  override def toString: String = "*"
}
case object DivideToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ConstantToken(n1 / n2)
  }
  override def toString: String = "/"
}
case object FractionalDivideToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = "//"
}
case object RemainderToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = "%"
}
case object LeftShiftToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = "<<"
}
case object RightShiftToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = ">>"
}
case object BitwiseAndToken extends StrongBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = "&"
}

sealed trait WeakBinaryOperatorToken extends BinaryOperatorToken

case object AdditionToken extends WeakBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ConstantToken(n1 + n2)
  }
  override def toString: String = "+"
}
case object SubtractionToken extends WeakBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ConstantToken(n1 - n2)
  }
  override def toString: String = "-"
}
case object BitwiseOrToken extends WeakBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = "|"
}
case object BitwiseExclusiveOrToken extends WeakBinaryOperatorToken {
  override def evaluate(p1: PrimaryToken, p2: PrimaryToken): PrimaryToken = (p1, p2) match {
    case (ConstantToken(n1, o1), ConstantToken(n2, o2)) => ???
  }
  override def toString: String = "^"
}
