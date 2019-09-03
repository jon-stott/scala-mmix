package org.jstott.mmix.assembler

sealed trait ExpressionToken

sealed trait UnaryOperatorToken

case object AffirmationToken extends UnaryOperatorToken
case object NegationToken extends UnaryOperatorToken
case object ComplementationToken extends UnaryOperatorToken
case object RegisterizationToken extends UnaryOperatorToken

sealed trait PrimaryToken extends ExpressionToken {
  def unaryOperator: UnaryOperatorToken = AffirmationToken
}

case class SymbolToken(s: String, override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken
case class ConstantToken(n: BigInt, override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken
case object CurrentLocationToken extends PrimaryToken
case class ParenthesisedExpression(expression: List[ExpressionToken], override val unaryOperator: UnaryOperatorToken = AffirmationToken) extends PrimaryToken

sealed trait BinaryOperatorToken extends ExpressionToken

sealed trait StrongBinaryOperatorToken extends BinaryOperatorToken

case object MultiplyToken extends StrongBinaryOperatorToken
case object DivideToken extends StrongBinaryOperatorToken
case object FractionalDivideToken extends StrongBinaryOperatorToken
case object RemainderToken extends StrongBinaryOperatorToken
case object LeftShiftToken extends StrongBinaryOperatorToken
case object RightShiftToken extends StrongBinaryOperatorToken
case object BitwiseAndToken extends StrongBinaryOperatorToken

sealed trait WeakBinaryOperatorToken extends BinaryOperatorToken

case object AdditionToken extends WeakBinaryOperatorToken
case object SubtractionToken extends WeakBinaryOperatorToken
case object BitwiseOrToken extends WeakBinaryOperatorToken
case object BitwiseExclusiveOrToken extends WeakBinaryOperatorToken
