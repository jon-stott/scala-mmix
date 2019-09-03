package org.jstott.mmix.assembler

import scala.util.parsing.combinator.RegexParsers

object ExpressionLexer extends RegexParsers {

  def decimalConstant: Parser[BigInt] = "[0-9]{1,10}".r ^^ { BigInt(_) }
  def hexadecimalConstant: Parser[BigInt] = "#" ~> "[0-9a-fA-F]+".r ^^ { BigInt(_, 16) }

  def constant: Parser[BigInt] = decimalConstant | hexadecimalConstant

  def affirmation: Parser[AffirmationToken.type] = "\\+".r ^^ { _ => AffirmationToken }
  def negation: Parser[NegationToken.type] = "-".r ^^ { _ => NegationToken }
  def complementation: Parser[ComplementationToken.type] = "~".r ^^ { _ => ComplementationToken }
  def registerization: Parser[RegisterizationToken.type] = "\\$".r ^^ { _ => RegisterizationToken }

  def unaryOperator: Parser[UnaryOperatorToken] = affirmation | negation | complementation | registerization

  def currentLocation: Parser[CurrentLocationToken.type] = "@" ^^ { _ => CurrentLocationToken}

  def parenthesisedExpression: Parser[ParenthesisedExpression] = opt(unaryOperator) ~ ("(" ~> expr <~ ")") ^^ {
    case Some(u) ~ expressionList => ParenthesisedExpression(expressionList, unaryOperator = u)
    case _ ~ expressionList => ParenthesisedExpression(expressionList)
  }

  def nonParenthesisedExpression: Parser[PrimaryToken] = opt(unaryOperator) ~ (constant | currentLocation) ^^ {
    case _ ~ CurrentLocationToken => CurrentLocationToken
    case Some(u) ~ (c: BigInt)    => ConstantToken(c, unaryOperator = u)
    case _ ~ (c: BigInt)          => ConstantToken(c)
  }

  def primary: Parser[PrimaryToken] = parenthesisedExpression | nonParenthesisedExpression

  def multiply: Parser[MultiplyToken.type] = "\\*".r ^^ { s => MultiplyToken }
  def divide: Parser[DivideToken.type] = "/".r ^^ { s => DivideToken }
  def fractionalDivide: Parser[FractionalDivideToken.type] = "//".r ^^ { s => FractionalDivideToken }
  def remainder: Parser[RemainderToken.type] = "%".r ^^ { s => RemainderToken }
  def leftShift: Parser[LeftShiftToken.type] = "<<".r ^^ { s => LeftShiftToken }
  def rightShift: Parser[RightShiftToken.type] = ">>".r ^^ { s => RightShiftToken }
  def bitwiseAnd: Parser[BitwiseAndToken.type] = "&".r ^^ { s => BitwiseAndToken }

  def strongBinaryOperator: Parser[StrongBinaryOperatorToken] = multiply | fractionalDivide | divide | remainder |
    leftShift | rightShift | bitwiseAnd

  def addition: Parser[AdditionToken.type] = "\\+".r ^^ { s => AdditionToken }
  def subtraction: Parser[SubtractionToken.type] = "-".r ^^ { s => SubtractionToken }
  def bitwiseOr: Parser[BitwiseOrToken.type] = "\\|".r ^^ { s => BitwiseOrToken }
  def bitwiseExclusiveOr: Parser[BitwiseExclusiveOrToken.type] = "\\^".r ^^ { s => BitwiseExclusiveOrToken }

  def weakBinaryOperator: Parser[WeakBinaryOperatorToken] = addition | subtraction | bitwiseOr | bitwiseExclusiveOr

  def term: Parser[List[ExpressionToken]] = {
    log(primary)("primary") ~ rep(log(strongBinaryOperator)("strong binary operator") ~ log(primary)("primary")) ^^ {
      case p ~ list => p +: list.flatMap {
        case o ~ y => List(o, y)
      }
    }
  }

  def expr: Parser[List[ExpressionToken]] = {
    log(term)("term") ~ rep(log(weakBinaryOperator)("weak binary operator") ~ log(term)("term")) ^^ {
      case t ~ list => t ++ list.flatMap {
        case o ~ ys => o +: ys
      }
    }
  }

  def apply(code: String): Either[String, List[ExpressionToken]] = {
    parse(expr, code) match {
      case NoSuccess(msg, next) => Left(s"Error: $msg")
      case Success(result, next) => Right(result)
    }
  }

}
