package org.jstott.mmix.assembler

import org.scalatest.{ FlatSpec, MustMatchers }

class ExpressionLexerSpec extends FlatSpec with MustMatchers {

  behavior of "ExpressionLexer"

  it should "lex 2" in {
    val result = ExpressionLexer.apply("2")
    result mustBe Right(List(ConstantToken(2, AffirmationToken)))
  }

  it should "lex 2028562" in {
    val result = ExpressionLexer.apply("2028562")
    result mustBe Right(List(ConstantToken(2028562, AffirmationToken)))
  }

  it should "lex #3456aeFb" in {
    val result = ExpressionLexer.apply("#3456aeFb")
    result mustBe Right(List(ConstantToken(0x3456aeFb, AffirmationToken)))
  }

  it should "lex -2 (negation)" in {
    val result = ExpressionLexer.apply("-2")
    result mustBe Right(List(ConstantToken(2, NegationToken)))
  }

  it should "lex +2 (affirmation)" in {
    val result = ExpressionLexer.apply("+2")
    result mustBe Right(List(ConstantToken(2, AffirmationToken)))
  }

  it should "lex ~2 (complementation)" in {
    val result = ExpressionLexer.apply("~2")
    result mustBe Right(List(ConstantToken(2, ComplementationToken)))
  }

  it should "lex $2 (registerization)" in {
    val result = ExpressionLexer.apply("$2")
    result mustBe Right(List(ConstantToken(2, RegisterizationToken)))
  }

  it should "lex @ (current location)" in {
    val result = ExpressionLexer.apply("@")
    result mustBe Right(List(CurrentLocationToken))
  }

  it should "lex 2*5" in {
    val result = ExpressionLexer.apply("2*5")
    result mustBe Right(List(ConstantToken(2), MultiplyToken, ConstantToken(5)))
  }

  it should "lex 2//5" in {
    val result = ExpressionLexer.apply("2//5")
    result mustBe Right(List(ConstantToken(2), FractionalDivideToken, ConstantToken(5)))
  }

  it should "lex 2/5" in {
    val result = ExpressionLexer.apply("2/5")
    result mustBe Right(List(ConstantToken(2), DivideToken, ConstantToken(5)))
  }

  it should "lex 2%5" in {
    val result = ExpressionLexer.apply("2%5")
    result mustBe Right(List(ConstantToken(2), RemainderToken, ConstantToken(5)))
  }

  it should "lex 2<<5" in {
    val result = ExpressionLexer.apply("2<<5")
    result mustBe Right(List(ConstantToken(2), LeftShiftToken, ConstantToken(5)))
  }

  it should "lex 2>>5" in {
    val result = ExpressionLexer.apply("2>>5")
    result mustBe Right(List(ConstantToken(2), RightShiftToken, ConstantToken(5)))
  }

  it should "lex 2&5" in {
    val result = ExpressionLexer.apply("2&5")
    result mustBe Right(List(ConstantToken(2), BitwiseAndToken, ConstantToken(5)))
  }

  it should "lex 2+5" in {
    val result = ExpressionLexer.apply("2+5")
    result mustBe Right(List(ConstantToken(2), AdditionToken, ConstantToken(5)))
  }

  it should "lex 2-5" in {
    val result = ExpressionLexer.apply("2-5")
    result mustBe Right(List(ConstantToken(2), SubtractionToken, ConstantToken(5)))
  }

  it should "lex 2|5" in {
    val result = ExpressionLexer.apply("2|5")
    result mustBe Right(List(ConstantToken(2), BitwiseOrToken, ConstantToken(5)))
  }

  it should "lex 2^5" in {
    val result = ExpressionLexer.apply("2^5")
    result mustBe Right(List(ConstantToken(2), BitwiseExclusiveOrToken, ConstantToken(5)))
  }

  it should "lex 2+(4-3)" in {
    val result = ExpressionLexer.apply("2+(4-3)")
    result mustBe Right(List(ConstantToken(2), AdditionToken,
      ParenthesisedExpression(List(ConstantToken(4), SubtractionToken, ConstantToken(3)))))
  }

  it should "lex 2+((5*(9/(1+2))-2)-3)" in {
    val result = ExpressionLexer.apply("2+((5*(9/(1+2))-2)-3)")
    result mustBe Right(
      List(
        ConstantToken(2),
        AdditionToken,
        ParenthesisedExpression(
          List(
            ParenthesisedExpression(
              List(
                ConstantToken(5),
                MultiplyToken,
                ParenthesisedExpression(
                  List(
                    ConstantToken(9),
                    DivideToken,
                    ParenthesisedExpression(
                      List(
                        ConstantToken(1),
                        AdditionToken,
                        ConstantToken(2)
                      )
                    )
                  )
                ),
                SubtractionToken,
                ConstantToken(2)
              )
            ),
            SubtractionToken,
            ConstantToken(3)
          )
        )
      )
    )
  }

  it should "lex 2+5*6-4/3" in {
    val result = ExpressionLexer.apply("2+5*6-4/3")
    result mustBe Right(List(ConstantToken(2), AdditionToken, ConstantToken(5), MultiplyToken, ConstantToken(6),
      SubtractionToken, ConstantToken(4), DivideToken, ConstantToken(3)))
  }

  it should "lex #ab<<32+42&~(42-1)" in {
    val result = ExpressionLexer.apply("#ab<<32+42&~(42-1)")
    result mustBe Right(List(
      ConstantToken(0xab),
      LeftShiftToken,
      ConstantToken(32),
      AdditionToken,
      ConstantToken(42),
      BitwiseAndToken,
      ParenthesisedExpression(
        List(
          ConstantToken(42),
          SubtractionToken,
          ConstantToken(1)
        ),
        unaryOperator = ComplementationToken
      )
    ))
  }

}
