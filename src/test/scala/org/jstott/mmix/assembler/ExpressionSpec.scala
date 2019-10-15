package org.jstott.mmix.assembler

import org.jstott.mmix.Mmix
import org.scalatest.{ FlatSpec, MustMatchers }

class ExpressionSpec extends FlatSpec with MustMatchers {

  private def fixture =
    new {
      val mmix: Mmix = Mmix()
      val state: AssemblerState = AssemblerState(mmix, currentLocation = 0x100)
    }

  behavior of "evaluate"

  it should "evaluate @" in {
    val expr = Expression(List(CurrentLocationToken))
    val expected = ConstantToken(0x100)
    val result = expr.evaluate(fixture.state)
    result mustBe expected
  }

  it should "evaluate 1+2" in {
    val expr = Expression(List(ConstantToken(1), AdditionToken, ConstantToken(2)))
    val expected = ConstantToken(3)
    val result = expr.evaluate(fixture.state)
    result mustBe expected
  }

  it should "evaluate (1+2)" in {
    val expr = Expression(List(ParenthesisedExpression(List(ConstantToken(1), AdditionToken, ConstantToken(2)))))
    val expected = ConstantToken(3)
    val result = expr.evaluate(fixture.state)
    result mustBe expected
  }

  it should "evaluate 1+2+3" in {
    val expr = Expression(List(ConstantToken(1), AdditionToken, ConstantToken(2), AdditionToken, ConstantToken(3)))
    val expected = ConstantToken(6)
    val result = expr.evaluate(fixture.state)
    result mustBe expected
  }

  it should "evaluate 1+(2+3)" in {
    val expr = Expression(List(ConstantToken(1), AdditionToken, ParenthesisedExpression(List(ConstantToken(2), AdditionToken, ConstantToken(3)))))
    val expected = ConstantToken(6)
    val result = expr.evaluate(fixture.state)
    result mustBe expected
  }

  it should "evaluate 1+(2*3)" in {
    val expr = Expression(List(ConstantToken(1), AdditionToken, ParenthesisedExpression(List(ConstantToken(2), MultiplyToken, ConstantToken(3)))))
    val expected = ConstantToken(7)
    val result = expr.evaluate(fixture.state)
    result mustBe expected
  }

  it should "evaluate PRIME1+2*L" in {
    val expr = Expression(List(SymbolToken("PRIME1"), AdditionToken, ParenthesisedExpression(List(ConstantToken(2), MultiplyToken, SymbolToken("L")))))
    val state = fixture.state.copy(
      labels = fixture.state.labels.
      updated(SymbolToken("PRIME1"), ConstantToken(BigInt("2000000000000000", 16))).
      updated(SymbolToken("L"), ConstantToken(BigInt(500)))
    )
    val expected = ConstantToken(BigInt("20000000000003e8", 16))
    val result = expr.evaluate(state)
    result mustBe expected
  }

}
