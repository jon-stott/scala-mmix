package org.jstott.mmix.assembler

case class Expression(tokens: List[ExpressionToken] = List.empty) {

  def evaluate(state: AssemblerState): PrimaryToken = {
    tokens.foldLeft[(PrimaryToken, Option[BinaryOperatorToken])]((ConstantToken(0), None)) {

      case ((n0, Some(t)), n1 @ ConstantToken(_, _)) =>
        (t.evaluate(n0, n1), None)

      case ((n0, Some(t)), SymbolToken(l, uo)) if state.labels.get(SymbolToken(l)).nonEmpty =>
        (t.evaluate(n0, state.labels(SymbolToken(l))), None)

      case ((n0, Some(t)), SymbolToken(l, uo)) if state.mmix.registers.get(l).nonEmpty =>
        (t.evaluate(n0, ConstantToken(state.mmix.registers.get(l).get.serial)), None)

      case ((n0, Some(t)), CurrentLocationToken) =>
        (t.evaluate(n0, ConstantToken(state.currentLocation)), None)

      case ((n0, Some(t)), ParenthesisedExpression(es, _)) =>
        (t.evaluate(n0, Expression(es).evaluate(state)), None)

      case ((_, None), n1 @ ConstantToken(_, _)) =>
        (n1, None)

      case ((_, None), ParenthesisedExpression(es, _)) =>
        (Expression(es).evaluate(state), None)

      case ((_, None), SymbolToken(SymbolToken.LocalSymbolPattern(d, bfh), uo)) if bfh == "B" || bfh == "F" =>
        state.findLocalSymbolAddress(d.toInt, bfh) match {
          case Left(err) => println(d + bfh); ???
          case Right(location) => (ConstantToken(location), None)
        }

      case ((_, None), s @ SymbolToken(_, _)) if state.labels.get(s).nonEmpty =>
        state.labels.get(s) match {
          case Some(labelVal) => (labelVal, None)
          case _              => println(s); ???
        }

      case ((_, None), SymbolToken(l, _)) if state.mmix.registers.get(l).nonEmpty =>
        (ConstantToken(state.mmix.registers.get(l).get.serial), None)

      case ((_, None), s @ StringToken(_, _)) =>
        (s, None)

      case ((_, None), CurrentLocationToken) =>
        (CurrentLocationToken, None)

      case ((n0, _), t: BinaryOperatorToken) =>
        (n0, Some(t))

    }._1
  }

  override def toString: String = tokens.mkString

}
