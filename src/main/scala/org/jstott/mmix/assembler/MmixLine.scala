package org.jstott.mmix.assembler

sealed trait MmixLine

case class MmixProgramLine(
                            label: Option[LabelToken],
                            opcode: OperationToken,
                            expr: List[Expression] = List.empty,
                            lineNumber: Int = 0
                          ) extends MmixLine {

  def containsForwardReference(state: AssemblerState): Boolean = {
    expr.exists { e =>
      e.tokens.exists {
        case SymbolToken(SymbolToken.LocalSymbolPattern(d, bfh), uo) =>
          // TODO handle unary operator
          state.findLocalSymbolAddress(d.toInt, bfh) match {
            case Left(_) => true
            case Right(_) => false
          }
        case s @ SymbolToken(_, _) if !state.labels.contains(s) && state.mmix.registers.get(s.s).isEmpty => true
        case _ => false
      }
    }
  }

  def evaluatedExpr(state: AssemblerState): List[PrimaryToken] = expr.map(_.evaluate(state))

  override def toString: String = {
    val labelStr = label match {
      case Some(l) => l.s
      case _ => " "
    }
    val exprStr = expr.mkString(",")
    f"$lineNumber%6d $labelStr%10s $opcode%6s $exprStr"
  }

}
