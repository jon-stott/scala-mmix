package org.jstott.mmix.assembler

sealed trait MmixLine

case class MmixProgramLine(
                            label: Option[LabelToken],
                            opcode: OperationToken,
                            expr: List[Expression] = List.empty
                          ) extends MmixLine

case class Expression(tokens: List[ExpressionToken] = List.empty)