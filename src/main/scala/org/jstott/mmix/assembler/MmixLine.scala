package org.jstott.mmix.assembler

sealed trait MmixLine

case class MmixProgramLine(label: Option[LabelToken], opcode: OperationToken) extends MmixLine
