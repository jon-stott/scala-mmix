package org.jstott.mmix.assembler

import org.jstott.mmix.MmixByte

sealed trait MmixAssemblerToken

case object WhitespaceToken extends MmixAssemblerToken
case object NewLineToken extends MmixAssemblerToken

case class LabelToken(s: String) extends MmixAssemblerToken

sealed trait OperationToken extends MmixAssemblerToken {
  def opcode: MmixByte
}

case object LdbToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x80) }
case object LdbuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x82) }
case object LdwToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x84) }
case object LdwuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x86) }
case object LdtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x88) }
case object LdtuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8a) }
case object LdoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8c) }
case object LdouToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8e) }