package org.jstott.mmix.assembler

import org.jstott.mmix.MmixByte

sealed trait MmixAssemblerToken

case object WhitespaceToken extends MmixAssemblerToken
case object NewLineToken extends MmixAssemblerToken

case class LabelToken(s: String) extends MmixAssemblerToken

sealed trait OperationToken extends MmixAssemblerToken {
  def opcode: MmixByte
}

case object MulToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x18) }
case object MuluToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x1a) }
case object DivToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x1c) }
case object DivuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x1e) }

case object AddToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x20) }
case object AdduToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x22) }
case object LdaToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x22) }
case object SubToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x24) }
case object SubuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x26) }
case object Addu2Token extends OperationToken { override def opcode: MmixByte = MmixByte(0x28) }
case object Addu4Token extends OperationToken { override def opcode: MmixByte = MmixByte(0x2a) }
case object Addu8Token extends OperationToken { override def opcode: MmixByte = MmixByte(0x2c) }
case object Addu16Token extends OperationToken { override def opcode: MmixByte = MmixByte(0x2e) }

case object CmpToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x30) }
case object CmpuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x32) }
case object NegToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x34) }
case object NeguToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x36) }
case object SlToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x38) }
case object SluToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x3a) }
case object SrToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x3c) }
case object SruToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x3e) }

case object LdbToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x80) }
case object LdbuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x82) }
case object LdwToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x84) }
case object LdwuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x86) }
case object LdtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x88) }
case object LdtuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8a) }
case object LdoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8c) }
case object LdouToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8e) }

// 90
case object LdhtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x92) }

case object StbToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa0) }
case object StbuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa2) }
case object StwToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa4) }
case object StwuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa6) }
case object SttToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa8) }
case object SttuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xaa) }
case object StoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xac) }
case object StouToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xae) }

// b0
case object SthtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xb2) }
case object StcoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xb4) }
