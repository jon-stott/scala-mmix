package org.jstott.mmix.assembler

import org.jstott.mmix.MmixByte

sealed trait MmixAssemblerToken

case object WhitespaceToken extends MmixAssemblerToken
case object NewLineToken extends MmixAssemblerToken

case class LabelToken(s: String) extends MmixAssemblerToken

sealed trait OperationToken extends MmixAssemblerToken

sealed trait AssemblerToken extends OperationToken
sealed trait OperatorToken extends OperationToken {
  def opcode: MmixByte
}

case object IsToken extends AssemblerToken
case object LocToken extends AssemblerToken

case object FcmpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x01) }
case object FunToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x02) }
case object FeqlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x03) }
case object FaddToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x04) }
case object FixToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x05) }
case object FsubToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x06) }
case object FixuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x07) }
case object FlotToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x08) }
case object FlotuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x0a) }
case object SflotToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x0c) }
case object SflotuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x0e) }

case object FmulToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x10) }
case object FcmpeToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x11) }
case object FuneToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x12) }
case object FeqleToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x13) }
case object FdivToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x14) }
case object FsqrtToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x15) }
case object FremToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x16) }
case object FintToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x17) }
case object MulToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x18) }
case object MuluToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x1a) }
case object DivToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x1c) }
case object DivuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x1e) }

case object AddToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x20) }
case object AdduToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x22) }
case object LdaToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x22) }
case object SubToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x24) }
case object SubuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x26) }
case object Addu2Token extends OperatorToken { override def opcode: MmixByte = MmixByte(0x28) }
case object Addu4Token extends OperatorToken { override def opcode: MmixByte = MmixByte(0x2a) }
case object Addu8Token extends OperatorToken { override def opcode: MmixByte = MmixByte(0x2c) }
case object Addu16Token extends OperatorToken { override def opcode: MmixByte = MmixByte(0x2e) }

case object CmpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x30) }
case object CmpuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x32) }
case object NegToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x34) }
case object NeguToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x36) }
case object SlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x38) }
case object SluToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x3a) }
case object SrToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x3c) }
case object SruToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x3e) }

case object CsnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x60) }
case object CszToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x62) }
case object CspToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x64) }
case object CsodToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x66) }
case object CsnnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x68) }
case object CsnzToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x6a) }
case object CsnpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x6c) }
case object CsevToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x6e) }

case object ZsnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x70) }
case object ZszToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x72) }
case object ZspToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x74) }
case object ZsodToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x76) }
case object ZsnnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x78) }
case object ZsnzToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x7a) }
case object ZsnpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x7c) }
case object ZsevToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x7e) }

case object LdbToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x80) }
case object LdbuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x82) }
case object LdwToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x84) }
case object LdwuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x86) }
case object LdtToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x88) }
case object LdtuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x8a) }
case object LdoToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x8c) }
case object LdouToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x8e) }

case object LdsfToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x90) }
case object LdhtToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x92) }

case object StbToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xa0) }
case object StbuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xa2) }
case object StwToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xa4) }
case object StwuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xa6) }
case object SttToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xa8) }
case object SttuToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xaa) }
case object StoToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xac) }
case object StouToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xae) }

case object StsfToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xb0) }
case object SthtToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xb2) }
case object StcoToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xb4) }

case object OrToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xc0) }
case object OrnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xc2) }
case object NorToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xc4) }
case object XorToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xc6) }
case object AndToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xc8) }
case object AndnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xca) }
case object NandToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xcc) }
case object NxorToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xce) }

case object BdifToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xd0) }
case object WdifToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xd2) }
case object TdifToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xd4) }
case object OdifToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xd6) }
case object MuxToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xd8) }
case object SaddToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xda) }
case object MorToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xdc) }
case object MxorToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xde) }
