package org.jstott.mmix.assembler

import org.jstott.mmix.MmixByte

sealed trait MmixAssemblerToken

case object WhitespaceToken extends MmixAssemblerToken
case object NewLineToken extends MmixAssemblerToken

case class LabelToken(s: String) extends MmixAssemblerToken

sealed trait OperationToken extends MmixAssemblerToken {
  def opcode: MmixByte
}

case object FcmpToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x01) }
case object FunToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x02) }
case object FeqlToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x03) }
case object FaddToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x04) }
case object FixToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x05) }
case object FsubToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x06) }
case object FixuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x07) }
case object FlotToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x08) }
case object FlotuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x0a) }
case object SflotToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x0c) }
case object SflotuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x0e) }

case object FmulToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x10) }
case object FcmpeToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x11) }
case object FuneToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x12) }
case object FeqleToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x13) }
case object FdivToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x14) }
case object FsqrtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x15) }
case object FremToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x16) }
case object FintToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x17) }
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

case object CsnToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x60) }
case object CszToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x62) }
case object CspToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x64) }
case object CsodToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x66) }
case object CsnnToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x68) }
case object CsnzToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x6a) }
case object CsnpToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x6c) }
case object CsevToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x6e) }

case object ZsnToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x70) }
case object ZszToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x72) }
case object ZspToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x74) }
case object ZsodToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x76) }
case object ZsnnToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x78) }
case object ZsnzToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x7a) }
case object ZsnpToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x7c) }
case object ZsevToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x7e) }

case object LdbToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x80) }
case object LdbuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x82) }
case object LdwToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x84) }
case object LdwuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x86) }
case object LdtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x88) }
case object LdtuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8a) }
case object LdoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8c) }
case object LdouToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x8e) }

case object LdsfToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x90) }
case object LdhtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0x92) }

case object StbToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa0) }
case object StbuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa2) }
case object StwToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa4) }
case object StwuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa6) }
case object SttToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xa8) }
case object SttuToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xaa) }
case object StoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xac) }
case object StouToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xae) }

case object StsfToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xb0) }
case object SthtToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xb2) }
case object StcoToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xb4) }

case object OrToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xc0) }
case object OrnToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xc2) }
case object NorToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xc4) }
case object XorToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xc6) }
case object AndToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xc8) }
case object AndnToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xca) }
case object NandToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xcc) }
case object NxorToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xce) }

case object BdifToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xd0) }
case object WdifToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xd2) }
case object TdifToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xd4) }
case object OdifToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xd6) }
case object MuxToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xd8) }
case object SaddToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xda) }
case object MorToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xdc) }
case object MxorToken extends OperationToken { override def opcode: MmixByte = MmixByte(0xde) }
