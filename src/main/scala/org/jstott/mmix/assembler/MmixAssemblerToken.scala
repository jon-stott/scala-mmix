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

case object TrapToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x00) }
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

case object BnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x40) }
case object BzToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x42) }
case object BpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x44) }
case object BodToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x46) }
case object BnnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x48) }
case object BnzToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x4a) }
case object BnpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x4c) }
case object BevToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x4e) }

case object PbnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x50) }
case object PbzToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x52) }
case object PbpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x54) }
case object PbodToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x56) }
case object PbnnToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x58) }
case object PbnzToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x5a) }
case object PbnpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x5c) }
case object PbevToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x5e) }

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
case object CswapToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x94) }
case object LduncToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x96) }
case object LdvtsToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x98) }
case object PreldToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x9a) }
case object PregoToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x9c) }
case object GoToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0x9e) }

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
case object StuncToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xb6) }
case object SyncdToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xb8) }
case object PrestToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xba) }
case object SyncidToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xbc) }
case object PushgoToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xbe) }

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

case object SethToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe0) }
case object SetmhToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe1) }
case object SetmlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe2) }
case object SetlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe3) }
case object InchToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe4) }
case object IncmhToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe5) }
case object IncmlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe6) }
case object InclToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe7) }
case object OrhToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe8) }
case object OrmhToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xe9) }
case object OrmlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xea) }
case object OrlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xeb) }
case object AndnhToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xec) }
case object AndnmhToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xed) }
case object AndnmlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xee) }
case object AndnlToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xef) }

case object JmpToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xf0) }
case object PushjToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xf2) }
case object PopToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xf8) }
case object ResumeToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xf9) }
case object SaveToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xfa) }
case object UnsaveToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xfb) }
case object SyncToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xfc) }
case object TripToken extends OperatorToken { override def opcode: MmixByte = MmixByte(0xff) }
