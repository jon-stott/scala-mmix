package org.jstott.mmix.assembler

import org.jstott.mmix.{ MmixByte, MmixTetra }

sealed trait MmixAssemblerToken

case object WhitespaceToken extends MmixAssemblerToken
case object NewLineToken extends MmixAssemblerToken

case class LabelToken(s: String) extends MmixAssemblerToken

sealed trait OperationToken extends MmixAssemblerToken {

  def opcode: MmixByte
  def opBits: MmixTetra = MmixTetra(0)

  final def isRelativeAddress: Boolean = checkBit(OperatorToken.relAddrBit)
  final def zMustBeRegister: Boolean = checkBit(OperatorToken.zrBit)
  final def yMustBeRegister: Boolean = checkBit(OperatorToken.yrBit)
  final def xMustBeRegister: Boolean = checkBit(OperatorToken.xrBit)
  final def isImmediate: Boolean = checkBit(OperatorToken.immedBit)
  final def canBeUsedInSpecialMode: Boolean = checkBit(OperatorToken.specBit)
  final def isLabelIgnored: Boolean = checkBit(OperatorToken.noLabelBit)
  final def isOneArg: Boolean = checkBit(OperatorToken.oneArgBit)
  final def isTwoArg: Boolean = checkBit(OperatorToken.twoArgBit)
  final def isThreeArg: Boolean = checkBit(OperatorToken.threeArgBit)
  final def isManyArg: Boolean = checkBit(OperatorToken.manyArgBit)
  final def canZBeRegister: Boolean = checkBit(OperatorToken.immedBit + OperatorToken.zrBit + OperatorToken.zarBit)
  final def mustZBeRegister: Boolean = checkBit(OperatorToken.zrBit)
  final def canYBeRegister: Boolean = checkBit(OperatorToken.yrBit + OperatorToken.yarBit)
  final def mustYBeRegister: Boolean = checkBit(OperatorToken.yrBit)
  final def canXBeRegister: Boolean = checkBit(OperatorToken.xrBit + OperatorToken.zarBit)
  final def mustXBeRegister: Boolean = checkBit(OperatorToken.xrBit)
  final def canYZBeRegister: Boolean = checkBit(OperatorToken.immedBit + OperatorToken.yzrBit + OperatorToken.yzarBit)
  final def mustYZBeRegister: Boolean = checkBit(OperatorToken.yzrBit)
  final def mustYZBeMemoryLocation: Boolean = checkBit(OperatorToken.memBit)
  final def canXYZBeRegister: Boolean = checkBit(OperatorToken.xyzrBit + OperatorToken.xyzarBit)
  final def mustXYZBeRegister: Boolean = checkBit(OperatorToken.xyzrBit)

  private def checkBit(bit: Int): Boolean = (opBits.t & bit) > 0

}

sealed trait AssemblerToken extends OperationToken
sealed trait OperatorToken extends OperationToken

object OperatorToken {

  val relAddrBit = 0x1
  val immedBit = 0x2
  val zarBit = 0x4
  val zrBit = 0x8
  val yarBit = 0x10
  val yrBit = 0x20
  val xrBit = 0x80
  val yzarBit = 0x100
  val yzrBit = 0x200
  val xyzarBit = 0x400
  val xyzrBit = 0x800
  val oneArgBit = 0x1000
  val twoArgBit = 0x2000
  val threeArgBit = 0x4000
  val manyArgBit = 0x8000
  val alignBits = 0x30000
  val noLabelBit = 0x40000
  val memBit = 0x80000
  val specBit = 0x100000

}

case object IsToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x101400)
  override def toString: String = "IS"
}
case object LocToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x1400)
  override def toString: String = "LOC"
}
case object GregToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x101000)
  override def toString: String = "GREG"
}
case object ByteToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x10f000)
  override def toString: String = "BYTE"
}
case object WydeToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x11f000)
  override def toString: String = "WYDE"
}
case object TetraToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x12f000)
  override def toString: String = "TETRA"
}
case object OctaToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(-1)
  override def opBits: MmixTetra = MmixTetra(0x13f000)
  override def toString: String = "OCTA"
}
case object SetToken extends AssemblerToken {
  override def opcode: MmixByte = MmixByte(0x100)
  override def opBits: MmixTetra = MmixTetra(0x22180)
  override def toString: String = "SET"
}

case object TrapToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x00)
  override def opBits: MmixTetra = MmixTetra(0x27554)
  override def toString: String = "TRAP"
}
case object FcmpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x01)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FCMP"
}
case object FunToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x02)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FUN"
}
case object FeqlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x03)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FEQL"
}
case object FaddToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x04)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FADD"
}
case object FixToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x05)
  override def opBits: MmixTetra = MmixTetra(0x26288)
  override def toString: String = "FIX"
}
case object FsubToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x06)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FSUB"
}
case object FixuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x07)
  override def opBits: MmixTetra = MmixTetra(0x26288)
  override def toString: String = "FIXU"
}
case object FlotToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x08)
  override def opBits: MmixTetra = MmixTetra(0x26282)
  override def toString: String = "FLOT"
}
case object FlotuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x0a)
  override def opBits: MmixTetra = MmixTetra(0x26282)
  override def toString: String = "FLOTU"
}
case object SflotToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x0c)
  override def opBits: MmixTetra = MmixTetra(0x26282)
  override def toString: String = "SFLOT"
}
case object SflotuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x0e)
  override def opBits: MmixTetra = MmixTetra(0x26282)
  override def toString: String = "SFLOTU"
}

case object FmulToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x10)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FMUL"
}
case object FcmpeToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x11)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FCMPE"
}
case object FuneToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x12)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FUNE"
}
case object FeqleToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x13)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FEQLE"
}
case object FdivToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x14)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FDIV"
}
case object FsqrtToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x15)
  override def opBits: MmixTetra = MmixTetra(0x26288)
  override def toString: String = "FSQRT"
}
case object FremToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x16)
  override def opBits: MmixTetra = MmixTetra(0x240a8)
  override def toString: String = "FREM"
}
case object FintToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x17)
  override def opBits: MmixTetra = MmixTetra(0x26288)
  override def toString: String = "FINT"
}
case object MulToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x18)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "MUL"
}
case object MuluToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x1a)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "MULU"
}
case object DivToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x1c)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "DIV"
}
case object DivuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x1e)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "DIVU"
}

case object AddToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x20)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ADD"
}
case object AdduToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x22)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ADDU"
}
case object LdaToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x22)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDA"
}
case object SubToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x24)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SUB"
}
case object SubuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x26)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SUBU"
}
case object Addu2Token extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x28)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "2ADDU"
}
case object Addu4Token extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x2a)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "4ADDU"
}
case object Addu8Token extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x2c)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "8ADDU"
}
case object Addu16Token extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x2e)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "16ADDU"
}

case object CmpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x30)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CMP"
}
case object CmpuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x32)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CMPU"
}
case object NegToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x34)
  override def opBits: MmixTetra = MmixTetra(0x26082)
  override def toString: String = "NEG"
}
case object NeguToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x36)
  override def opBits: MmixTetra = MmixTetra(0x26082)
  override def toString: String = "NEGU"
}
case object SlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x38)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SL"
}
case object SluToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x3a)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SLU"
}
case object SrToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x3c)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SR"
}
case object SruToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x3e)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SRU"
}

case object BnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x40)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BZ"
}
case object BzToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x42)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BZ"
}
case object BpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x44)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BP"
}
case object BodToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x46)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BOD"
}
case object BnnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x48)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BNN"
}
case object BnzToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x4a)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BNZ"
}
case object BnpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x4c)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BNP"
}
case object BevToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x4e)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "BEV"
}

case object PbnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x50)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBN"
}
case object PbzToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x52)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBZ"
}
case object PbpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x54)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBP"
}
case object PbodToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x56)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBOD"
}
case object PbnnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x58)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBNN"
}
case object PbnzToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x5a)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBNZ"
}
case object PbnpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x5c)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBNP"
}
case object PbevToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x5e)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "PBEV"
}

case object CsnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x60)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSN"
}
case object CszToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x62)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSZ"
}
case object CspToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x64)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSP"
}
case object CsodToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x66)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSOD"
}
case object CsnnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x68)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSNN"
}
case object CsnzToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x6a)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSNZ"
}
case object CsnpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x6c)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSNP"
}
case object CsevToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x6e)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "CSEV"
}

case object ZsnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x70)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSN"
}
case object ZszToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x72)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSZ"
}
case object ZspToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x74)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSP"
}
case object ZsodToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x76)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSOD"
}
case object ZsnnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x78)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSNN"
}
case object ZsnzToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x7a)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSNZ"
}
case object ZsnpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x7c)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSNP"
}
case object ZsevToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x7e)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ZSEV"
}

case object LdbToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x80)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "lDB"
}
case object LdbuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x82)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDBU"
}
case object LdwToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x84)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDW"
}
case object LdwuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x86)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDWU"
}
case object LdtToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x88)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDT"
}
case object LdtuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x8a)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDTU"
}
case object LdoToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x8c)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDO"
}
case object LdouToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x8e)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDOU"
}

case object LdsfToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x90)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDSF"
}
case object LdhtToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x92)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDHT"
}
case object CswapToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x94)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "CSWAP"
}
case object LduncToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x96)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDUNC"
}
case object LdvtsToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x98)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "LDVTS"
}
case object PreldToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x9a)
  override def opBits: MmixTetra = MmixTetra(0xa6022)
  override def toString: String = "PRELD"
}
case object PregoToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x9c)
  override def opBits: MmixTetra = MmixTetra(0xa6022)
  override def toString: String = "PREGO"
}
case object GoToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0x9e)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "GO"
}

case object StbToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xa0)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STB"
}
case object StbuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xa2)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STBU"
}
case object StwToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xa4)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STW"
}
case object StwuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xa6)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STWU"
}
case object SttToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xa8)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STT"
}
case object SttuToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xaa)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STTU"
}
case object StoToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xac)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STO"
}
case object StouToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xae)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STOU"
}

case object StsfToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xb0)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STSF"
}
case object SthtToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xb2)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STHT"
}
case object StcoToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xb4)
  override def opBits: MmixTetra = MmixTetra(0xa6022)
  override def toString: String = "STCO"
}
case object StuncToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xb6)
  override def opBits: MmixTetra = MmixTetra(0xa60a2)
  override def toString: String = "STUNC"
}
case object SyncdToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xb8)
  override def opBits: MmixTetra = MmixTetra(0xa6022)
  override def toString: String = "SYNCD"
}
case object PrestToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xba)
  override def opBits: MmixTetra = MmixTetra(0xa6022)
  override def toString: String = "PREST"
}
case object SyncidToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xbc)
  override def opBits: MmixTetra = MmixTetra(0xa6022)
  override def toString: String = "SYNCID"
}
case object PushgoToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xbe)
  override def opBits: MmixTetra = MmixTetra(0xa6062)
  override def toString: String = "PUSHGO"
}

case object OrToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xc0)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "OR"
}
case object OrnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xc2)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ORN"
}
case object NorToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xc4)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "NOR"
}
case object XorToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xc6)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "XOR"
}
case object AndToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xc8)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "AND"
}
case object AndnToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xca)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ANDN"
}
case object NandToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xcc)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "NAND"
}
case object NxorToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xce)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "NXOR"
}

case object BdifToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xd0)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "BDIF"
}
case object WdifToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xd2)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "WDIF"
}
case object TdifToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xd4)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "TDIF"
}
case object OdifToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xd6)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "ODIF"
}
case object MuxToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xd8)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "MUX"
}
case object SaddToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xda)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "SADD"
}
case object MorToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xdc)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "MOR"
}
case object MxorToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xde)
  override def opBits: MmixTetra = MmixTetra(0x240a2)
  override def toString: String = "MXOR"
}

case object SethToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe0)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "SETH"
}
case object SetmhToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe1)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "SETMH"
}
case object SetmlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe2)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "SETML"
}
case object SetlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe3)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "SETL"
}
case object InchToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe4)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "INCH"
}
case object IncmhToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe5)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "INCMH"
}
case object IncmlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe6)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "INCML"
}
case object InclToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe7)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "INCL"
}
case object OrhToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe8)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ORH"
}
case object OrmhToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xe9)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ORMH"
}
case object OrmlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xea)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ORML"
}
case object OrlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xeb)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ORL"
}
case object AndnhToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xec)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ANDNH"
}
case object AndnmhToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xed)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ANDNMH"
}
case object AndnmlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xee)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ANDNML"
}
case object AndnlToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xef)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "ANDNL"
}

case object JmpToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xf0)
  override def opBits: MmixTetra = MmixTetra(0x21001)
  override def toString: String = "JMP"
}
case object PushjToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xf2)
  override def opBits: MmixTetra = MmixTetra(0x22041)
  override def toString: String = "PUSHJ"
}
case object GetaToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xf4)
  override def opBits: MmixTetra = MmixTetra(0x22081)
  override def toString: String = "GETA"
}
case object PutToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xf6)
  override def opBits: MmixTetra = MmixTetra(0x22002)
  override def toString: String = "PUT"
}
case object PopToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xf8)
  override def opBits: MmixTetra = MmixTetra(0x23000)
  override def toString: String = "POP"
}
case object ResumeToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xf9)
  override def opBits: MmixTetra = MmixTetra(0x21000)
  override def toString: String = "RESUME"
}
case object SaveToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xfa)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "SAVE"
}
case object UnsaveToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xfb)
  override def opBits: MmixTetra = MmixTetra(0x23a00)
  override def toString: String = "UNSAVE"
}
case object SyncToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xfc)
  override def opBits: MmixTetra = MmixTetra(0x21000)
  override def toString: String = "SYNC"
}
case object SwymToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xfd)
  override def opBits: MmixTetra = MmixTetra(0x27554)
  override def toString: String = "SWYM"
}
case object GetToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xfe)
  override def opBits: MmixTetra = MmixTetra(0x22080)
  override def toString: String = "GET"
}
case object TripToken extends OperatorToken {
  override def opcode: MmixByte = MmixByte(0xff)
  override def opBits: MmixTetra = MmixTetra(0x27554)
  override def toString: String = "TRIP"
}
