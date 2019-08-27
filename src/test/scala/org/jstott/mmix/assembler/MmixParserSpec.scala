package org.jstott.mmix.assembler

import org.scalatest.{ FlatSpec, MustMatchers }

class MmixParserSpec extends FlatSpec with MustMatchers {

  behavior of "MixParser"

  it should "parse load operations (LDx)" in {
    val line =
      """foo LDB
        |    LDBU
        |    LDWU
        |    LDW
        |    LDT
        |    LDTU
        |    LDOU
        |    LDO
        |    LDA
        |    LDHT
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), LdbToken),
        MmixProgramLine(None, LdbuToken),
        MmixProgramLine(None, LdwuToken),
        MmixProgramLine(None, LdwToken),
        MmixProgramLine(None, LdtToken),
        MmixProgramLine(None, LdtuToken),
        MmixProgramLine(None, LdouToken),
        MmixProgramLine(None, LdoToken),
        MmixProgramLine(None, LdaToken),
        MmixProgramLine(None, LdhtToken)
      )
    )
    result mustBe expected
  }

  it should "parse store operations (STx)" in {
    val line =
      """foo STB
        |    STBU
        |    STWU
        |    STW
        |    STT
        |    STTU
        |    STOU
        |    STO
        |    STCO
        |    STHT
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), StbToken),
        MmixProgramLine(None, StbuToken),
        MmixProgramLine(None, StwuToken),
        MmixProgramLine(None, StwToken),
        MmixProgramLine(None, SttToken),
        MmixProgramLine(None, SttuToken),
        MmixProgramLine(None, StouToken),
        MmixProgramLine(None, StoToken),
        MmixProgramLine(None, StcoToken),
        MmixProgramLine(None, SthtToken)
      )
    )
    result mustBe expected
  }

  it should "parse arithmetic operations" in {
    val line =
      """foo ADD
        |    ADDU
        |    SUB
        |    SUBU
        |    MUL
        |    MULU
        |    DIV
        |    DIVU
        |    2ADDU
        |    4ADDU
        |    8ADDU
        |    16ADDU
        |    NEG
        |    NEGU
        |    SL
        |    SLU
        |    SR
        |    SRU
        |    CMP
        |    CMPU
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), AddToken),
        MmixProgramLine(None, AdduToken),
        MmixProgramLine(None, SubToken),
        MmixProgramLine(None, SubuToken),
        MmixProgramLine(None, MulToken),
        MmixProgramLine(None, MuluToken),
        MmixProgramLine(None, DivToken),
        MmixProgramLine(None, DivuToken),
        MmixProgramLine(None, Addu2Token),
        MmixProgramLine(None, Addu4Token),
        MmixProgramLine(None, Addu8Token),
        MmixProgramLine(None, Addu16Token),
        MmixProgramLine(None, NegToken),
        MmixProgramLine(None, NeguToken),
        MmixProgramLine(None, SlToken),
        MmixProgramLine(None, SluToken),
        MmixProgramLine(None, SrToken),
        MmixProgramLine(None, SruToken),
        MmixProgramLine(None, CmpToken),
        MmixProgramLine(None, CmpuToken)
      )
    )
    result mustBe expected
  }

  it should "parse conditional operations (CSx/ZSx)" in {
    val line =
      """foo CSN
        |    CSZ
        |    CSP
        |    CSOD
        |    CSNN
        |    CSNZ
        |    CSNP
        |    CSEV
        |    ZSN
        |    ZSZ
        |    ZSP
        |    ZSOD
        |    ZSNN
        |    ZSNZ
        |    ZSNP
        |    ZSEV
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), CsnToken),
        MmixProgramLine(None, CszToken),
        MmixProgramLine(None, CspToken),
        MmixProgramLine(None, CsodToken),
        MmixProgramLine(None, CsnnToken),
        MmixProgramLine(None, CsnzToken),
        MmixProgramLine(None, CsnpToken),
        MmixProgramLine(None, CsevToken),
        MmixProgramLine(None, ZsnToken),
        MmixProgramLine(None, ZszToken),
        MmixProgramLine(None, ZspToken),
        MmixProgramLine(None, ZsodToken),
        MmixProgramLine(None, ZsnnToken),
        MmixProgramLine(None, ZsnzToken),
        MmixProgramLine(None, ZsnpToken),
        MmixProgramLine(None, ZsevToken)
      )
    )
    result mustBe expected
  }

  it should "parse bitwise operations" in {
    val line =
      """foo AND
        |    OR
        |    XOR
        |    ANDN
        |    ORN
        |    NAND
        |    NOR
        |    NXOR
        |    MUX
        |    SADD
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), AndToken),
        MmixProgramLine(None, OrToken),
        MmixProgramLine(None, XorToken),
        MmixProgramLine(None, AndnToken),
        MmixProgramLine(None, OrnToken),
        MmixProgramLine(None, NandToken),
        MmixProgramLine(None, NorToken),
        MmixProgramLine(None, NxorToken),
        MmixProgramLine(None, MuxToken),
        MmixProgramLine(None, SaddToken)
      )
    )
    result mustBe expected
  }

}
