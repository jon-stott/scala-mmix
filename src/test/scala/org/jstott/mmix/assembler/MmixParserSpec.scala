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

  it should "parse bytewise operations" in {
    val line =
      """foo BDIF
        |    WDIF
        |    TDIF
        |    ODIF
        |    MOR
        |    MXOR
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), BdifToken),
        MmixProgramLine(None, WdifToken),
        MmixProgramLine(None, TdifToken),
        MmixProgramLine(None, OdifToken),
        MmixProgramLine(None, MorToken),
        MmixProgramLine(None, MxorToken)
      )
    )
    result mustBe expected
  }

  it should "parse floating point operations" in {
    val line =
      """foo FADD
        |    FSUB
        |    FMUL
        |    FDIV
        |    FREM
        |    FSQRT
        |    FINT
        |    FCMP
        |    FEQL
        |    FUN
        |    FCMPE
        |    FEQLE
        |    FUNE
        |    FIX
        |    FIXU
        |    FLOT
        |    FLOTU
        |    SFLOT
        |    SFLOTU
        |    LDSF
        |    STSF
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), FaddToken),
        MmixProgramLine(None, FsubToken),
        MmixProgramLine(None, FmulToken),
        MmixProgramLine(None, FdivToken),
        MmixProgramLine(None, FremToken),
        MmixProgramLine(None, FsqrtToken),
        MmixProgramLine(None, FintToken),
        MmixProgramLine(None, FcmpToken),
        MmixProgramLine(None, FeqlToken),
        MmixProgramLine(None, FunToken),
        MmixProgramLine(None, FcmpeToken),
        MmixProgramLine(None, FeqleToken),
        MmixProgramLine(None, FuneToken),
        MmixProgramLine(None, FixToken),
        MmixProgramLine(None, FixuToken),
        MmixProgramLine(None, FlotToken),
        MmixProgramLine(None, FlotuToken),
        MmixProgramLine(None, SflotToken),
        MmixProgramLine(None, SflotuToken),
        MmixProgramLine(None, LdsfToken),
        MmixProgramLine(None, StsfToken)
      )
    )
    result mustBe expected
  }

  it should "parse immediate constant operations" in {
    val line =
      """foo SETH
        |    SETMH
        |    SETML
        |    SETL
        |    INCH
        |    INCMH
        |    INCML
        |    INCL
        |    ORH
        |    ORMH
        |    ORML
        |    ORL
        |    ANDNH
        |    ANDNMH
        |    ANDNML
        |    ANDNL
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), SethToken),
        MmixProgramLine(None, SetmhToken),
        MmixProgramLine(None, SetmlToken),
        MmixProgramLine(None, SetlToken),
        MmixProgramLine(None, InchToken),
        MmixProgramLine(None, IncmhToken),
        MmixProgramLine(None, IncmlToken),
        MmixProgramLine(None, InclToken),
        MmixProgramLine(None, OrhToken),
        MmixProgramLine(None, OrmhToken),
        MmixProgramLine(None, OrmlToken),
        MmixProgramLine(None, OrlToken),
        MmixProgramLine(None, AndnhToken),
        MmixProgramLine(None, AndnmhToken),
        MmixProgramLine(None, AndnmlToken),
        MmixProgramLine(None, AndnlToken)
      )
    )
    result mustBe expected
  }

  it should "parse jump and branch operations" in {
    val line =
      """foo JMP
        |    GO
        |    BN
        |    BZ
        |    BP
        |    BOD
        |    BNN
        |    BNZ
        |    BNP
        |    BEV
        |    PBN
        |    PBZ
        |    PBP
        |    PBOD
        |    PBNN
        |    PBNZ
        |    PBNP
        |    PBEV
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), JmpToken),
        MmixProgramLine(None, GoToken),
        MmixProgramLine(None, BnToken),
        MmixProgramLine(None, BzToken),
        MmixProgramLine(None, BpToken),
        MmixProgramLine(None, BodToken),
        MmixProgramLine(None, BnnToken),
        MmixProgramLine(None, BnzToken),
        MmixProgramLine(None, BnpToken),
        MmixProgramLine(None, BevToken),
        MmixProgramLine(None, PbnToken),
        MmixProgramLine(None, PbzToken),
        MmixProgramLine(None, PbpToken),
        MmixProgramLine(None, PbodToken),
        MmixProgramLine(None, PbnnToken),
        MmixProgramLine(None, PbnzToken),
        MmixProgramLine(None, PbnpToken),
        MmixProgramLine(None, PbevToken)
      )
    )
    result mustBe expected
  }

  it should "parse subroutine call operations" in {
    val line =
      """foo PUSHJ
        |    PUSHGO
        |    POP
        |    SAVE
        |    UNSAVE
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), PushjToken),
        MmixProgramLine(None, PushgoToken),
        MmixProgramLine(None, PopToken),
        MmixProgramLine(None, SaveToken),
        MmixProgramLine(None, UnsaveToken)
      )
    )
    result mustBe expected
  }

  it should "parse system operations" in {
    val line =
      """foo LDUNC
        |    STUNC
        |    PRELD
        |    PREST
        |    PREGO
        |    SYNCID
        |    SYNCD
        |    SYNC
        |    CSWAP
        |    LDVTS
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), LduncToken),
        MmixProgramLine(None, StuncToken),
        MmixProgramLine(None, PreldToken),
        MmixProgramLine(None, PrestToken),
        MmixProgramLine(None, PregoToken),
        MmixProgramLine(None, SyncidToken),
        MmixProgramLine(None, SyncdToken),
        MmixProgramLine(None, SyncToken),
        MmixProgramLine(None, CswapToken),
        MmixProgramLine(None, LdvtsToken),
      )
    )
    result mustBe expected
  }

  it should "parse interrupt operations" in {
    val line =
      """foo TRIP
        |    TRAP
        |    RESUME
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), TripToken),
        MmixProgramLine(None, TrapToken),
        MmixProgramLine(None, ResumeToken)
      )
    )
    result mustBe expected
  }

  it should "parse other operations" in {
    val line =
      """foo GET
        |    PUT
        |    GETA
        |    SWYM
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), GetToken),
        MmixProgramLine(None, PutToken),
        MmixProgramLine(None, GetaToken),
        MmixProgramLine(None, SwymToken)
      )
    )
    result mustBe expected
  }

  it should "parse assembler tokens" in {
    val line =
      """foo IS
        |    LOC
        |    GREG
        |    BYTE
        |    WYDE
        |    TETRA
        |    OCTA
        |""".stripMargin
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(Some(LabelToken("foo")), IsToken),
        MmixProgramLine(None, LocToken),
        MmixProgramLine(None, GregToken),
        MmixProgramLine(None, ByteToken),
        MmixProgramLine(None, WydeToken),
        MmixProgramLine(None, TetraToken),
        MmixProgramLine(None, OctaToken)
      )
    )
    result mustBe expected
  }

  it should "parse assembler tokens with a single expression" in {
    val line = " JMP 1F"
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(None, JmpToken, List(
          Expression(List(SymbolToken("1F")))
        ))
      )
    )
    result mustBe expected
  }

  it should "parse assembler tokens with two expressions" in {
    val line = " PBNZ $0,1B"
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(None, PbnzToken, List(
          Expression(List(ConstantToken(0, RegisterizationToken))),
          Expression(List(SymbolToken("1B")))
        ))
      )
    )
    result mustBe expected
  }

  it should "parse assembler tokens with three expressions" in {
    val line = " LDO $3,x0,$0"
    val result = MmixParser.apply(line)
    val expected = Right(
      List(
        MmixProgramLine(None, LdoToken, List(
          Expression(List(ConstantToken(3, RegisterizationToken))),
          Expression(List(SymbolToken("x0"))),
          Expression(List(ConstantToken(0, RegisterizationToken)))
        ))
      )
    )
    result mustBe expected
  }

}
