package org.jstott.mmix.assembler

import org.scalatest.{ FlatSpec, MustMatchers }

class MmixParserSpec extends FlatSpec with MustMatchers {

  behavior of "MixParser"

  it should "parse LDx" in {
    val line =
      """foo LDB
        |    LDBU
        |    LDWU
        |    LDW
        |    LDT
        |    LDTU
        |    LDOU
        |    LDO
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
        MmixProgramLine(None, LdoToken)
      )
    )
    result mustBe expected
  }

}
