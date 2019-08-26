package org.jstott.mmix.assembler

import org.jstott.mmix.{ MmixByte, MmixOcta }

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object MmixParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def w: Parser[WhitespaceToken.type] = whiteSpace ^^ { s => WhitespaceToken }
  def newLine: Parser[NewLineToken.type] = "\n".r ^^ { s => NewLineToken }

  def label: Parser[LabelToken] = "\\d*[A-Za-z][A-Za-z0-9]*".r ^^ { label => LabelToken(label) }

  def ldb: Parser[LdbToken.type] = "LDB" ^^ { _ => LdbToken }
  def ldbu: Parser[LdbuToken.type] = "LDBU" ^^ { _ => LdbuToken }
  def ldw: Parser[LdwToken.type] = "LDW" ^^ { _ => LdwToken }
  def ldwu: Parser[LdwuToken.type] = "LDWU" ^^ { _ => LdwuToken }
  def ldt: Parser[LdtToken.type] = "LDT" ^^ { _ => LdtToken }
  def ldtu: Parser[LdtuToken.type] = "LDTU" ^^ { _ => LdtuToken }
  def ldo: Parser[LdoToken.type] = "LDO" ^^ { _ => LdoToken }
  def ldou: Parser[LdouToken.type] = "LDOU" ^^ { _ => LdouToken }

  def operation: Parser[OperationToken] = ldbu | ldb | ldwu | ldw | ldtu | ldt | ldou | ldo

  def line: Parser[MmixProgramLine] = {
    opt(label) ~ w ~ operation /*~ opt(w) ~ opt(address) ~ opt(w)*/ ~ opt(newLine) ^^ {
//      case l ~ _ ~ o ~ _ ~ Some(Vacuous) ~ _ ~ _ => MmixProgramLine(l, o, None)
      case l ~ _ ~ o /*~ _ ~ a ~ _*/ ~ _ => MmixProgramLine(l, o)
    }
  }

  def tokens: Parser[List[MmixLine]] = {
    phrase(
      rep(
        line
//        comment | line | alf
      )
    )
  }

  def apply(code: String): Either[String, List[MmixLine]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => Left(s"Error: $msg")
      case Success(result, _) => Right(result)
    }
  }

}
