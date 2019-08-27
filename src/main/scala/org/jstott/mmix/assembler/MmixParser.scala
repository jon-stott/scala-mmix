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

  def lda: Parser[LdaToken.type] = "LDA" ^^ { _ => LdaToken }
  def ldb: Parser[LdbToken.type] = "LDB" ^^ { _ => LdbToken }
  def ldbu: Parser[LdbuToken.type] = "LDBU" ^^ { _ => LdbuToken }
  def ldht: Parser[LdhtToken.type] = "LDHT" ^^ { _ => LdhtToken }
  def ldo: Parser[LdoToken.type] = "LDO" ^^ { _ => LdoToken }
  def ldou: Parser[LdouToken.type] = "LDOU" ^^ { _ => LdouToken }
  def ldt: Parser[LdtToken.type] = "LDT" ^^ { _ => LdtToken }
  def ldtu: Parser[LdtuToken.type] = "LDTU" ^^ { _ => LdtuToken }
  def ldw: Parser[LdwToken.type] = "LDW" ^^ { _ => LdwToken }
  def ldwu: Parser[LdwuToken.type] = "LDWU" ^^ { _ => LdwuToken }

  def loadOperation: Parser[OperationToken] = lda | ldbu | ldb | ldwu | ldw | ldtu | ldt | ldou | ldo | ldht

  def stb: Parser[StbToken.type] = "STB" ^^ { _ => StbToken }
  def stbu: Parser[StbuToken.type] = "STBU" ^^ { _ => StbuToken }
  def stco: Parser[StcoToken.type] = "STCO" ^^ { _ => StcoToken }
  def stht: Parser[SthtToken.type] = "STHT" ^^ { _ => SthtToken }
  def sto: Parser[StoToken.type] = "STO" ^^ { _ => StoToken }
  def stou: Parser[StouToken.type] = "STOU" ^^ { _ => StouToken }
  def stt: Parser[SttToken.type] = "STT" ^^ { _ => SttToken }
  def sttu: Parser[SttuToken.type] = "STTU" ^^ { _ => SttuToken }
  def stw: Parser[StwToken.type] = "STW" ^^ { _ => StwToken }
  def stwu: Parser[StwuToken.type] = "STWU" ^^ { _ => StwuToken }

  def storeOperation: Parser[OperationToken] = stbu | stb | stwu | stw | sttu | stt | stou | sto | stht | stco

  def add: Parser[AddToken.type] = "ADD" ^^ { _ => AddToken }
  def addu: Parser[AdduToken.type] = "ADDU" ^^ { _ => AdduToken }
  def sub: Parser[SubToken.type] = "SUB" ^^ { _ => SubToken }
  def subu: Parser[SubuToken.type] = "SUBU" ^^ { _ => SubuToken }
  def mul: Parser[MulToken.type] = "MUL" ^^ { _ => MulToken }
  def mulu: Parser[MuluToken.type] = "MULU" ^^ { _ => MuluToken }
  def div: Parser[DivToken.type] = "DIV" ^^ { _ => DivToken }
  def divu: Parser[DivuToken.type] = "DIVU" ^^ { _ => DivuToken }
  def addu2: Parser[Addu2Token.type] = "2ADDU" ^^ { _ => Addu2Token }
  def addu4: Parser[Addu4Token.type] = "4ADDU" ^^ { _ => Addu4Token }
  def addu8: Parser[Addu8Token.type] = "8ADDU" ^^ { _ => Addu8Token }
  def addu16: Parser[Addu16Token.type] = "16ADDU" ^^ { _ => Addu16Token }
  def neg: Parser[NegToken.type] = "NEG" ^^ { _ => NegToken }
  def negu: Parser[NeguToken.type] = "NEGU" ^^ { _ => NeguToken }
  def sl: Parser[SlToken.type] = "SL" ^^ { _ => SlToken }
  def slu: Parser[SluToken.type] = "SLU" ^^ { _ => SluToken }
  def sr: Parser[SrToken.type] = "SR" ^^ { _ => SrToken }
  def sru: Parser[SruToken.type] = "SRU" ^^ { _ => SruToken }
  def cmp: Parser[CmpToken.type] = "CMP" ^^ { _ => CmpToken }
  def cmpu: Parser[CmpuToken.type] = "CMPU" ^^ { _ => CmpuToken }

  def arithmeticOperation: Parser[OperationToken] = addu | add | subu | sub | mulu | mul | divu | div | addu2 | addu4 |
    addu8 | addu16 | negu | neg | slu | sl | sru | sr | cmpu | cmp

  def operation: Parser[OperationToken] = loadOperation | storeOperation | arithmeticOperation

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
