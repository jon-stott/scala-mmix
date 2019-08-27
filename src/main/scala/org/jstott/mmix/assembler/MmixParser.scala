package org.jstott.mmix.assembler

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object MmixParser extends RegexParsers {

  override def skipWhitespace: Boolean = false

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def w: Parser[WhitespaceToken.type] = whiteSpace ^^ { s => WhitespaceToken }
  def newLine: Parser[NewLineToken.type] = "\n".r ^^ { s => NewLineToken }

  def label: Parser[LabelToken] = "\\d*[A-Za-z][A-Za-z0-9]*".r ^^ { label => LabelToken(label) }

  def is: Parser[IsToken.type] = "IS" ^^ { _ => IsToken }
  def loc: Parser[LocToken.type] = "LOC" ^^ { _ => LocToken }

  def assemblerToken: Parser[OperationToken] = is | loc

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

  def csn: Parser[CsnToken.type] = "CSN" ^^ { _ => CsnToken }
  def csz: Parser[CszToken.type] = "CSZ" ^^ { _ => CszToken }
  def csp: Parser[CspToken.type] = "CSP" ^^ { _ => CspToken }
  def csod: Parser[CsodToken.type] = "CSOD" ^^ { _ => CsodToken }
  def csnn: Parser[CsnnToken.type] = "CSNN" ^^ { _ => CsnnToken }
  def csnz: Parser[CsnzToken.type] = "CSNZ" ^^ { _ => CsnzToken }
  def csnp: Parser[CsnpToken.type] = "CSNP" ^^ { _ => CsnpToken }
  def csev: Parser[CsevToken.type] = "CSEV" ^^ { _ => CsevToken }
  def zsn: Parser[ZsnToken.type] = "ZSN" ^^ { _ => ZsnToken }
  def zsz: Parser[ZszToken.type] = "ZSZ" ^^ { _ => ZszToken }
  def zsp: Parser[ZspToken.type] = "ZSP" ^^ { _ => ZspToken }
  def zsod: Parser[ZsodToken.type] = "ZSOD" ^^ { _ => ZsodToken }
  def zsnn: Parser[ZsnnToken.type] = "ZSNN" ^^ { _ => ZsnnToken }
  def zsnz: Parser[ZsnzToken.type] = "ZSNZ" ^^ { _ => ZsnzToken }
  def zsnp: Parser[ZsnpToken.type] = "ZSNP" ^^ { _ => ZsnpToken }
  def zsev: Parser[ZsevToken.type] = "ZSEV" ^^ { _ => ZsevToken }

  def conditionalOperation: Parser[OperationToken] =  csnn | csnz | csnp | csn | csz | csp | csod | csev | zsnn |
    zsnz | zsnp | zsn | zsz | zsp | zsod | zsev

  def and: Parser[AndToken.type] = "AND" ^^ { _ => AndToken }
  def or: Parser[OrToken.type] = "OR" ^^ { _ => OrToken }
  def xor: Parser[XorToken.type] = "XOR" ^^ { _ => XorToken }
  def andn: Parser[AndnToken.type] = "ANDN" ^^ { _ => AndnToken }
  def orn: Parser[OrnToken.type] = "ORN" ^^ { _ => OrnToken }
  def nand: Parser[NandToken.type] = "NAND" ^^ { _ => NandToken }
  def nor: Parser[NorToken.type] = "NOR" ^^ { _ => NorToken }
  def nxor: Parser[NxorToken.type] = "NXOR" ^^ { _ => NxorToken }
  def mux: Parser[MuxToken.type] = "MUX" ^^ { _ => MuxToken }
  def sadd: Parser[SaddToken.type] = "SADD" ^^ { _ => SaddToken }

  def bitwiseOperation: Parser[OperationToken] = andn | and | orn | or | xor | nor | nand | nxor | sadd | mux

  def bdif: Parser[BdifToken.type] = "BDIF" ^^ { _ => BdifToken }
  def wdif: Parser[WdifToken.type] = "WDIF" ^^ { _ => WdifToken }
  def tdif: Parser[TdifToken.type] = "TDIF" ^^ { _ => TdifToken }
  def odif: Parser[OdifToken.type] = "ODIF" ^^ { _ => OdifToken }
  def mor: Parser[MorToken.type] = "MOR" ^^ { _ => MorToken }
  def mxor: Parser[MxorToken.type] = "MXOR" ^^ { _ => MxorToken }

  def bytewiseOperation: Parser[OperationToken] = bdif | wdif | tdif | odif | mor | mxor

  def fadd: Parser[FaddToken.type] = "FADD" ^^ { _ => FaddToken }
  def fsub: Parser[FsubToken.type] = "FSUB" ^^ { _ => FsubToken }
  def fmul: Parser[FmulToken.type] = "FMUL" ^^ { _ => FmulToken }
  def fdiv: Parser[FdivToken.type] = "FDIV" ^^ { _ => FdivToken }
  def frem: Parser[FremToken.type] = "FREM" ^^ { _ => FremToken }
  def fsqrt: Parser[FsqrtToken.type] = "FSQRT" ^^ { _ => FsqrtToken }
  def fint: Parser[FintToken.type] = "FINT" ^^ { _ => FintToken }
  def fcmp: Parser[FcmpToken.type] = "FCMP" ^^ { _ => FcmpToken }
  def feql: Parser[FeqlToken.type] = "FEQL" ^^ { _ => FeqlToken }
  def fun: Parser[FunToken.type] = "FUN" ^^ { _ => FunToken }
  def fcmpe: Parser[FcmpeToken.type] = "FCMPE" ^^ { _ => FcmpeToken }
  def feqle: Parser[FeqleToken.type] = "FEQLE" ^^ { _ => FeqleToken }
  def fune: Parser[FuneToken.type] = "FUNE" ^^ { _ => FuneToken }
  def fix: Parser[FixToken.type] = "FIX" ^^ { _ => FixToken }
  def fixu: Parser[FixuToken.type] = "FIXU" ^^ { _ => FixuToken }
  def flot: Parser[FlotToken.type] = "FLOT" ^^ { _ => FlotToken }
  def flotu: Parser[FlotuToken.type] = "FLOTU" ^^ { _ => FlotuToken }
  def sflot: Parser[SflotToken.type] = "SFLOT" ^^ { _ => SflotToken }
  def sflotu: Parser[SflotuToken.type] = "SFLOTU" ^^ { _ => SflotuToken }
  def ldsf: Parser[LdsfToken.type] = "LDSF" ^^ { _ => LdsfToken }
  def stsf: Parser[StsfToken.type] = "STSF" ^^ { _ => StsfToken }

  def floatingPointOperation: Parser[OperationToken] = fadd | fmul | fsub | fdiv | frem | fsqrt | fint | fcmpe | fcmp |
    feqle | feql | fune | fun | fixu | fix | flotu | flot | sflotu | sflot | ldsf | stsf

  def seth: Parser[SethToken.type] = "SETH" ^^ { _ => SethToken }
  def setmh: Parser[SetmhToken.type] = "SETMH" ^^ { _ => SetmhToken }
  def setml: Parser[SetmlToken.type] = "SETML" ^^ { _ => SetmlToken }
  def setl: Parser[SetlToken.type] = "SETL" ^^ { _ => SetlToken }
  def inch: Parser[InchToken.type] = "INCH" ^^ { _ => InchToken }
  def incmh: Parser[IncmhToken.type] = "INCMH" ^^ { _ => IncmhToken }
  def incml: Parser[IncmlToken.type] = "INCML" ^^ { _ => IncmlToken }
  def incl: Parser[InclToken.type] = "INCL" ^^ { _ => InclToken }
  def orh: Parser[OrhToken.type] = "ORH" ^^ { _ => OrhToken }
  def ormh: Parser[OrmhToken.type] = "ORMH" ^^ { _ => OrmhToken }
  def orml: Parser[OrmlToken.type] = "ORML" ^^ { _ => OrmlToken }
  def orl: Parser[OrlToken.type] = "ORL" ^^ { _ => OrlToken }
  def andnh: Parser[AndnhToken.type] = "ANDNH" ^^ { _ => AndnhToken }
  def andnmh: Parser[AndnmhToken.type] = "ANDNMH" ^^ { _ => AndnmhToken }
  def andnml: Parser[AndnmlToken.type] = "ANDNML" ^^ { _ => AndnmlToken }
  def andnl: Parser[AndnlToken.type] = "ANDNL" ^^ { _ => AndnlToken }

  def immediateConstantOperation: Parser[OperationToken] = seth | setmh | setml | setl | inch | incmh | incml | incl |
    orh | ormh | orml | orl | andnh | andnmh | andnml | andnl

  def jmp: Parser[JmpToken.type] = "JMP" ^^ { _ => JmpToken }
  def go: Parser[GoToken.type] = "GO" ^^ { _ => GoToken }
  def bn: Parser[BnToken.type] = "BN" ^^ { _ => BnToken }
  def bz: Parser[BzToken.type] = "BZ" ^^ { _ => BzToken }
  def bp: Parser[BpToken.type] = "BP" ^^ { _ => BpToken }
  def bod: Parser[BodToken.type] = "BOD" ^^ { _ => BodToken }
  def bnn: Parser[BnnToken.type] = "BNN" ^^ { _ => BnnToken }
  def bnz: Parser[BnzToken.type] = "BNZ" ^^ { _ => BnzToken }
  def bnp: Parser[BnpToken.type] = "BNP" ^^ { _ => BnpToken }
  def bev: Parser[BevToken.type] = "BEV" ^^ { _ => BevToken }
  def pbn: Parser[PbnToken.type] = "PBN" ^^ { _ => PbnToken }
  def pbz: Parser[PbzToken.type] = "PBZ" ^^ { _ => PbzToken }
  def pbp: Parser[PbpToken.type] = "PBP" ^^ { _ => PbpToken }
  def pbod: Parser[PbodToken.type] = "PBOD" ^^ { _ => PbodToken }
  def pbnn: Parser[PbnnToken.type] = "PBNN" ^^ { _ => PbnnToken }
  def pbnz: Parser[PbnzToken.type] = "PBNZ" ^^ { _ => PbnzToken }
  def pbnp: Parser[PbnpToken.type] = "PBNP" ^^ { _ => PbnpToken }
  def pbev: Parser[PbevToken.type] = "PBEV" ^^ { _ => PbevToken }

  def jumpOrBranchOperation: Parser[OperationToken] = jmp | go | bz | bp | bod | bnn | bnz | bnp | bn | bev | pbz |
    pbp | pbod | pbnn | pbnz | pbnp | pbn | pbev

  def pushj: Parser[PushjToken.type] = "PUSHJ" ^^ { _ => PushjToken }
  def pushgo: Parser[PushgoToken.type] = "PUSHGO" ^^ { _ => PushgoToken }
  def pop: Parser[PopToken.type] = "POP" ^^ { _ => PopToken }
  def save: Parser[SaveToken.type] = "SAVE" ^^ { _ => SaveToken }
  def unsave: Parser[UnsaveToken.type] = "UNSAVE" ^^ { _ => UnsaveToken }

  def subroutineCallOperation: Parser[OperationToken] = pushj | pushgo | pop | save | unsave

  def ldunc: Parser[LduncToken.type] = "LDUNC" ^^ { _ => LduncToken }
  def stunc: Parser[StuncToken.type] = "STUNC" ^^ { _ => StuncToken }
  def preld: Parser[PreldToken.type] = "PRELD" ^^ { _ => PreldToken }
  def prest: Parser[PrestToken.type] = "PREST" ^^ { _ => PrestToken }
  def prego: Parser[PregoToken.type] = "PREGO" ^^ { _ => PregoToken }
  def syncid: Parser[SyncidToken.type] = "SYNCID" ^^ { _ => SyncidToken }
  def syncd: Parser[SyncdToken.type] = "SYNCD" ^^ { _ => SyncdToken }
  def sync: Parser[SyncToken.type] = "SYNC" ^^ { _ => SyncToken }
  def cswap: Parser[CswapToken.type] = "CSWAP" ^^ { _ => CswapToken }
  def ldvts: Parser[LdvtsToken.type] = "LDVTS" ^^ { _ => LdvtsToken }

  def systemOperation: Parser[OperationToken] = ldunc | stunc | preld | prest | prego | syncid | syncd | sync | cswap |
    ldvts

  def trip: Parser[TripToken.type] = "TRIP" ^^ { _ => TripToken }
  def trap: Parser[TrapToken.type] = "TRAP" ^^ { _ => TrapToken }
  def resume: Parser[ResumeToken.type] = "RESUME" ^^ { _ => ResumeToken }

  def interruptOperation: Parser[OperationToken] = trip | trap | resume

  def get: Parser[GetToken.type] = "GET" ^^ { _ => GetToken }
  def put: Parser[PutToken.type] = "PUT" ^^ { _ => PutToken }
  def geta: Parser[GetaToken.type] = "GETA" ^^ { _ => GetaToken }
  def swym: Parser[SwymToken.type] = "SWYM" ^^ { _ => SwymToken }

  def otherOperation: Parser[OperationToken] = geta | get | put | swym

  def operation: Parser[OperationToken] = assemblerToken | loadOperation | storeOperation | arithmeticOperation |
    conditionalOperation | immediateConstantOperation | bitwiseOperation | bytewiseOperation | floatingPointOperation |
    jumpOrBranchOperation | subroutineCallOperation | systemOperation | interruptOperation | otherOperation

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
