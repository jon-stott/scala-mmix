package org.jstott.mmix.assembler

import org.jstott.mmix.{ MmixByte, MmixOcta }
import org.scalatest.{ FlatSpec, MustMatchers }

class AssemblerSpec extends FlatSpec with MustMatchers {

  behavior of "Assembler"

  it should "assemble a LOC opcode using a symbol" in {
    val program = """ LOC Data_Segment""".stripMargin
    val state = new Assembler(program).assemble
    state.currentLocation mustBe BigInt("2000000000000000", 16)
  }

  it should "assemble a LOC opcode using a constant" in {
    val program = """ LOC #100""".stripMargin
    val state = new Assembler(program).assemble
    state.currentLocation mustBe BigInt(0x100)
  }

  it should "assemble a basic hello world program" in {
    val program = """ LOC Data_Segment
                    | GREG @
                    |Text BYTE "Hello-world!",10,0
                    | LOC #100
                    |Main LDA $255,Text
                    |	TRAP 0,Fputs,StdOut
                    | TRAP 0,Halt,0""".stripMargin
    val state = new Assembler(program).assemble
    state.mmix.generalRegisters(254) mustBe MmixOcta(BigInt("2000000000000000", 16).longValue)
    state.mmix.generalRegisters(255) mustBe MmixOcta(BigInt(0x100).longValue)
    state.globalRegisterAllocator mustBe 254
    state.currentGlobalRegister mustBe 254
    state.mmix.memory.get(BigInt("2000000000000000", 16)) mustBe Some(MmixByte('H'))
    state.mmix.memory.get(BigInt("2000000000000001", 16)) mustBe Some(MmixByte('e'))
    state.mmix.memory.get(BigInt("2000000000000002", 16)) mustBe Some(MmixByte('l'))
    state.mmix.memory.get(BigInt("2000000000000003", 16)) mustBe Some(MmixByte('l'))
    state.mmix.memory.get(BigInt("2000000000000004", 16)) mustBe Some(MmixByte('o'))
    state.mmix.memory.get(BigInt("2000000000000005", 16)) mustBe Some(MmixByte('-'))
    state.mmix.memory.get(BigInt("2000000000000006", 16)) mustBe Some(MmixByte('w'))
    state.mmix.memory.get(BigInt("2000000000000007", 16)) mustBe Some(MmixByte('o'))
    state.mmix.memory.get(BigInt("2000000000000008", 16)) mustBe Some(MmixByte('r'))
    state.mmix.memory.get(BigInt("2000000000000009", 16)) mustBe Some(MmixByte('l'))
    state.mmix.memory.get(BigInt("200000000000000a", 16)) mustBe Some(MmixByte('d'))
    state.mmix.memory.get(BigInt("200000000000000b", 16)) mustBe Some(MmixByte('!'))
    state.mmix.memory.get(BigInt("200000000000000c", 16)) mustBe Some(MmixByte(10))
    state.mmix.memory.get(BigInt("200000000000000d", 16)) mustBe Some(MmixByte(0))
    state.labels.get(SymbolToken("Text")) mustBe Some(ConstantToken(BigInt("2000000000000000", 16)))
    state.labels.get(SymbolToken("Main")) mustBe Some(ConstantToken(BigInt(0x100)))
    state.mmix.memory.get(BigInt(0x100)) mustBe Some(MmixByte(0x23)) // LDA - immediate
    state.mmix.memory.get(BigInt(0x101)) mustBe Some(MmixByte(255)) // $255
    state.mmix.memory.get(BigInt(0x102)) mustBe Some(MmixByte(254)) // Text - high
    state.mmix.memory.get(BigInt(0x103)) mustBe Some(MmixByte(0)) // Text - low
    state.mmix.memory.get(BigInt(0x104)) mustBe Some(MmixByte(0)) // TRAP
    state.mmix.memory.get(BigInt(0x105)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory.get(BigInt(0x106)) mustBe Some(MmixByte(7)) // Fputs
    state.mmix.memory.get(BigInt(0x107)) mustBe Some(MmixByte(1)) // StdOut
    state.mmix.memory.get(BigInt(0x108)) mustBe Some(MmixByte(0)) // TRAP
    state.mmix.memory.get(BigInt(0x109)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory.get(BigInt(0x10a)) mustBe Some(MmixByte(0)) // Halt
    state.mmix.memory.get(BigInt(0x10b)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory must have size 26
    state.currentLocation mustBe 0x10c
  }

  it should "assemble the first 500 primes program" in {
    val program =
      """L      IS   500
        |t      IS   $255
        |n      GREG
        |q      GREG
        |r      GREG
        |jj     GREG
        |kk     GREG
        |pk     GREG
        |mm     IS   kk
        |       LOC  Data_Segment
        |PRIME1 WYDE 2
        |       LOC  PRIME1+2*L
        |ptop   GREG @
        |j0     GREG PRIME1+2-@
        |BUF    OCTA
        |       LOC  #100
        |Main   SET  n,3
        |       SET  jj,j0
        |2H     STWU n,ptop,jj
        |       INCL jj,2
        |3H     BZ   jj,2F
        |4H     INCL n,2
        |5H     SET  kk,j0
        |6H     LDWU pk,ptop,kk
        |       DIV  q,n,pk
        |       GET  r,rR
        |       BZ   r,4B
        |7H     CMP  t,q,pk
        |       BNP  t,2B
        |8H     INCL kk,2
        |       JMP  6B
        |       GREG @
        |Title  BYTE "First_Five_Hundred_Primes"
        |NewLn  BYTE #a,0
        |Blanks BYTE #20,#20,#20,0
        |2H     LDA  t,Title
        |       TRAP 0,Fputs,StdOut
        |       NEG  mm,2
        |3H     ADD  mm,mm,j0
        |       LDA  t,Blanks
        |       TRAP 0,Fputs,StdOut
        |2H     LDWU pk,ptop,mm
        |0H     GREG #2030303030000000
        |       STOU 0B,BUF
        |       LDA  t,BUF+4
        |1H     DIV  pk,pk,10
        |       GET  r,rR
        |       INCL r,'0'
        |       STBU r,t,0
        |       SUB  t,t,1
        |       PBNZ pk,1B
        |       LDA  t,BUF
        |       TRAP 0,Fputs,StdOut
        |       INCL mm,2*L/10
        |       PBN  mm,2B
        |       LDA  t,NewLn
        |       TRAP 0,Fputs,StdOut
        |       CMP  t,mm,2*(L/10-1)
        |       PBNZ t,3B
        |       TRAP 0,Halt,0""".stripMargin
    val state = new Assembler(program).assemble
    // 0x100 e3fe 0003 = Main SET $n,3 = SETL n,3
    state.labels.get(SymbolToken("Main")) mustBe Some(ConstantToken(BigInt(0x100)))
    state.mmix.memory.get(BigInt(0x100)) mustBe Some(MmixByte(0xe3)) // SETL
    state.mmix.memory.get(BigInt(0x101)) mustBe Some(MmixByte(0xfe)) // $n = $254 = 0xfe
    state.mmix.memory.get(BigInt(0x102)) mustBe Some(MmixByte(0x00)) // 3.h = 0 = 0x00
    state.mmix.memory.get(BigInt(0x103)) mustBe Some(MmixByte(0x03)) // 3.l = 3 = 0x03
    // 0x104 c1fb f700 =  OR $jj,$j0
    state.mmix.memory.get(BigInt(0x104)) mustBe Some(MmixByte(0xc1)) // OR
    state.mmix.memory.get(BigInt(0x105)) mustBe Some(MmixByte(0xfb)) // $jj = $251 = 0xfb
    state.mmix.memory.get(BigInt(0x106)) mustBe Some(MmixByte(0xf7)) // $j0 = $247 = 0xf7
    state.mmix.memory.get(BigInt(0x107)) mustBe Some(MmixByte(0x00)) // 0x00
    // 0x108 a6fe f8fb = 2H STWU n,ptop,jj
    state.localLabels(2) must contain(BigInt(0x108))
    state.mmix.memory.get(BigInt(0x108)) mustBe Some(MmixByte(0xa6)) // STWU
    state.mmix.memory.get(BigInt(0x109)) mustBe Some(MmixByte(0xfe)) // $n = $254 = 0xfe
    state.mmix.memory.get(BigInt(0x10a)) mustBe Some(MmixByte(0xf8)) // $ptop = $248 = 0xf8
    state.mmix.memory.get(BigInt(0x10b)) mustBe Some(MmixByte(0xfb)) // $jj = $251 = 0xfb
    // 0x10c e7fb 0002 =  INCL jj,2
    state.mmix.memory.get(BigInt(0x10c)) mustBe Some(MmixByte(0xe7)) // INCL
    state.mmix.memory.get(BigInt(0x10d)) mustBe Some(MmixByte(0xfb)) // $jj = $251 = 0xfb
    state.mmix.memory.get(BigInt(0x10e)) mustBe Some(MmixByte(0x00)) // 2.h = 0 = 0x00
    state.mmix.memory.get(BigInt(0x10f)) mustBe Some(MmixByte(0x02)) // 2.l = 2 = 0x02
    // 0x110 42fb 0000 = 3H BZ jj,2F
    state.localLabels(3) must contain(BigInt(0x110))
    state.mmix.memory.get(BigInt(0x110)) mustBe Some(MmixByte(0x42)) // BZ
    state.mmix.memory.get(BigInt(0x111)) mustBe Some(MmixByte(0xfb)) // $jj = $251 = 0xfb
    state.mmix.memory.get(BigInt(0x112)) mustBe Some(MmixByte(0x00)) // 19.h = 0 = 0x00
    state.mmix.memory.get(BigInt(0x113)) mustBe Some(MmixByte(0x13)) // 19.l = 19 = 0x13
    // 0x114 e7fe 0002 = 4H INCL n,2
    state.localLabels(4) must contain(BigInt(0x114))
    state.mmix.memory.get(BigInt(0x114)) mustBe Some(MmixByte(0xe7)) // INCL
    state.mmix.memory.get(BigInt(0x115)) mustBe Some(MmixByte(0xfe)) // $n = $254 = 0xfe
    state.mmix.memory.get(BigInt(0x116)) mustBe Some(MmixByte(0x00)) // 2.h = 0 = 0x00
    state.mmix.memory.get(BigInt(0x117)) mustBe Some(MmixByte(0x02)) // 2.l = 2 = 0x02
    // 0x118 c1fa f700 = 5H SET kk,j0
    state.localLabels(5) must contain(BigInt(0x118))
    state.mmix.memory.get(BigInt(0x118)) mustBe Some(MmixByte(0xc1)) // OR - immediate
    state.mmix.memory.get(BigInt(0x119)) mustBe Some(MmixByte(0xfa)) // $kk = $250 = 0xfa
    state.mmix.memory.get(BigInt(0x11a)) mustBe Some(MmixByte(0xf7)) // $j0 = $247 = 0xf7
    state.mmix.memory.get(BigInt(0x11b)) mustBe Some(MmixByte(0x00)) // 0 = 0x00
    // 0x11c 86f9 f8fa = 6H LDWU pk,ptop,kk
    state.localLabels(6) must contain(BigInt(0x11c))
    state.mmix.memory.get(BigInt(0x11c)) mustBe Some(MmixByte(0x86)) // LDWU
    state.mmix.memory.get(BigInt(0x11d)) mustBe Some(MmixByte(0xf9)) // $pk = $249 = 0xf9
    state.mmix.memory.get(BigInt(0x11e)) mustBe Some(MmixByte(0xf8)) // $ptop = $248 = 0xf8
    state.mmix.memory.get(BigInt(0x11f)) mustBe Some(MmixByte(0xfa)) // $kk = $250 = 0xfa
    // 0x120 1cfd fef9 =  DIV  q,n,pk
    state.mmix.memory.get(BigInt(0x120)) mustBe Some(MmixByte(0x1c)) // DIV
    state.mmix.memory.get(BigInt(0x121)) mustBe Some(MmixByte(0xfd)) // $q = $253 = 0xfd
    state.mmix.memory.get(BigInt(0x122)) mustBe Some(MmixByte(0xfe)) // $n = $254 = 0xfe
    state.mmix.memory.get(BigInt(0x123)) mustBe Some(MmixByte(0xf9)) // $pk = $249 = 0xf9
    // 0x124 fefc 0006 =  GET  r,rR
    state.mmix.memory.get(BigInt(0x124)) mustBe Some(MmixByte(0xfe)) // GET
    state.mmix.memory.get(BigInt(0x125)) mustBe Some(MmixByte(0xfc)) // $r = $252 = 0xfc
    state.mmix.memory.get(BigInt(0x126)) mustBe Some(MmixByte(0x00)) // rR.h = 0 = 0x00
    state.mmix.memory.get(BigInt(0x127)) mustBe Some(MmixByte(0x06)) // rR.l = 6 = 0x06
    // 0x128 43fc fffb =  BZ   r,4B
    state.mmix.memory.get(BigInt(0x128)) mustBe Some(MmixByte(0x43)) // BZ - backwards
    state.mmix.memory.get(BigInt(0x129)) mustBe Some(MmixByte(0xfc)) // $r = $252 = 0xfc
    state.mmix.memory.get(BigInt(0x12a)) mustBe Some(MmixByte(0xff)) // -4.h = -0 = 0xff
    state.mmix.memory.get(BigInt(0x12b)) mustBe Some(MmixByte(0xfb)) // -4.l = -4 = 0xfb
    // 0x12c 30ff fdf9 = 7H     CMP  t,q,pk
    state.localLabels(7) must contain(BigInt(0x12c))
    state.mmix.memory.get(BigInt(0x12c)) mustBe Some(MmixByte(0x30)) // CMP
    state.mmix.memory.get(BigInt(0x12d)) mustBe Some(MmixByte(0xff)) // $t = $255 = 0xff
    state.mmix.memory.get(BigInt(0x12e)) mustBe Some(MmixByte(0xfd)) // $q = $253 = 0xfd
    state.mmix.memory.get(BigInt(0x12f)) mustBe Some(MmixByte(0xf9)) // $pk = $249 = 0xf9
    // 0x130 4dff fff6 =  BNP  t,2B
    state.mmix.memory.get(BigInt(0x130)) mustBe Some(MmixByte(0x4d)) // BNP - backwards
    state.mmix.memory.get(BigInt(0x131)) mustBe Some(MmixByte(0xff)) // $t = $255 = 0xff
    state.mmix.memory.get(BigInt(0x132)) mustBe Some(MmixByte(0xff)) // -10.h = -0 = 0xff
    state.mmix.memory.get(BigInt(0x133)) mustBe Some(MmixByte(0xf6)) // -10.l = -10 = 0xf6
    // 0x134 e7fa 0002 = 8H     INCL kk,2
    state.localLabels(8) must contain(BigInt(0x134))
    state.mmix.memory.get(BigInt(0x134)) mustBe Some(MmixByte(0xe7)) // INCL
    state.mmix.memory.get(BigInt(0x135)) mustBe Some(MmixByte(0xfa)) // $kk = $250 = 0xfa
    state.mmix.memory.get(BigInt(0x136)) mustBe Some(MmixByte(0x00)) // 2.h = 0 = 0x00
    state.mmix.memory.get(BigInt(0x137)) mustBe Some(MmixByte(0x02)) // 2.l = 2 = 0x02
    // 0x138 f1ff fff9 =  JMP  6B
    state.mmix.memory.get(BigInt(0x138)) mustBe Some(MmixByte(0xf1)) // JMP - backwards
    state.mmix.memory.get(BigInt(0x139)) mustBe Some(MmixByte(0xff)) // 0xff
    state.mmix.memory.get(BigInt(0x13a)) mustBe Some(MmixByte(0xff)) // -7.h = 0 = 0xff
    state.mmix.memory.get(BigInt(0x13b)) mustBe Some(MmixByte(0xf9)) // -7.l = -7 = 0xf9

    state.mmix.memory.get(BigInt(0x13c)) mustBe Some(MmixByte(0x46)) // F
    state.mmix.memory.get(BigInt(0x13d)) mustBe Some(MmixByte(0x69)) // i
    state.mmix.memory.get(BigInt(0x13e)) mustBe Some(MmixByte(0x72)) // r
    state.mmix.memory.get(BigInt(0x13f)) mustBe Some(MmixByte(0x73)) // s
    state.mmix.memory.get(BigInt(0x140)) mustBe Some(MmixByte(0x74)) // t
    state.mmix.memory.get(BigInt(0x141)) mustBe Some(MmixByte(0x5f)) // _
    state.mmix.memory.get(BigInt(0x142)) mustBe Some(MmixByte(0x46)) // F
    state.mmix.memory.get(BigInt(0x143)) mustBe Some(MmixByte(0x69)) // i
    state.mmix.memory.get(BigInt(0x144)) mustBe Some(MmixByte(0x76)) // v
    state.mmix.memory.get(BigInt(0x145)) mustBe Some(MmixByte(0x65)) // e
    state.mmix.memory.get(BigInt(0x146)) mustBe Some(MmixByte(0x5f)) // _
    state.mmix.memory.get(BigInt(0x147)) mustBe Some(MmixByte(0x48)) // H
    state.mmix.memory.get(BigInt(0x148)) mustBe Some(MmixByte(0x75)) // u
    state.mmix.memory.get(BigInt(0x149)) mustBe Some(MmixByte(0x6e)) // n
    state.mmix.memory.get(BigInt(0x14a)) mustBe Some(MmixByte(0x64)) // d
    state.mmix.memory.get(BigInt(0x14b)) mustBe Some(MmixByte(0x72)) // r
    state.mmix.memory.get(BigInt(0x14c)) mustBe Some(MmixByte(0x65)) // e
    state.mmix.memory.get(BigInt(0x14d)) mustBe Some(MmixByte(0x64)) // d
    state.mmix.memory.get(BigInt(0x14e)) mustBe Some(MmixByte(0x5f)) // _
    state.mmix.memory.get(BigInt(0x14f)) mustBe Some(MmixByte(0x50)) // P
    state.mmix.memory.get(BigInt(0x150)) mustBe Some(MmixByte(0x72)) // r
    state.mmix.memory.get(BigInt(0x151)) mustBe Some(MmixByte(0x69)) // i
    state.mmix.memory.get(BigInt(0x152)) mustBe Some(MmixByte(0x6d)) // m
    state.mmix.memory.get(BigInt(0x153)) mustBe Some(MmixByte(0x65)) // e
    state.mmix.memory.get(BigInt(0x154)) mustBe Some(MmixByte(0x73)) // s
    state.mmix.memory.get(BigInt(0x155)) mustBe Some(MmixByte(0x0a)) // 0x0a
    state.mmix.memory.get(BigInt(0x156)) mustBe Some(MmixByte(0x00)) // 0x00
    state.mmix.memory.get(BigInt(0x157)) mustBe Some(MmixByte(0x20)) // 0x20
    state.mmix.memory.get(BigInt(0x158)) mustBe Some(MmixByte(0x20)) // 0x20
    state.mmix.memory.get(BigInt(0x159)) mustBe Some(MmixByte(0x20)) // 0x20
// FIXME   state.mmix.memory.get(BigInt(0x15a)) mustBe Some(MmixByte(0x00)) // 0x00

    // 23ff f600 - 2H     LDA  t,Title
    state.localLabels(2) must contain(BigInt(0x15c))
    state.mmix.memory.get(BigInt(0x15c)) mustBe Some(MmixByte(0x23)) // LDA - immediate
    state.mmix.memory.get(BigInt(0x15d)) mustBe Some(MmixByte(0xff)) // t = $255 = 0xff
// FIXME   state.mmix.memory.get(BigInt(0x15e)) mustBe Some(MmixByte(0xf6)) // Title.h = 246 = 0xf6
// FIXME   state.mmix.memory.get(BigInt(0x15f)) mustBe Some(MmixByte(0x00)) // Title.l = 0 = 0x00
    // 0000 0701 - TRAP 0,Fputs,StdOut
    state.mmix.memory.get(BigInt(0x160)) mustBe Some(MmixByte(0)) // TRAP
    state.mmix.memory.get(BigInt(0x161)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory.get(BigInt(0x162)) mustBe Some(MmixByte(7)) // Fputs
    state.mmix.memory.get(BigInt(0x163)) mustBe Some(MmixByte(1)) // StdOut
    // 35fa 0002 - NEG  mm,2
    state.mmix.memory.get(BigInt(0x164)) mustBe Some(MmixByte(0x35)) // NEG - immediate
    state.mmix.memory.get(BigInt(0x165)) mustBe Some(MmixByte(0xfa)) // $mm = $kk = $250 = 0xfa
    state.mmix.memory.get(BigInt(0x166)) mustBe Some(MmixByte(0x00)) // 2.h = 0x00
    state.mmix.memory.get(BigInt(0x167)) mustBe Some(MmixByte(0x02)) // 2.l = 0x02
    // 20fa faf7 - 3H     ADD  mm,mm,j0
    state.localLabels(3) must contain(BigInt(0x168))
    state.mmix.memory.get(BigInt(0x168)) mustBe Some(MmixByte(0x20)) // ADD
    state.mmix.memory.get(BigInt(0x169)) mustBe Some(MmixByte(0xfa)) // $mm = $kk = $250 = 0xfa
    state.mmix.memory.get(BigInt(0x16a)) mustBe Some(MmixByte(0xfa)) // $mm = $kk = $250 = 0xfa
    state.mmix.memory.get(BigInt(0x16b)) mustBe Some(MmixByte(0xf7)) // $j0 = $247 = 0xf7
    // 23ff f61b - LDA  t,Blanks
    state.mmix.memory.get(BigInt(0x16c)) mustBe Some(MmixByte(0x23)) // LDA - immediate
    state.mmix.memory.get(BigInt(0x16d)) mustBe Some(MmixByte(0xff)) // t = $255 = 0xff
// FIXME   state.mmix.memory.get(BigInt(0x16e)) mustBe Some(MmixByte(0xf6)) // Blanks.h = 246 = 0xf6
// FIXME   state.mmix.memory.get(BigInt(0x16f)) mustBe Some(MmixByte(0x1b)) // Blanks.l = 27 = 0x1b
    // 0000 0701 - TRAP 0,Fputs,StdOut
    state.mmix.memory.get(BigInt(0x170)) mustBe Some(MmixByte(0)) // TRAP
    state.mmix.memory.get(BigInt(0x171)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory.get(BigInt(0x172)) mustBe Some(MmixByte(7)) // Fputs
    state.mmix.memory.get(BigInt(0x173)) mustBe Some(MmixByte(1)) // StdOut
    // 86f9 f8fa - 2H     LDWU pk,ptop,mm
    state.localLabels(2) must contain(BigInt(0x174))
    state.mmix.memory.get(BigInt(0x174)) mustBe Some(MmixByte(0x86)) // LDWU
    state.mmix.memory.get(BigInt(0x175)) mustBe Some(MmixByte(0xf9)) // $pk = $249 = 0xf9
    state.mmix.memory.get(BigInt(0x176)) mustBe Some(MmixByte(0xf8)) // $ptop = $248 = 0xf8
    state.mmix.memory.get(BigInt(0x177)) mustBe Some(MmixByte(0xfa)) // $mm = $kk = $250 = 0xfa

    // 0x178 - aff5 f800 -        STOU 0B,BUF
    state.mmix.memory.get(BigInt(0x178)) mustBe Some(MmixByte(0xaf)) // STOU - immediate
    state.mmix.memory.get(BigInt(0x179)) mustBe Some(MmixByte(0xf5)) //
    state.mmix.memory.get(BigInt(0x17a)) mustBe Some(MmixByte(0xf8)) // BUF.h
    state.mmix.memory.get(BigInt(0x17b)) mustBe Some(MmixByte(0x00)) // BUF.l
    // 0x17c - 23ff f804 -        LDA  t,BUF+4
    state.mmix.memory.get(BigInt(0x17c)) mustBe Some(MmixByte(0x23)) // LDA - immediate
    state.mmix.memory.get(BigInt(0x17d)) mustBe Some(MmixByte(0xff)) // $t = $255 = 0xff
    state.mmix.memory.get(BigInt(0x17e)) mustBe Some(MmixByte(0xf8)) // BUF+4.h
    state.mmix.memory.get(BigInt(0x17f)) mustBe Some(MmixByte(0x04)) // BUF+4.l
    // 0x180 - 1df9 f90a - 1H     DIV  pk,pk,10
    state.localLabels(1) must contain(BigInt(0x180))
    // 0x184 - fefc 0006 -        GET  r,rR
    // 0x188 - e7fc 0030 -        INCL r,'0'
    // 0x18c - a3fc ff00 -        STBU r,t,0
    // 0x190 - 25ff ff01 -        SUB  t,t,1
    // 0x194 - 5bf9 fffb -        PBNZ pk,1B
    // 0x198 - 23ff f800 -        LDA  t,BUF
    // 0x19c - 0000 0701 -        TRAP 0,Fputs,StdOut
    state.mmix.memory.get(BigInt(0x19c)) mustBe Some(MmixByte(0)) // TRAP
    state.mmix.memory.get(BigInt(0x19d)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory.get(BigInt(0x19e)) mustBe Some(MmixByte(7)) // Fputs
    state.mmix.memory.get(BigInt(0x19f)) mustBe Some(MmixByte(1)) // StdOut
    // 0x1a0 - e7fa 0064 -        INCL mm,2*L/10
    // 0x1a4 - 51fa fff4 -        PBN  mm,2B
    // 0x1a8 - 23ff f619 -        LDA  t,NewLn
    // 0x1ac - 0000 0701 -        TRAP 0,Fputs,StdOut
    state.mmix.memory.get(BigInt(0x1ac)) mustBe Some(MmixByte(0)) // TRAP
    state.mmix.memory.get(BigInt(0x1ad)) mustBe Some(MmixByte(0)) // 0
    state.mmix.memory.get(BigInt(0x1ae)) mustBe Some(MmixByte(7)) // Fputs
    state.mmix.memory.get(BigInt(0x1af)) mustBe Some(MmixByte(1)) // StdOut
    // 0x1b0 - 31ff fa62 -        CMP  t,mm,2*(L/10-1)
    // 0x1b4 - 5bff ffed -        PBNZ t,3B
    // 0x1b8 - 0000 0000 -        TRAP 0,Halt,0







    state.mmix.memory must have size 184
  }

}
