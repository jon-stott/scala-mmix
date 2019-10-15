package org.jstott.mmix.assembler

import org.jstott.mmix.Mmix

case class AssemblerState(
                           mmix: Mmix,
                           currentLocation: BigInt = 0,
                           unevaluatedOperations: Map[Int, MmixProgramLine] = Map.empty,
                           literalOperations: Seq[MmixProgramLine] = Seq.empty,
                           startExecutionFrom: BigInt = 0,
                           globalRegisterAllocator: Int = 255,
                           currentGlobalRegister: Int = 0,
                           labels: Map[SymbolToken, ConstantToken] = AssemblerState.defaultLabels,
                           localLabels: Map[Int, Set[BigInt]] = Map.empty,
                           isSpecialMode: Boolean = false,
                           linesWithForwardReferences: Set[(BigInt, MmixProgramLine)] = Set.empty
                         ) {

  def greg: Int = globalRegisterAllocator
  def cur_greg: Int = currentGlobalRegister

  def withAlignedCurrentLocation(op: OperationToken): AssemblerState = {

//    j=(op_bits&align_bits)>>16;
//    acc.h=-1, acc.l=-(1<<j);
//    cur_loc=oand(incr(cur_loc,(1<<j)-1),acc);

    val x = op match {
      case operator: OperatorToken =>
        val j = 1 << ((operator.opBits.t & OperatorToken.alignBits) >> 16)
        (currentLocation + j - 1) & -j
      case _ => currentLocation
    }
    this.copy(currentLocation = x)
  }

  def findLocalSymbolAddress(n: Int, bfh: String): Either[String, BigInt] = {
    val result = localLabels.get(n) match {
      case Some(labelLocations) =>
        val diffs = labelLocations.map(_ - currentLocation).toSeq
        bfh match {
          case "B" =>
            val filtered = diffs.filter(_ <= 0)
            if (filtered.nonEmpty) {
              Right(currentLocation + filtered.sorted.max)
            } else {
              Left(s"Can't dereference symbol $n$bfh")
            }
          case "F" =>
            val filtered = diffs.filter(_ >= 0)
            if (filtered.nonEmpty) {
              Right(currentLocation + filtered.sorted.min)
            } else {
              Left(s"Can't dereference symbol $n$bfh")
            }
          case "H" =>
            Left(s"Unexpected local symbol $n$bfh")
        }
      case _ => Left(s"Can't dereference symbol $n$bfh")
    }
    result
  }

}

object AssemblerState {

  val defaultLabels: Map[SymbolToken, ConstantToken] = Map(
    SymbolToken("ROUND_CURRENT") -> ConstantToken(BigInt(0x0)),
    SymbolToken("ROUND_OFF") -> ConstantToken(BigInt(0x1)),
    SymbolToken("ROUND_UP") -> ConstantToken(BigInt(0x2)),
    SymbolToken("ROUND_DOWN") -> ConstantToken(BigInt(0x3)),
    SymbolToken("ROUND_NEAR") -> ConstantToken(BigInt(0x4)),
    SymbolToken("Inf") -> ConstantToken(BigInt("7ff0000000000000", 16)),
    SymbolToken("Data_Segment") -> ConstantToken(BigInt("2000000000000000", 16)),
    SymbolToken("Pool_Segment") -> ConstantToken(BigInt("4000000000000000", 16)),
    SymbolToken("Stack_Segment") -> ConstantToken(BigInt("6000000000000000", 16)),
    SymbolToken("D_BIT") -> ConstantToken(BigInt(0x80)),
    SymbolToken("V_BIT") -> ConstantToken(BigInt(0x40)),
    SymbolToken("W_BIT") -> ConstantToken(BigInt(0x20)),
    SymbolToken("I_BIT") -> ConstantToken(BigInt(0x10)),
    SymbolToken("O_BIT") -> ConstantToken(BigInt(0x8)),
    SymbolToken("U_BIT") -> ConstantToken(BigInt(0x4)),
    SymbolToken("Z_BIT") -> ConstantToken(BigInt(0x2)),
    SymbolToken("X_BIT") -> ConstantToken(BigInt(0x1)),
    SymbolToken("D_Handler") -> ConstantToken(BigInt(0x10)),
    SymbolToken("V_Handler") -> ConstantToken(BigInt(0x20)),
    SymbolToken("W_Handler") -> ConstantToken(BigInt(0x30)),
    SymbolToken("I_Handler") -> ConstantToken(BigInt(0x40)),
    SymbolToken("O_Handler") -> ConstantToken(BigInt(0x50)),
    SymbolToken("U_Handler") -> ConstantToken(BigInt(0x60)),
    SymbolToken("Z_Handler") -> ConstantToken(BigInt(0x70)),
    SymbolToken("X_Handler") -> ConstantToken(BigInt(0x80)),
    SymbolToken("StdIn") -> ConstantToken(BigInt(0x0)),
    SymbolToken("StdOut") -> ConstantToken(BigInt(0x1)),
    SymbolToken("StdErr") -> ConstantToken(BigInt(0x2)),
    SymbolToken("TextRead") -> ConstantToken(BigInt(0x0)),
    SymbolToken("TextWrite") -> ConstantToken(BigInt(0x1)),
    SymbolToken("BinaryRead") -> ConstantToken(BigInt(0x2)),
    SymbolToken("BinaryWrite") -> ConstantToken(BigInt(0x3)),
    SymbolToken("BinaryReadWrite") -> ConstantToken(BigInt(0x4)),
    SymbolToken("Halt") -> ConstantToken(BigInt(0x0)),
    SymbolToken("Fopen") -> ConstantToken(BigInt(0x1)),
    SymbolToken("Fclose") -> ConstantToken(BigInt(0x2)),
    SymbolToken("Fread") -> ConstantToken(BigInt(0x3)),
    SymbolToken("Fgets") -> ConstantToken(BigInt(0x4)),
    SymbolToken("Fgetws") -> ConstantToken(BigInt(0x5)),
    SymbolToken("Fwrite") -> ConstantToken(BigInt(0x6)),
    SymbolToken("Fputs") -> ConstantToken(BigInt(0x7)),
    SymbolToken("Fputws") -> ConstantToken(BigInt(0x8)),
    SymbolToken("Fseek") -> ConstantToken(BigInt(0x9)),
    SymbolToken("Ftell") -> ConstantToken(BigInt(0x10))
  )

}