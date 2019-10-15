package org.jstott.mmix.assembler

import org.jstott.mmix.assembler.exception.{ AssemblerException, NotARegisterException, RegisterException, UnexpectedRegisterException }
import org.jstott.mmix.{ Mmix, MmixByte, MmixData, MmixOcta, MmixTetra, MmixWyde }

class Assembler(code: String) {

  def assemble: AssemblerState = {
    val mmixParser = new MmixParser
    val finalState = mmixParser.apply(code) match {
      case Left(error) => throw new IllegalArgumentException(s"syntax error: $error")
      case Right(lines) =>
        val firstPass = lines.filter(_.isInstanceOf[MmixProgramLine]).foldLeft(AssemblerState(Mmix())) { case (state, line: MmixProgramLine) =>
          try {
            handleToken(state, line)
          } catch {
            case re: RegisterException => throw new AssemblerException(line, re)
          }
        }
        firstPass.linesWithForwardReferences.foldLeft(firstPass) { case (state, key@(location, line)) =>
          val alteredLocationCounter = state.copy(
            currentLocation = location,
            linesWithForwardReferences = state.linesWithForwardReferences - key
          )
          val newState = handleToken(alteredLocationCounter, line)
          newState
        }
    }
    // $255 needs to contain the location of Main
    val mainLine = MmixOcta(finalState.labels.getOrElse(SymbolToken("Main"), ConstantToken(BigInt(0))).n.longValue)
    finalState.copy(
      mmix = finalState.mmix.copy(
        generalRegisters = finalState.mmix.generalRegisters.updated(255, mainLine)
      )
    )
  }

  private def handleToken(state: AssemblerState, line: MmixProgramLine): AssemblerState = {
    line.opcode match {
      case a if a.isInstanceOf[AssemblerToken] => handleAssemblerToken(state, line)
      case a if a.isInstanceOf[OperatorToken] => handleOperatorToken(state, line)
    }
  }

  private def handleAssemblerToken(state: AssemblerState, line: MmixProgramLine): AssemblerState = {

    val stateWithAlignedLocation = state.withAlignedCurrentLocation(line.opcode)

    val stateAfterGregHandled = line.opcode match {
      case GregToken  => handleGreg(stateWithAlignedLocation, line)
      case _ => stateWithAlignedLocation
    }

    val stateWithUpdatedLabel = updateSymbols(stateAfterGregHandled, line)

    line.opcode match {
      case LocToken   => handleLoc(stateWithUpdatedLabel, line)
      case ByteToken  => handleData(stateWithUpdatedLabel, line, { d: BigInt => MmixByte(d.shortValue) })
      case WydeToken  => handleData(stateWithUpdatedLabel, line, { d: BigInt => MmixWyde(d.intValue) })
      case TetraToken => handleData(stateWithUpdatedLabel, line, { d: BigInt => MmixTetra(d.longValue) })
      case OctaToken  => handleData(stateWithUpdatedLabel, line, { d: BigInt => MmixOcta(d.longValue) })
      case IsToken    => handleIs(stateWithUpdatedLabel, line)
      case SetToken   => handleSet(stateWithUpdatedLabel, line)
      case _          => stateWithUpdatedLabel
    }

  }

  private def handleOperatorToken(state: AssemblerState, line: MmixProgramLine): AssemblerState = {

    val operator: OperationToken = line.opcode.asInstanceOf[OperatorToken]
    val toAdvanceCurrentLocationBy = 1 << ((operator.opBits.t & OperatorToken.alignBits) >> 16)

    // Check for special mode
    if (state.isSpecialMode && !operator.canBeUsedInSpecialMode) {
      // TODO error - can't use operator in special mode
    }

    val stateWithAlignedLocation = state.withAlignedCurrentLocation(line.opcode)
    val stateWithUpdatedLabel = if (!operator.isLabelIgnored) updateSymbols(stateWithAlignedLocation, line) else stateWithAlignedLocation

    if (line.containsForwardReference(state)) {
      return stateWithUpdatedLabel.copy(
        currentLocation = stateWithUpdatedLabel.currentLocation + toAdvanceCurrentLocationBy,
        linesWithForwardReferences = stateWithUpdatedLabel.linesWithForwardReferences ++ Set((stateWithUpdatedLabel.currentLocation, line))
      )
    }

    val evaluatedExpressions = line.evaluatedExpr(state)
    val isLastExpressionPure = evaluatedExpressions.nonEmpty && evaluatedExpressions.last.unaryOperator != RegisterizationToken
    val incOpcode = isLastExpressionPure && operator.isImmediate
    // FIXME must be a way to refactor this to avoid a var!
    var opcode = operator.opcode + (if (incOpcode) 1 else 0).shortValue

    val fieldBytes = if (operator.isManyArg) {
      // TODO handle many arg opcode
      List.empty
    } else {
      operator match {
        case o: OperatorToken if (o.isOneArg && evaluatedExpressions.length == 1) =>
          // TODO handle one-arg operator
          val (xyz, isRelativeAddressBackwards) = doXYZ(o, evaluatedExpressions(0), state)
          if (isRelativeAddressBackwards) opcode = opcode + 1.shortValue
          xyz.toMmixTetra.toBytes.tail // Tetra is four bytes but we only want the last six bytes
          // TODO what if xyz is negative?
        case o: OperatorToken if (o.isTwoArg && evaluatedExpressions.length == 2) =>
          val (yz, isRelativeAddressBackwards) = doYZ(o, evaluatedExpressions(1), state)
          val x = doX(o, evaluatedExpressions(0))
          if (isRelativeAddressBackwards) opcode = opcode + 1.shortValue
          List(x.toMmixByte, yz.toMmixWyde)
        case o: OperatorToken if (o.isThreeArg && evaluatedExpressions.length == 3) =>
          val z = doZ(o, evaluatedExpressions(2))
          val y = doY(o, evaluatedExpressions(1))
          // TODO val yz = (y << 8) + z
          val x = doX(o, evaluatedExpressions(0))
          // TODO val xyz = (x << 16) + yz
          List(x.toMmixByte, y.toMmixByte, z.toMmixByte)
        case _ =>
          // TODO error!
          List(MmixByte(), MmixByte(), MmixByte())
      }
    }

    val bytes = opcode +: fieldBytes

    stateWithUpdatedLabel.copy(
      mmix = stateWithUpdatedLabel.mmix.withUpdatedMemory(stateWithUpdatedLabel.currentLocation, bytes),
      currentLocation = stateWithUpdatedLabel.currentLocation + toAdvanceCurrentLocationBy
    )
  }

  private def doZ(o: OperationToken, expr: PrimaryToken): PrimaryToken = {
    // TODO sort the z field
    if (expr.isRegister) {
      if (!o.canZBeRegister) {
        throw new UnexpectedRegisterException(expr)
      }
    } else if (o.mustZBeRegister) {
      throw new NotARegisterException(expr)
    }
    expr
  }

  private def doY(o: OperationToken, expr: PrimaryToken): PrimaryToken = {
    if (expr.isRegister) {
      if (!o.canYBeRegister) {
        throw new UnexpectedRegisterException(expr)
      }
    } else if (o.mustYBeRegister) {
      throw new NotARegisterException(expr)
    }
    expr
  }

  private def doX(o: OperationToken, expr: PrimaryToken): PrimaryToken = {
    if (expr.isRegister) {
      if (!o.canXBeRegister) {
        throw new UnexpectedRegisterException(expr)
      }
    } else if (o.mustXBeRegister) {
      throw new NotARegisterException(expr)
    }
    expr
  }

  private def doYZ(o: OperationToken, expr: PrimaryToken, state: AssemblerState): (PrimaryToken, Boolean) = {
    if (expr.isRegister) {
      if (!o.canYZBeRegister) {
        throw new UnexpectedRegisterException(expr)
      }
      // TODO if opcode is SET, left-shift expr value by 8 and set opcode to 0xc1 (OR)
      // TODO else if memBit is set, left-shift expr value by 8 (silently append ',0')
    } else {
      if (o.mustYZBeMemoryLocation) {
        // FIXME refactor to avoid using var
        var octa = expr.toMmixOcta
        var k = 0
        var acc: MmixOcta = MmixOcta()
        for (j <- state.greg until 255) {
          val jRegVal = state.mmix.generalRegisters(j)
          if (jRegVal.o != 0) {
            acc = MmixOcta(expr.toMmixOcta.o - jRegVal.o)
            if (acc.h.t <= octa.h.t && (acc.l.t <= octa.l.t || acc.h.t < octa.h.t)) {
              octa = acc
              k = j
            }
          }
        }
        if (octa.l.t <= 0xff && octa.h.t == 0 && k != 0) {
          val res = ConstantToken((k << 8) + octa.l.t)
          return (res, false)
          // TODO else if (!expanding)...
        } else {
          for (j <- 0xe0 to 0xeb) {
            val yz = j & 3 match {
              case 0 => octa.h.t >> 16    // SETH
              case 1 => octa.h.t & 0xffff // SETMH or ORMH
              case 2 => octa.l.t >> 16    // SETML or ORML
              case 3 => octa.l.t & 0xffff // SETL or ORL
            }
            if (yz != 0 || j == 0xe3) {
              // TODO assemble(4,(j<<24)+(255<<16)+yz,0)
              // TODO j |= 0xe8
            }
          }
          if (k != 0) {
            // Y = $k, Z = $255
            return (ConstantToken((k << 8) + 255), false)
          } else {
            // Y = $255, Z = 0
            return (ConstantToken(0xffff0000), false)
          }
        }
      }
      // TODO if opcode is SET, set opcode to 0xe3 (SETL)
      if (o.mustYZBeRegister) {
        throw new NotARegisterException(expr)
      }
      if (o.isRelativeAddress) {
        val source = MmixOcta(state.currentLocation.longValue) >> 2
        val dest = expr.toMmixOcta >> 2
        val diff = dest - source
        if (diff > 0xffff) {
          // TODO error - relative address is more than 0xffff tetrabytes forward
        } else if (diff < -0xffff) {
          // TODO error - relative address is more than 0xffff tetrabytes behind
        } else {
          return (ConstantToken(diff.o), diff < 0)
        }
      }
    }
    (expr, false)
  }

  private def doXYZ(o: OperationToken, expr: PrimaryToken, state: AssemblerState): (PrimaryToken, Boolean) = {
    if (expr.isRegister) {
      if (!o.canXYZBeRegister) {
        throw new UnexpectedRegisterException(expr)
      }
    } else {
      if (o.mustXYZBeRegister) {
        // TODO error - xyz must be a register but isn't
      }
      if (o.isRelativeAddress) {
        // FIXME refactor this - similar to doXYZ above
        val source = MmixOcta(state.currentLocation.longValue) >> 2
        val dest = expr.toMmixOcta >> 2
        val diff = dest - source
        if (diff > 0xffffff) {
          // TODO error - relative address is more than 0xffffff tetrabytes forward
        } else if (diff < -0xffffff) {
          // TODO error - relative address is more than 0xffffff tetrabytes behind
        } else {
          return (ConstantToken(diff.o), diff < 0)
        }
      }
    }
    // TODO handle normal XYZ value
    (expr, false)
  }

  private def handleLoc(state: AssemblerState, line: MmixProgramLine): AssemblerState = {
    line.evaluatedExpr(state).headOption match {
      case Some(ConstantToken(n, uo)) =>
        // TODO handle unary operator
        state.copy(currentLocation = n)
      case _ => ??? // TODO Error - no expression
    }
  }

  private def handleGreg(state: AssemblerState, line: MmixProgramLine): AssemblerState = {
    val exprValue = if (line.expr.nonEmpty) {
      line.evaluatedExpr(state).head match {
        case ConstantToken(n, uo) =>
          // TODO handle unary operator
          MmixOcta(n.longValue)
        case CurrentLocationToken =>
          MmixOcta(state.currentLocation.longValue)
      }
    } else {
      MmixOcta(0)
    }
    if (exprValue.o != 0) {
      for (j <- state.globalRegisterAllocator until 255) {
        if (state.mmix.generalRegisters(j).o == exprValue.o) {
          return state.copy(currentGlobalRegister = j)
        }
      }
    }
    if (state.globalRegisterAllocator == 32) {
      // TODO error - too many global registers
    }
    val newCurrentGlobalRegister = state.globalRegisterAllocator - 1
    state.copy(
      globalRegisterAllocator = newCurrentGlobalRegister,
      currentGlobalRegister = newCurrentGlobalRegister,
      mmix = state.mmix.copy(generalRegisters = state.mmix.generalRegisters.updated(newCurrentGlobalRegister, exprValue))
    )
  }

  private def handleData(state: AssemblerState, line: MmixProgramLine, f: BigInt => MmixData): AssemblerState = {
    line.expr.foldLeft(state) { (newState, expr) =>
      expr.evaluate(newState) match {
        // TODO symbol
        case ConstantToken(c, uo) =>
          // TODO handle unary operator
          val bytes = f(c).toBytes
          newState.copy(
            mmix = newState.mmix.withUpdatedMemory(newState.currentLocation, bytes),
            currentLocation = newState.currentLocation + bytes.length
          )
        // TODO current location
        case StringToken(cs, uo) =>
          // TODO handle unary operator
          cs.foldLeft(newState) { (newNewState, c) =>
            val bytes = f(c).toBytes
            newNewState.copy(
              mmix = newNewState.mmix.withUpdatedMemory(newNewState.currentLocation, bytes),
              currentLocation = newNewState.currentLocation + bytes.length
            )
          }
      }
    }
  }

  private def handleIs(state: AssemblerState, line: MmixProgramLine): AssemblerState = {
    val evaluatedExpr = line.evaluatedExpr(state).head
    val exprValue = evaluatedExpr match {
      case ConstantToken(n, _) =>
        // TODO handle unary operator
        MmixOcta(n.longValue)
      case CurrentLocationToken =>
        MmixOcta(state.currentLocation.longValue)
    }
    line.label match {
      case Some(LabelToken(SymbolToken.LocalSymbolPattern(_, "H"))) | None =>
        state
      case Some(LabelToken(s)) =>
        state.copy(
          labels = state.labels.updated(SymbolToken(s), ConstantToken(exprValue.o, evaluatedExpr.unaryOperator))
        )
    }
  }

  private def handleSet(state: AssemblerState, line: MmixProgramLine): AssemblerState = {
    if (line.expr.length == 2) {
      val exprValue = line.expr(1).evaluate(state)
      val newLine = if (exprValue.isRegister) {
        // Treat as OR $X,$Y,0
        line.copy(opcode = OrToken, expr = line.expr :+ Expression(tokens = List(ConstantToken(0))))
      } else {
        // Treat as SETL $X,Y
        line.copy(opcode = SetlToken)
      }
      handleOperatorToken(state, newLine)
    } else {
      // TODO error - needs two expressions
      state
    }
  }

  private def updateSymbols(state: AssemblerState, line: MmixProgramLine): AssemblerState = {
    line.label match {
      case Some(LabelToken(SymbolToken.LocalSymbolPattern(n, "H"))) =>
        val labelNum = n.toInt
        val updated = state.localLabels.get(labelNum) match {
          case Some(existingLocalLabel) => existingLocalLabel + state.currentLocation
          case None => Set(state.currentLocation)
        }
        state.copy(localLabels = state.localLabels + (labelNum -> updated))
      case Some(LabelToken(s)) if line.opcode == GregToken =>
        // if the opcode is GREG, use the current global register as the value for the label
        state.copy(
          labels = state.labels.updated(SymbolToken(s), ConstantToken(state.cur_greg, RegisterizationToken))
        )
      case Some(LabelToken(s)) =>
        // if the opcode is not GREG, use the current location as the value for the label
        state.copy(
          labels = state.labels.updated(SymbolToken(s), ConstantToken(state.currentLocation))
        )
      case None =>
        state
    }
  }

}
