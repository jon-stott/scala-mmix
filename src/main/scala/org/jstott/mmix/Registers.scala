package org.jstott.mmix

case class Registers(
                      arithmeticStatus: ArithmeticStatusRegister = ArithmeticStatusRegister(MmixOcta()),
                      bootstrapTrip: BootstrapTripRegister = BootstrapTripRegister(MmixOcta()),
                      cycleCounter: CycleCounterRegister = CycleCounterRegister(MmixOcta()),
                      dividend: DividendRegister = DividendRegister(MmixOcta()),
                      epsilon: EpsilonRegister = EpsilonRegister(MmixOcta()),
                      failureLocation: FailureLocationRegister = FailureLocationRegister(MmixOcta()),
                      globalThreshold: GlobalThresholdRegister = GlobalThresholdRegister(MmixOcta(32)),
                      himult: HimultRegister = HimultRegister(MmixOcta()),
                      intervalCounter: IntervalCounterRegister = IntervalCounterRegister(MmixOcta()),
                      returnJump: ReturnJumpRegister = ReturnJumpRegister(MmixOcta()),
                      interruptMask: InterruptMaskRegister = InterruptMaskRegister(MmixOcta()),
                      localThreshold: LocalThresholdRegister = LocalThresholdRegister(MmixOcta(2)),
                      multipleMask: MultipleMaskRegister = MultipleMaskRegister(MmixOcta()),
                      serialNumber: SerialNumberRegister = SerialNumberRegister(MmixOcta()),
                      registerStackOffset: RegisterStackOffsetRegister = RegisterStackOffsetRegister(MmixOcta()),
                      prediction: PredictionRegister = PredictionRegister(MmixOcta()),
                      interruptRequest: InterruptRequestRegister = InterruptRequestRegister(MmixOcta()),
                      remainder: RemainderRegister = RemainderRegister(MmixOcta()),
                      registerStackPointer: RegisterStackPointerRegister = RegisterStackPointerRegister(MmixOcta()),
                      trapAddress: TrapAddressRegister = TrapAddressRegister(MmixOcta()),
                      usageCounter: UsageCounterRegister = UsageCounterRegister(MmixOcta()),
                      virtualTranslation: VirtualTranslationRegister = VirtualTranslationRegister(MmixOcta()),
                      whereInterruptedTrip: WhereInterruptedTripRegister = WhereInterruptedTripRegister(MmixOcta()),
                      executionTrip: ExecutionTripRegister = ExecutionTripRegister(MmixOcta()),
                      yOperandTrip: YOperandTripRegister = YOperandTripRegister(MmixOcta()),
                      zOperandTrip: ZOperandTripRegister = ZOperandTripRegister(MmixOcta()),
                      bootstrapTrap: BootstrapTrapRegister = BootstrapTrapRegister(MmixOcta()),
                      dynamicTrapAddress: DynamicTrapAddressRegister = DynamicTrapAddressRegister(MmixOcta()),
                      whereInterruptedTrap: WhereInterruptedTrapRegister = WhereInterruptedTrapRegister(MmixOcta()),
                      executionTrap: ExecutionTrapRegister = ExecutionTrapRegister(MmixOcta()),
                      yOperandTrap: YOperandTrapRegister = YOperandTrapRegister(MmixOcta()),
                      zOperandTrap: ZOperandTrapRegister = ZOperandTrapRegister(MmixOcta())
                    ) {

  def rA: ArithmeticStatusRegister = arithmeticStatus
  def rB: BootstrapTripRegister = bootstrapTrip
  def rC: CycleCounterRegister = cycleCounter
  def rD: DividendRegister = dividend
  def rE: EpsilonRegister = epsilon
  def rF: FailureLocationRegister = failureLocation
  def rG: GlobalThresholdRegister = globalThreshold
  def rH: HimultRegister = himult
  def rI: IntervalCounterRegister = intervalCounter
  def rJ: ReturnJumpRegister = returnJump
  def rK: InterruptMaskRegister = interruptMask
  def rL: LocalThresholdRegister = localThreshold
  def rM: MultipleMaskRegister = multipleMask
  def rN: SerialNumberRegister = serialNumber
  def rO: RegisterStackOffsetRegister = registerStackOffset
  def rP: PredictionRegister = prediction
  def rQ: InterruptRequestRegister = interruptRequest
  def rR: RemainderRegister = remainder
  def rS: RegisterStackPointerRegister = registerStackPointer
  def rT: TrapAddressRegister = trapAddress
  def rU: UsageCounterRegister = usageCounter
  def rV: VirtualTranslationRegister = virtualTranslation
  def rW: WhereInterruptedTripRegister = whereInterruptedTrip
  def rX: ExecutionTripRegister = executionTrip
  def rY: YOperandTripRegister = yOperandTrip
  def rZ: ZOperandTripRegister = zOperandTrip
  def rBB: BootstrapTrapRegister = bootstrapTrap
  def rTT: DynamicTrapAddressRegister = dynamicTrapAddress
  def rWW: WhereInterruptedTrapRegister = whereInterruptedTrap
  def rXX: ExecutionTrapRegister = executionTrap
  def rYY: YOperandTrapRegister = yOperandTrap
  def rZZ: ZOperandTrapRegister = zOperandTrap

  def get(registerName: String): Option[Register] = {
    registerName match {
      case "rA" => Some(rA)
      case "rB" => Some(rB)
      case "rC" => Some(rC)
      case "rD" => Some(rD)
      case "rE" => Some(rE)
      case "rF" => Some(rF)
      case "rG" => Some(rG)
      case "rH" => Some(rH)
      case "rI" => Some(rI)
      case "rJ" => Some(rJ)
      case "rK" => Some(rK)
      case "rL" => Some(rL)
      case "rM" => Some(rM)
      case "rN" => Some(rN)
      case "rO" => Some(rO)
      case "rP" => Some(rP)
      case "rQ" => Some(rQ)
      case "rR" => Some(rR)
      case "rS" => Some(rS)
      case "rT" => Some(rT)
      case "rU" => Some(rU)
      case "rV" => Some(rV)
      case "rW" => Some(rW)
      case "rX" => Some(rX)
      case "rY" => Some(rY)
      case "rZ" => Some(rZ)
      case "rBB" => Some(rBB)
      case "rTT" => Some(rTT)
      case "rWW" => Some(rWW)
      case "rXX" => Some(rXX)
      case "rYY" => Some(rYY)
      case "rZZ" => Some(rZZ)
      case _ => None
    }
  }

}
