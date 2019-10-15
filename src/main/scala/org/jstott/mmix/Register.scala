package org.jstott.mmix

sealed trait Register {
  def value: MmixOcta
  def name: String
  def serial: Int
}

case class BootstrapTripRegister(value: MmixOcta) extends Register {
  override def name: String = "rB"
  override def serial: Int = 0
}
case class DividendRegister(value: MmixOcta) extends Register {
  override def name: String = "rD"
  override def serial: Int = 1
}
case class EpsilonRegister(value: MmixOcta) extends Register {
  override def name: String = "rE"
  override def serial: Int = 2
}
case class HimultRegister(value: MmixOcta) extends Register {
  override def name: String = "rH"
  override def serial: Int = 3
}
case class ReturnJumpRegister(value: MmixOcta) extends Register {
  override def name: String = "rJ"
  override def serial: Int = 4
}
case class MultipleMaskRegister(value: MmixOcta) extends Register {
  override def name: String = "rM"
  override def serial: Int = 5
}
case class RemainderRegister(value: MmixOcta) extends Register {
  override def name: String = "rR"
  override def serial: Int = 6
}
case class BootstrapTrapRegister(value: MmixOcta) extends Register {
  override def name: String = "rBB"
  override def serial: Int = 7
}
case class CycleCounterRegister(value: MmixOcta) extends Register {
  override def name: String = "rC"
  override def serial: Int = 8
}
case class SerialNumberRegister(value: MmixOcta) extends Register {
  override def name: String = "rN"
  override def serial: Int = 9
}
case class RegisterStackOffsetRegister(value: MmixOcta) extends Register {
  override def name: String = "rO"
  override def serial: Int = 10
}
case class RegisterStackPointerRegister(value: MmixOcta) extends Register {
  override def name: String = "rS"
  override def serial: Int = 11
}
case class IntervalCounterRegister(value: MmixOcta) extends Register {
  override def name: String = "rI"
  override def serial: Int = 12
}
case class TrapAddressRegister(value: MmixOcta) extends Register {
  override def name: String = "rT"
  override def serial: Int = 13
}
case class DynamicTrapAddressRegister(value: MmixOcta) extends Register {
  override def name: String = "rTT"
  override def serial: Int = 14
}
case class InterruptMaskRegister(value: MmixOcta) extends Register {
  override def name: String = "rK"
  override def serial: Int = 15
}
case class InterruptRequestRegister(value: MmixOcta) extends Register {
  override def name: String = "rQ"
  override def serial: Int = 16
}
case class UsageCounterRegister(value: MmixOcta) extends Register {
  override def name: String = "rU"
  override def serial: Int = 17
}
case class VirtualTranslationRegister(value: MmixOcta) extends Register {
  override def name: String = "rV"
  override def serial: Int = 18
}
case class GlobalThresholdRegister(value: MmixOcta) extends Register {
  override def name: String = "rG"
  override def serial: Int = 19
}
case class LocalThresholdRegister(value: MmixOcta) extends Register {
  override def name: String = "rL"
  override def serial: Int = 20
}
case class ArithmeticStatusRegister(value: MmixOcta) extends Register {
  override def name: String = "rA"
  override def serial: Int = 21
}
case class FailureLocationRegister(value: MmixOcta) extends Register {
  override def name: String = "rF"
  override def serial: Int = 22
}
case class PredictionRegister(value: MmixOcta) extends Register {
  override def name: String = "rP"
  override def serial: Int = 23
}
case class WhereInterruptedTripRegister(value: MmixOcta) extends Register {
  override def name: String = "rW"
  override def serial: Int = 24
}
case class ExecutionTripRegister(value: MmixOcta) extends Register {
  override def name: String = "rX"
  override def serial: Int = 25
}
case class YOperandTripRegister(value: MmixOcta) extends Register {
  override def name: String = "rY"
  override def serial: Int = 26
}
case class ZOperandTripRegister(value: MmixOcta) extends Register {
  override def name: String = "rZ"
  override def serial: Int = 27
}
case class WhereInterruptedTrapRegister(value: MmixOcta) extends Register {
  override def name: String = "rWW"
  override def serial: Int = 28
}
case class ExecutionTrapRegister(value: MmixOcta) extends Register {
  override def name: String = "rXX"
  override def serial: Int = 29
}
case class YOperandTrapRegister(value: MmixOcta) extends Register {
  override def name: String = "rYY"
  override def serial: Int = 30
}
case class ZOperandTrapRegister(value: MmixOcta) extends Register {
  override def name: String = "rZZ"
  override def serial: Int = 31
}
