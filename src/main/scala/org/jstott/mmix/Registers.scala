package org.jstott.mmix

case class Registers(
                      arithmeticStatus: MmixOcta = MmixOcta(),
                      bootstrapTrip: MmixOcta = MmixOcta(),
                      cycleCounter: MmixOcta = MmixOcta(),
                      dividend: MmixOcta = MmixOcta(),
                      epsilon: MmixOcta = MmixOcta(),
                      failureLocation: MmixOcta = MmixOcta(),
                      globalThreshold: MmixOcta = MmixOcta(),
                      himult: MmixOcta = MmixOcta(),
                      intervalCounter: MmixOcta = MmixOcta(),
                      returnJump: MmixOcta = MmixOcta(),
                      interruptMask: MmixOcta = MmixOcta(),
                      localThreshold: MmixOcta = MmixOcta(),
                      multipleMask: MmixOcta = MmixOcta(),
                      serialNumber: MmixOcta = MmixOcta(),
                      registerStackOffset: MmixOcta = MmixOcta(),
                      prediction: MmixOcta = MmixOcta(),
                      interruptRequest: MmixOcta = MmixOcta(),
                      remainder: MmixOcta = MmixOcta(),
                      registerStackPointer: MmixOcta = MmixOcta(),
                      trapAddress: MmixOcta = MmixOcta(),
                      usageCounter: MmixOcta = MmixOcta(),
                      virtualTranslation: MmixOcta = MmixOcta(),
                      whereInterruptedTrip: MmixOcta = MmixOcta(),
                      executionTrip: MmixOcta = MmixOcta(),
                      yOperandTrip: MmixOcta = MmixOcta(),
                      zOperandTrip: MmixOcta = MmixOcta(),
                      bootstrapTrap: MmixOcta = MmixOcta(),
                      dynamicTrapAddress: MmixOcta = MmixOcta(),
                      whereInterruptedTrap: MmixOcta = MmixOcta(),
                      executionTrap: MmixOcta = MmixOcta(),
                      yOperandTrap: MmixOcta = MmixOcta(),
                      zOperandTrap: MmixOcta = MmixOcta()
                    ) {

  def rA: MmixOcta = arithmeticStatus
  def rB: MmixOcta = bootstrapTrip
  def rC: MmixOcta = cycleCounter
  def rD: MmixOcta = dividend
  def rE: MmixOcta = epsilon
  def rF: MmixOcta = failureLocation
  def rG: MmixOcta = globalThreshold
  def rH: MmixOcta = himult
  def rI: MmixOcta = intervalCounter
  def rJ: MmixOcta = returnJump
  def rK: MmixOcta = interruptMask
  def rL: MmixOcta = localThreshold
  def rM: MmixOcta = multipleMask
  def rN: MmixOcta = serialNumber
  def rO: MmixOcta = registerStackOffset
  def rP: MmixOcta = prediction
  def rQ: MmixOcta = interruptRequest
  def rR: MmixOcta = remainder
  def rS: MmixOcta = registerStackPointer
  def rT: MmixOcta = trapAddress
  def rU: MmixOcta = usageCounter
  def rV: MmixOcta = virtualTranslation
  def rW: MmixOcta = whereInterruptedTrip
  def rX: MmixOcta = executionTrip
  def rY: MmixOcta = yOperandTrip
  def rZ: MmixOcta = zOperandTrip
  def rBB: MmixOcta = bootstrapTrap
  def rTT: MmixOcta = dynamicTrapAddress
  def rWW: MmixOcta = whereInterruptedTrap
  def rXX: MmixOcta = executionTrap
  def rYY: MmixOcta = yOperandTrap
  def rZZ: MmixOcta = zOperandTrap

}
