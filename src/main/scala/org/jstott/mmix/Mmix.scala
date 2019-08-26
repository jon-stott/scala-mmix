package org.jstott.mmix

case class Mmix(
               memory: Map[BigInt, MmixByte],
               generalRegisters: Seq[MmixOcta] = Seq.fill(256)(MmixOcta()),
               registers: Registers = Registers()
               ) {

  def generalRegister(index: Int): MmixOcta = {
    if (index < 0 || index > 255) {
      // TODO error
      MmixOcta()
    }
    generalRegisters(index)
  }

}
