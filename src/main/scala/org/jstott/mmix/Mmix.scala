package org.jstott.mmix

case class Mmix(
               memory: Map[BigInt, MmixByte] = Map(),
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

  def withUpdatedMemory(loc: BigInt, bytes: List[MmixData]): Mmix = {
    this.copy(memory = bytes.zipWithIndex.foldLeft(memory) { case (m, (d, i)) =>
      d.toBytes.zipWithIndex.foldLeft(m) { case (m2, (b, j)) =>
        m2.updated(loc + i + j, b)
      }
    })
  }

}
