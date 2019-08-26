package org.jstott.mmix

case class MmixOcta(o: Long = 0)

object MmixOcta {

  def apply(b0: MmixByte, b1: MmixByte, b2: MmixByte, b3: MmixByte): MmixOcta = {
    MmixOcta(b0.b << 24 + b1.b << 16 + b2.b << 8 + b3.b)
  }

}
