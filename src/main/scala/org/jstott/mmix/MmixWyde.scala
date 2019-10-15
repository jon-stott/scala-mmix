package org.jstott.mmix

case class MmixWyde(w: Int) extends MmixData {

  override def toBytes: List[MmixByte] = List(
    MmixByte(((w & 0xff00) >> 8).shortValue),
    MmixByte((w & 0xff).shortValue)
  )

  override def toString = f"MmixWyde($w|0x${w.toHexString}%4s)"

}

object MmixWyde {

  def apply(b0: MmixByte, b1: MmixByte): MmixWyde = {
    MmixWyde(b0.b << 8 + b1.b)
  }

}
