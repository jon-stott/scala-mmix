package org.jstott.mmix

case class MmixTetra(t: Long) extends MmixData {

  override def toBytes: List[MmixByte] = List(
    MmixByte(((t >> 24) & 0xff).shortValue),
    MmixByte(((t >> 16) & 0xff).shortValue),
    MmixByte(((t >> 8) & 0xff).shortValue),
    MmixByte((t & 0xff).shortValue)
  )

  def &(toAnd: MmixTetra): MmixTetra = this & toAnd.t
  def &(toAnd: Long): MmixTetra = MmixTetra(t & toAnd)

  def !=(cmp: MmixTetra): Boolean = t != cmp.t
  def !=(cmp: Long): Boolean = t != cmp
  def <=(cmp: MmixTetra): Boolean = t <= cmp.t
  def <=(cmp: Long): Boolean = t <= cmp
  def <(cmp: MmixTetra): Boolean = t < cmp.t
  def <(cmp: Long): Boolean = t < cmp
  def >(cmp: MmixTetra): Boolean = t > cmp.t
  def >(cmp: Long): Boolean = t > cmp
  def >=(cmp: MmixTetra): Boolean = t >= cmp.t
  def >=(cmp: Long): Boolean = t >= cmp

  override def toString = f"MmixTetra($t|0x${t.toHexString}%8s)"

}

object MmixTetra {

  def apply(b0: MmixByte, b1: MmixByte, b2: MmixByte, b3: MmixByte): MmixTetra = {
    MmixTetra(b0.b << 24 + b1.b << 16 + b2.b << 8 + b3.b)
  }

  def apply(w0: MmixWyde, w1: MmixWyde): MmixTetra = {
    val w0b = w0.toBytes
    val w1b = w1.toBytes
    apply(w0b(0), w0b(1), w1b(0), w1b(1))
  }

}
