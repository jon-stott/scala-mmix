package org.jstott.mmix

case class MmixOcta(o: Long = 0) extends MmixData {

  lazy val h: MmixTetra = MmixTetra(o >> 32)
  lazy val l: MmixTetra = MmixTetra(o & 0xffffffff)

  override def toBytes: List[MmixByte] = List(
    MmixByte(((o >> 56) & 0xff).shortValue),
    MmixByte(((o >> 48) & 0xff).shortValue),
    MmixByte(((o >> 40) & 0xff).shortValue),
    MmixByte(((o >> 32) & 0xff).shortValue),
    MmixByte(((o >> 24) & 0xff).shortValue),
    MmixByte(((o >> 16) & 0xff).shortValue),
    MmixByte(((o >> 8) & 0xff).shortValue),
    MmixByte((o & 0xff).shortValue)
  )

  def +(toAdd: MmixOcta): MmixOcta = this + toAdd.o
  def +(toAdd: Long): MmixOcta = MmixOcta(toAdd + o)

  def -(toSub: MmixOcta): MmixOcta = this - toSub.o
  def -(toSub: Long): MmixOcta = MmixOcta(o - toSub)

  def &(toAnd: MmixOcta): MmixOcta = this & toAnd.o
  def &(toAnd: Long): MmixOcta = MmixOcta(o & toAnd)

  def >>(toShift: Int): MmixOcta = MmixOcta(o >> toShift)

  def !=(cmp: MmixOcta): Boolean = o != cmp.o
  def <=(cmp: MmixOcta): Boolean = o <= cmp.o
  def <(cmp: MmixOcta): Boolean = o < cmp.o
  def <(cmp: Long): Boolean = o < cmp
  def >(cmp: MmixOcta): Boolean = o > cmp.o
  def >(cmp: Long): Boolean = o > cmp
  def >=(cmp: MmixOcta): Boolean = o >= cmp.o

  def ==(cmp: MmixOcta): Boolean = o == cmp.o
  def ==(cmp: Long): Boolean = o == cmp

  override def toString = f"MmixOcta($o|0x${o.toHexString}%16s)"

}

object MmixOcta {

  def apply(b0: MmixByte, b1: MmixByte, b2: MmixByte, b3: MmixByte, b4: MmixByte, b5: MmixByte, b6: MmixByte, b7: MmixByte): MmixOcta = {
    MmixOcta(b0.b << 56 + b1.b << 48 + b2.b << 40 + b3.b << 32 + b4.b << 24 + b5.b << 16 + b6.b << 8 + b7.b)
  }

  def apply(w0: MmixWyde, w1: MmixWyde, w2: MmixWyde, w3: MmixWyde): MmixOcta = {
    val w0b = w0.toBytes
    val w1b = w1.toBytes
    val w2b = w2.toBytes
    val w3b = w3.toBytes
    apply(w0b(0), w0b(1), w1b(0), w1b(1), w2b(0), w2b(1), w3b(0), w3b(1))
  }

  def apply(t0: MmixTetra, t1: MmixTetra): MmixOcta = {
    val t0b = t0.toBytes
    val t1b = t1.toBytes
    apply(t0b(0), t0b(1), t0b(2), t0b(3), t1b(0), t1b(1), t1b(2), t1b(3))
  }

}
