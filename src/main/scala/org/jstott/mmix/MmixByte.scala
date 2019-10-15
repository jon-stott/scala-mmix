package org.jstott.mmix

case class MmixByte(b: Short = 0) extends MmixData {

  override def toBytes: List[MmixByte] = List(this)

  def +(o: MmixByte): MmixByte = this + o.b
  def +(o: Short): MmixByte = MmixByte((b + o).shortValue)

  override def toString = f"MmixByte($b|0x${b.toHexString}%2s)"

}
