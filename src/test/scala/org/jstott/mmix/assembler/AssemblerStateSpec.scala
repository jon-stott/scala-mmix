package org.jstott.mmix.assembler

import org.jstott.mmix.Mmix
import org.scalatest.{ FlatSpec, MustMatchers }

class AssemblerStateSpec extends FlatSpec with MustMatchers {

  behavior of "withAlignedCurrentLocation"

  it should "align an LDA token (align to octa/4 bytes) with currentLocation = 0" in {
    val state = AssemblerState(mmix = Mmix(), currentLocation = 0)
    val result = state.withAlignedCurrentLocation(LdaToken)
    result.currentLocation mustBe 0
  }

  it should "align an LDA token (align to octa/4 bytes) with currentLocation = 1" in {
    val state = AssemblerState(mmix = Mmix(), currentLocation = 1)
    val result = state.withAlignedCurrentLocation(LdaToken)
    result.currentLocation mustBe 4
  }

  it should "align an LDA token (align to octa/4 bytes) with currentLocation = 2" in {
    val state = AssemblerState(mmix = Mmix(), currentLocation = 2)
    val result = state.withAlignedCurrentLocation(LdaToken)
    result.currentLocation mustBe 4
  }

  it should "align an LDA token (align to octa/4 bytes) with currentLocation = 3" in {
    val state = AssemblerState(mmix = Mmix(), currentLocation = 3)
    val result = state.withAlignedCurrentLocation(LdaToken)
    result.currentLocation mustBe 4
  }

  it should "align an LDA token (align to octa/4 bytes) with currentLocation = 4" in {
    val state = AssemblerState(mmix = Mmix(), currentLocation = 4)
    val result = state.withAlignedCurrentLocation(LdaToken)
    result.currentLocation mustBe 4
  }

  it should "align an LDA token (align to octa/4 bytes) with currentLocation = 5" in {
    val state = AssemblerState(mmix = Mmix(), currentLocation = 5)
    val result = state.withAlignedCurrentLocation(LdaToken)
    result.currentLocation mustBe 8
  }

}
