package org.jstott.mmix.assembler.exception

import org.jstott.mmix.assembler.PrimaryToken

class UnexpectedRegisterException(override val expression: PrimaryToken) extends RegisterException {

  override def getMessage: String = s"Expression must not be a register but was: $expression"

}