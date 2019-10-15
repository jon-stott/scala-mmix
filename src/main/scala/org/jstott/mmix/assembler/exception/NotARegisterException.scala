package org.jstott.mmix.assembler.exception

import org.jstott.mmix.assembler.PrimaryToken

class NotARegisterException(override val expression: PrimaryToken) extends RegisterException {

  override def getMessage: String = s"Expression must be a register but wasn't: $expression"

}