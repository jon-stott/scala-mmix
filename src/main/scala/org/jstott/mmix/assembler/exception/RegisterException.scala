package org.jstott.mmix.assembler.exception

import org.jstott.mmix.assembler.PrimaryToken

abstract class RegisterException extends RuntimeException {

  def expression: PrimaryToken

}
