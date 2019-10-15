package org.jstott.mmix.assembler.exception

import org.jstott.mmix.assembler.MmixProgramLine

class AssemblerException(line: MmixProgramLine, cause: Throwable) extends RuntimeException {

  override def getMessage: String = cause.getMessage + ":\n " + line.toString

}