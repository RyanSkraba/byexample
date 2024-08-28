package com.skraba.byexample.scala.ammonite.validator

import com.skraba.docoptcli.AnsiConsole

/** A status message is a list of messages to store as a consequence of running validations. These are for
  * human-readability reporting.
  */
sealed trait StatusMessage {

  /** The check that was
    */
  val name: Any
  val text: String
  val fatal: Boolean
}

case class Ok(name: Any, text: String) extends StatusMessage {
  val fatal = false
}

case class Warn(name: Any, text: String) extends StatusMessage {
  val fatal = false
}

case class Msg(name: Any, text: String) extends StatusMessage {
  val fatal = false
}

case class Error(name: Any, text: String) extends StatusMessage {
  val fatal = true
}

object StatusMessage {
  def assertThat(out: AnsiConsole, name: Any, expected: Any, actual: Any): StatusMessage = {
    if (expected == actual) Ok(name, out.ok(s"$expected == $actual"))
    else Error(name, out.error(s"$expected != $actual"))
  }

}

case class Validator(checks: Seq[StatusMessage] = Seq.empty) {
  def doStep[T](out: AnsiConsole, head1: Any, head2: Any = "")(step: => T): T = {
    out.vPrintln(out.msg(head1, head2, bold = true))
    val cmd = step
    cmd match {
      case result: os.CommandResult => out.vPrintln(out.msg2(result.out.lines().mkString("\n")))
      case _                        =>
    }
    cmd
  }
}
