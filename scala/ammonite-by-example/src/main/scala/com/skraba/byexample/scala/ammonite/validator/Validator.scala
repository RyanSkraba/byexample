package com.skraba.byexample.scala.ammonite.validator

import com.tinfoiled.docopt4s.AnsiConsole

/** [[StatusMessage]]s are generated as a consequence of running checks. They can be warnings, errors or just messages.
  */
sealed trait StatusMessage {

  /** A brief name describing the message. */
  val name: Any

  /** Some information about the status that occurred. */
  val text: String

  /** True if the message indicates that a fatal error occurred and processing cannot continue. */
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

  /** Create a StatusMessage that is [[Ok]] if the expected and actual values are the same, otherwise [[Error]].
    *
    * @param out
    *   The console to use for formatting output.
    * @param name
    *   A brief name describing the check.
    * @param expected
    *   The expected value.
    * @param actual
    *   The actual value.
    * @return
    *   A [[StatusMessage]] indicating the result of the check.
    */
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
