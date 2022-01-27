package com.skraba.byexample.scala

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.util.{Success, Try}
import sys.process._

/** Running a system process in Scala
  *
  * @see
  *   https://alvinalexander.com/scala/how-to-handle-stdout-stderr-external-system-commands-scala/
  */
class SysProcessSpec extends AnyFunSpecLike with Matchers {

  /** @return true if echo, can and grep exists to be used for system exec */
  def checkCommands(): Boolean = {
    Try("echo ECHO CHECK".#|("grep CHECK").!!) == Success(
      "ECHO CHECK\n"
    ) && Try("cat".!!) == Success("")

  }

  describe("Running a system process") {

    it("should be simple with .! and .!!") {
      assume(checkCommands())

      // Get the return status or values.
      "echo Hello world".! shouldBe 0
      "echo Hello world".!! shouldBe "Hello world\n"
      Seq("echo", "-n", "Hello", "world").!! shouldBe "Hello world\n"

      Process("echo Hello world").!
      Process("echo Hello world").run.exitValue() shouldBe 0
    }

    it("allows you to pipe stdout and stdin") {
      assume(checkCommands())

      // Pipe to another process.
      "echo Hello world".#|("grep -i Hello").!! shouldBe "Hello world\n"

      // Pipe to a function.
      "echo Hello world".!!(ProcessLogger(_ => ())) shouldBe "Hello world\n"

      // Pipe a string into a command (this could also a be a File or URL)
      val text = "Hello world"
      (Seq("cat") #< new ByteArrayInputStream(
        text.getBytes(StandardCharsets.UTF_8)
      )).!! shouldBe "Hello world\n"

      // Use string builders to save console output.
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val status =
        "echo Hello world".!(ProcessLogger(stdout.append(_), stderr.append(_)))
      status shouldBe 0
      stdout.toString() shouldBe "Hello world"
      stderr.size shouldBe 0
    }
  }

}
