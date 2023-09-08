package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET, YELLOW}
import scala.reflect.io._
import scala.sys.process.stdout
import scala.util.Properties

/** Tests on the [[https://github.com/com-lihaoyi/os-lib os-lib]] library. */
class OsLibSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  describe("Running a command with os.proc()") {

    // Hello world scenario
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "greet").writeAll("Hello world!")

    it("should ignore a test if the command doesn't exist") {
      // How to check a command exists
      assume(os.proc("which", "no-exist").call(os.pwd, check = false).exitCode == 0)
      1 shouldBe 2
    }

    it("should run a test when the command exists") {
      // This command does exist
      assume(os.proc("which", "cat").call(os.pwd, check = false).exitCode == 0)
      val cmd = os.proc("cat", (Basic/"greet").toString).call(os.pwd)
      cmd.exitCode shouldBe 0
      cmd.out.lines() shouldBe Seq("Hello world!")
      cmd.err.lines() shouldBe Seq.empty
    }
  }
}
