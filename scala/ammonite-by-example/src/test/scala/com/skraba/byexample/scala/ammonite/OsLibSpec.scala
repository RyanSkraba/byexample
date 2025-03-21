package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io._

/** Tests on the [[https://github.com/com-lihaoyi/os-lib os-lib]] library. */
class OsLibSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  /** A helper method used to ensure that commands exist before running on the as a process */
  def commandExists(cmds: String*): Unit = cmds.foreach(cmd =>
    assume(
      os.proc("which", cmd).call(os.pwd, check = false).exitCode == 0
    )
  )

  describe("Manipulating files with os.Path") {

    it("should manage relative and absolute files") {
      // os.Path is always absolute
      val tmp: os.Path = os.root / "tmp"
      val home = os.root / "home"

      tmp.toString shouldBe "/tmp"

      // There's a class for relative paths too
      val relative = os.RelPath("next")
      (tmp / relative).toString shouldBe "/tmp/next"
      (tmp / "next").toString shouldBe "/tmp/next"
      (tmp / os.RelPath("next1/next2")).toString shouldBe "/tmp/next1/next2"

      tmp.relativeTo(home).toString shouldBe "../tmp"
      home.relativeTo(tmp).toString shouldBe "../home"
    }
  }

  describe("Running a command with os.proc(...).call(...)") {

    // Hello world scenario
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "greet").writeAll("Hello world!")

    it("should ignore a test if the command doesn't exist") {
      // How to check a command exists
      commandExists("no-exists")
      1 shouldBe 2
    }

    it("should run a test when the command exists") {
      // This command does exist
      commandExists("cat")
      val cmd = os.proc("cat", (Basic / "greet").toString).call(os.pwd)
      cmd.exitCode shouldBe 0
      cmd.out.text() shouldBe "Hello world!"
      cmd.err.text() shouldBe empty
    }

    it("does not send to console stdout by default") {
      commandExists("cat")
      // Success case
      withConsoleMatch {
        val cmd = os.proc("cat", (Basic / "greet").toString).call(os.pwd)
        cmd.exitCode shouldBe 0
        cmd.out.text() shouldBe "Hello world!"
        cmd.err.text() shouldBe empty
      } { case (_, stdout, stderr) =>
        // The console doesn't see anything.
        stdout shouldBe empty
        stderr shouldBe empty
      }
    }

    it("does not send to console stderr by default") {
      withConsoleMatch {
        // By default, stderr is captured by the test runner.
        val cmd = os
          .proc("cat", (Basic / "no-exists").toString)
          .call(os.pwd, check = false)
        cmd.exitCode shouldBe 1
        cmd.out.text() shouldBe empty
        cmd.err.text() shouldBe empty
      } { case (_, stdout, stderr) =>
        // The console doesn't see anything.
        stdout shouldBe empty
        stderr shouldBe empty
      }

      withConsoleMatch {
        // We can send stderr to a pipe, which captures the output in the command output
        val cmd = os
          .proc("cat", (Basic / "no-exists").toString)
          .call(os.pwd, stderr = os.Pipe, check = false)
        cmd.exitCode shouldBe 1
        cmd.out.text() shouldBe empty
        cmd.err
          .text() shouldBe s"cat: $Tmp/basic/no-exists: No such file or directory\n"
      } { case (_, stdout, stderr) =>
        // The console doesn't see anything.
        stdout shouldBe empty
        stderr shouldBe empty
      }
    }
  }
}
