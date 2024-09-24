package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.OsPathScalaRelectIOConverters._
import com.skraba.docoptcli.AnsiConsole
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.io.AnsiColor._
import scala.reflect.io.Directory

/** Test the [[FileMaker]] class. */
class FileMakerSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  describe("Create a basic file") {

    it("should create the file as needed") {
      // Create a styled instance from a verbose template
      val basic = FileMaker(out = AnsiConsole(verbose = true))
      val styled = basic.black(Tmp / "txt" / "greeting.txt", "txt") { out =>
        // The directory is created automatically internally
        (Tmp / "txt").jfile should exist
        out.toIO shouldNot exist
        out.writeAll("Hello world!")
      }

      // Before creating, the file shouldn't exist
      AmmoniteScriptSpecBase.withConsoleMatch {
        (Tmp / "txt").jfile shouldNot exist
        styled.dst
        (Tmp / "txt").jfile should exist
        styled.dst
      } { case (result, stdout, stderr) =>
        result.value shouldBe reflectPathToOsPath(Tmp / "txt" / "greeting.txt")
        stdout shouldBe s"${BLACK}(txt${RESET}${BLACK})${RESET}"
        stderr shouldBe empty
      }

      (Tmp / "txt" / "greeting.txt").jfile should exist
      (Tmp / "txt" / "greeting.txt").toFile.slurp() shouldBe "Hello world!"
    }

    it("should not overwrite the file if it exists") {
      // Create a file immediately
      FileMaker(dstPath = Some(Tmp / "txt" / "greeting.txt"), thunk = _.writeAll("Hello world!")).dst
      (Tmp / "txt" / "greeting.txt").jfile should exist
      (Tmp / "txt" / "greeting.txt").toFile.slurp() shouldBe "Hello world!"

      // Create a styled instance from a verbose template
      val basic = FileMaker(out = AnsiConsole(verbose = true))
      val styled = basic.white(Tmp / "txt" / "greeting.txt", "txt") { _ => fail("This shouldn't be called") }

      // When using the styled instance, it shouldn't call it's method because the file already exists
      AmmoniteScriptSpecBase.withConsoleMatch {
        styled.dst
      } { case (result, stdout, stderr) =>
        result.value shouldBe reflectPathToOsPath(Tmp / "txt" / "greeting.txt")
        stdout shouldBe s""
        stderr shouldBe empty
      }

      (Tmp / "txt" / "greeting.txt").jfile should exist
      (Tmp / "txt" / "greeting.txt").toFile.slurp() shouldBe "Hello world!"
    }

    it("should overwrite a file when requested") {
      // Create a file immediately
      FileMaker(dstPath = Some(Tmp / "txt" / "greeting.txt"), thunk = _.writeAll("Hello world!")).dst
      (Tmp / "txt" / "greeting.txt").jfile should exist
      (Tmp / "txt" / "greeting.txt").toFile.slurp() shouldBe "Hello world!"

      // Create a styled instance from a verbose template with overwrite
      val basic = FileMaker(out = AnsiConsole(verbose = true), overwrite = true)
      val styled = basic.red(Tmp / "txt" / "greeting.txt", "txt") { _.writeAll("Hi overwritten world.") }

      // Try to overwrite it
      (Tmp / "txt").jfile should exist
      AmmoniteScriptSpecBase.withConsoleMatch {
        styled.dst
      } { case (result, stdout, stderr) =>
        result.value shouldBe reflectPathToOsPath(Tmp / "txt" / "greeting.txt")
        stdout shouldBe s"${RED}(txt${RESET}${RED}*${RESET}${RED})${RESET}"
        stderr shouldBe empty
      }

      // It should have been overwritten
      (Tmp / "txt" / "greeting.txt").jfile should exist
      (Tmp / "txt" / "greeting.txt").toFile.slurp() shouldBe "Hi overwritten world."
    }
  }
}
