package com.skraba.byexample.scala.ammonite.git

import com.skraba.byexample.scala.ammonite.AmmoniteSpec._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET, YELLOW}
import scala.reflect.io.{Directory, Path, Streamable}
import scala.util.Properties

/** Test the git_checker.sc script. */
class GitCheckerSpec
    extends AnyFunSpecLike
    with BeforeAndAfterAll
    with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: Path = Path(
    Paths.get(getClass.getResource("/git_checker.sc").toURI).toFile
  ).parent

  /** Either create a new home directory reused across this suite, or use the
    * common one.
    */
  val HomeFolder: Path =
    if (ReuseAmmoniteHome) ReusableAmmoniteHome
    else Directory.makeTemp(getClass.getSimpleName)

  /** And delete it after the tests. */
  override protected def afterAll(): Unit =
    try {
      if (!ReuseAmmoniteHome) HomeFolder.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  describe("Running the git_checker help") {
    it("should print a useful message") {
      // with helpers
      withAmmoniteMain0AndNoStdIn(
        HomeFolder,
        (ScriptPath / "git_checker.sc").toString,
        "help"
      ) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        stdout should startWith(
          s"$BOLD${GREEN}git_checker.sc$RESET - Do some analysis on git repositories"
        )
        result shouldBe true
      }
    }
  }
}
