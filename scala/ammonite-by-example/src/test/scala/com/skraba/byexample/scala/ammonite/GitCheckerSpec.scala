package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteSpec._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.{Directory, Path}

/** Test the git_checker.sc script. */
class GitCheckerSpec
    extends AnyFunSpecLike
    with BeforeAndAfterAll
    with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: Path = AmmoniteSpec.find("/git_checker.sc")

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

    /** Helper to run ammonite_example.sc argTest successfully with some initial
      * checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def help(args: String*): String = {
      val arguments: Seq[String] = Seq(ScriptPath.toString, "help") ++ args
      AmmoniteSpec.withAmmoniteMain0AndNoStdIn(
        HomeFolder,
        arguments: _*
      ) { case (result, stdout, stderr) =>
        result shouldBe true
        stderr shouldBe empty
        stdout
      }
    }

    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(
        s"$BOLD${GREEN}git_checker.sc$RESET - Do some analysis on git repositories"
      )
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "git_checker.sc - Do some analysis on git repositories"
      )
    }
  }
}
