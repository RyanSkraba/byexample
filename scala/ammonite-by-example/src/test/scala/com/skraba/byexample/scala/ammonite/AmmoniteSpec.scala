package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET, YELLOW}
import scala.reflect.io._
import scala.util.Properties

/** Testing ammonite scripts can be a bit tricky!
  */
class AmmoniteSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: Path = find("/ammonite_example.sc")

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Either create a new home directory reused across this suite, or use the
    * common one.
    */
  val HomeFolder: Path =
    if (ReuseAmmoniteHome) ReusableAmmoniteHome
    else Tmp / "ammonite.home"

  /** And delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try {
      Tmp.deleteRecursively()
      if (!ReuseAmmoniteHome && HomeFolder.exists)
        HomeFolder.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  /** A standalone helper method for running one specific script (in this case
    * ammonite_example.sc).
    *
    * @param args
    *   The arguments to apply to the ammonite example script
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from
    *   the ammonite call
    * @tparam U
    *   If any, the type of output of the partial function
    * @return
    *   The output of the partial function
    */
  def withAmmoniteExample[U](args: String*)(
      pf: scala.PartialFunction[(Boolean, String, String), U]
  ): U = Streamable.closing(new ByteArrayInputStream(Array.empty[Byte])) { in =>
    Console.withIn(in) {
      withConsoleMatch(
        ammonite.AmmoniteMain.main0(
          List(
            "--silent",
            "--home",
            HomeFolder.toString,
            ScriptPath.toString
          ) ++ args,
          in,
          Console.out,
          Console.err
        )
      )(pf)
    }
  }

  describe("Demonstrating testing ammonite_example help in different ways") {

    val first =
      s"$BOLD${GREEN}ammonite_example.sc$RESET - Demonstrate how to script with Ammonite\n"

    it("using the short-form, script-specific helper") {
      withAmmoniteExample("help") { case (result, stdout, stderr) =>
        result shouldBe true
        stderr shouldBe empty
        stdout should startWith(first)
      }
    }

    it("using AmmoniteSpec.withAmmoniteMain0AndNoStdIn") {
      // This form allows specifying the home folder and which script to run
      withAmmoniteMain0AndNoStdIn(
        HomeFolder,
        ScriptPath.toString,
        "help"
      ) { case (result, stdout, stderr) =>
        result shouldBe true
        stderr shouldBe empty
        stdout should startWith(first)
      }
    }

    it("building on AmmoniteSpec.withAmmoniteMain0AndNoStdIn") {

      /** Helper to run ammonite_example.sc help successfully with some initial
        * checks
        *
        * @param args
        *   Additional arguments to the script
        * @return
        *   stdout
        */
      def ammoniteHelp(args: String*): String = {
        val arguments: Seq[String] = Seq(ScriptPath.toString, "help") ++ args
        withAmmoniteMain0AndNoStdIn(
          HomeFolder,
          arguments: _*
        ) { case (result, stdout, stderr) =>
          result shouldBe true
          stderr shouldBe empty
          stdout
        }
      }

      // Using the custom, short form
      ammoniteHelp("--verbose") should startWith(first)
    }

    it("using the long form") {
      // This form only uses withConsoleMatch to manage the console streams and
      // no additional helpers.
      val stdIn = new ByteArrayInputStream(Array.empty[Byte])
      Streamable.closing(stdIn) { in =>
        Console.withIn(in) {
          withConsoleMatch(
            ammonite.AmmoniteMain.main0(
              List(
                "--silent",
                "--home",
                HomeFolder.toString,
                ScriptPath.toString,
                "help"
              ),
              in,
              Console.out,
              Console.err
            )
          ) { case (result, stdout, stderr) =>
            result shouldBe true
            stderr shouldBe empty
            stdout should startWith(first)
          }
        }
      }
    }
  }

  describe("Running the ammonite_example argTest") {

    val ExpectedSignature = """Expected Signature: argTest
                              |  --user <str>      The user running the script, or current user if not present
                              |  --greeting <str>  A string value
                              |  --verbose         Verbose for extra output
                              |
                              |
                              |""".stripMargin

    /** Helper to run ammonite_example.sc argTest successfully with some initial
      * checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def argTest(args: String*): String = {
      val arguments: Seq[String] = Seq(ScriptPath.toString, "argTest") ++ args
      withAmmoniteMain0AndNoStdIn(
        HomeFolder,
        arguments: _*
      ) { case (result, stdout, stderr) =>
        result shouldBe true
        stderr shouldBe empty
        stdout
      }
    }

    it("should print hello without any arguments") {
      val stdout = argTest()
      stdout shouldBe s"${YELLOW}Hello, $BOLD${Properties.userName}$RESET\n"
      stdout should not include s"\nThe --verbose flag was set!\n"
    }

    it("with one argument (Me)") {
      val stdout = argTest("Me")
      stdout shouldBe s"${YELLOW}Hello, ${BOLD}Me$RESET\n"
      stdout should not include s"\nThe --verbose flag was set!\n"
    }

    it("with one argument and the --verbose flag") {
      val stdout = argTest("--verbose", "Me")
      stdout should startWith(s"${YELLOW}Hello, ${BOLD}Me$RESET")
      stdout should include(s"\nThe --verbose flag was set!\n")
    }

    it("with two arguments (Me Hey)") {
      val stdout = argTest("Me", "Hey")
      stdout shouldBe s"${YELLOW}Hey, ${BOLD}Me$RESET\n"
      stdout should not include s"\nThe --verbose flag was set!\n"
    }

    it("with three arguments (You 'Hello there' VerboseFlag)") {
      val stdout = argTest("You", "Hello there", "VerboseFlag")
      stdout should startWith(s"${YELLOW}Hello there, ${BOLD}You$RESET\n")
      stdout should include(s"\nThe --verbose flag was set!\n")
    }

    it("with a named argument (--greeting Yo)") {
      val stdout = argTest("--greeting", "Yo")
      stdout shouldBe s"${YELLOW}Yo, $BOLD${Properties.userName}$RESET\n"
      stdout should not include s"\nThe --verbose flag was set!\n"
    }

    it("should fail with an unknown flag (--help)") {
      // We can't use the helper for an error condition
      withAmmoniteExample("argTest", "--help") {
        case (result, stdout, stderr) =>
          result shouldBe false
          stderr shouldBe s"""Unknown argument: "--help"\n$ExpectedSignature"""
          stdout shouldBe empty
      }
    }

    it(
      "should fail with too many arguments (Me 'Hello there' VerboseFlag Invalid)"
    ) {
      withAmmoniteExample(
        "argTest",
        "You",
        "Hello there",
        "VerboseFlag",
        "Invalid"
      ) { case (result, stdout, stderr) =>
        result shouldBe false
        stderr shouldBe s"""Unknown argument: "Invalid"\n$ExpectedSignature"""
        stdout shouldBe empty
      }
    }
  }

  describe("Running the ammonite_example search and replace") {

    // Hello world scenario
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "greet").writeAll("Hello world!")

    it("should do a basic replace") {

      withAmmoniteExample(
        "sar",
        Basic.toString(),
        "--re",
        "Hello",
        "--re",
        "Hi"
      ) { case (result, stdout, stderr) =>
        result shouldBe true
        stderr shouldBe empty
        stdout shouldBe empty

        File(Basic / "greet").slurp() shouldBe "Hi world!"
      }
    }
  }
}
