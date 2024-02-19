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
class AmmoniteExampleSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: Path = find("/ammonite_example.sc")

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Either create a new home directory reused across this suite, or use the common one.
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

  /** A standalone helper method for running one specific script (in this case ammonite_example.sc).
    *
    * @param args
    *   The arguments to apply to the ammonite example script
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from the ammonite call
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
          List("--silent", "--home", HomeFolder.toString, ScriptPath.toString) ++ args,
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
        stderr shouldBe empty
        result shouldBe true
        stdout should startWith(first)
      }
    }

    it("using AmmoniteSpec.withAmmoniteMain0AndNoStdIn") {
      // This form allows specifying the home folder and which script to run
      withAmmoniteMain0AndNoStdIn(HomeFolder, ScriptPath.toString, "help") { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout should startWith(first)
      }
    }

    it("building on AmmoniteSpec.withAmmoniteMain0AndNoStdIn") {

      /** Helper to run ammonite_example.sc help successfully with some initial checks
        *
        * @param args
        *   Additional arguments to the script
        * @return
        *   stdout
        */
      def ammoniteHelp(args: String*): String = {
        val arguments: Seq[String] = Seq(ScriptPath.toString, "help") ++ args
        withAmmoniteMain0AndNoStdIn(HomeFolder, arguments: _*) { case (result, stdout, stderr) =>
          stderr shouldBe empty
          result shouldBe true
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
              List("--silent", "--home", HomeFolder.toString, ScriptPath.toString, "help"),
              in,
              Console.out,
              Console.err
            )
          ) { case (result, stdout, stderr) =>
            stderr shouldBe empty
            result shouldBe true
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
                              |  --plain           Don't use ansi colour codes
                              |  --yes             Don't prompt for user confirmation, assume yes
                              |
                              |
                              |""".stripMargin

    /** Helper to run ammonite_example.sc argTest successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def argTest(args: String*): String = {
      val arguments: Seq[String] = Seq(ScriptPath.toString, "argTest") ++ args
      withAmmoniteMain0AndNoStdIn(HomeFolder, arguments: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
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
      withAmmoniteExample("argTest", "--help") { case (result, stdout, stderr) =>
        result shouldBe false
        stderr shouldBe s"""Unknown argument: "--help"\n$ExpectedSignature"""
        stdout shouldBe empty
      }
    }

    it("should fail with too many arguments (Me 'Hello there' VerboseFlag X Invalid)") {
      withAmmoniteExample("argTest", "You", "Hello there", "VerboseFlag", "X", "X", "Invalid") {
        case (result, stdout, stderr) =>
          result shouldBe false
          stderr shouldBe s"""Unknown argument: "Invalid"\n$ExpectedSignature"""
          stdout shouldBe empty
      }
    }
  }

  describe("Running the ammonite_example search and replace") {

    // Hello world scenario
    def scenario(tag: String): Directory = {
      val scenario = (Tmp / tag).createDirectory()
      File(scenario / "a1").writeAll("Hello world!")
      File(scenario / "a2").writeAll("Hello world!\nHello you!")
      File(scenario / "a3").writeAll("Hello you!")
      File(scenario / "a4").writeAll("Greetings world!")
      File(scenario / "b1").writeAll("Goodbye world!")
      scenario
    }

    it("should do a basic replace") {
      val src = scenario("basic")
      withAmmoniteExample("sar", src.toString, "--re", "Hello", "--re", "Hi") { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout shouldBe empty
        File(src / "a1").slurp() shouldBe "Hi world!"
        File(src / "a2").slurp() shouldBe "Hi world!\nHi you!"
        File(src / "a3").slurp() shouldBe "Hi you!"
        File(src / "a4").slurp() shouldBe "Greetings world!"
        File(src / "b1").slurp() shouldBe "Goodbye world!"
      }
    }

    it("should do a basic replace while logging ansi output") {
      val src = scenario("basic_output")
      // For colour codes
      val cfg = ConsoleCfg()
      val X = s"${cfg.Red}X${cfg.Reset}"
      val x = s"${cfg.Green}x${cfg.Reset}"
      withAmmoniteExample("sar", src.toString, "--re", "Hello", "--re", "Hi", "--verbose") {
        case (result, stdout, stderr) =>
          stderr shouldBe empty
          result shouldBe true
          stdout shouldBe
            s"""${cfg.Bold}${cfg.Green}Matching files:${cfg.Reset}
               |  a1
               |  a2
               |  a3
               |  a4
               |  b1
               |Exclude patterns (leaving 5 file to scan):
               |  \btarget\b
               |  ^.git
               |Include patterns:
               |  .*
               |
               |Processing: $X$X$X$x$x
               |
               |${cfg.Bold}Modified 3 files.${cfg.Reset}
               |""".stripMargin
      }
    }

    it("should do a basic replace while logging plain output") {
      val src = scenario("basic_output")
      withAmmoniteExample("sar", src.toString, "--re", "Hello", "--re", "Hi", "--verbose", "--plain") {
        case (result, stdout, stderr) =>
          stderr shouldBe empty
          result shouldBe true
          stdout shouldBe
            """Matching files:
              |  a1
              |  a2
              |  a3
              |  a4
              |  b1
              |Exclude patterns (leaving 5 file to scan):
              |  \btarget\b
              |  ^\.git
              |Include patterns:
              |  .*
              |
              |Processing: XXXxx
              |
              |Modified 3 files.
              |""".stripMargin
      }
    }

    it("should do a basic replace of only included files") {
      val src = scenario("basic_included")
      // Excludes are always processed before includes
      withAmmoniteExample("sar", src.toString, "--include", "a", "--exclude", "a2", "--re", "world", "--re", "all") {
        case (result, stdout, stderr) =>
          stderr shouldBe empty
          result shouldBe true
          stdout shouldBe empty
          File(src / "a1").slurp() shouldBe "Hello all!"
          File(src / "a2").slurp() shouldBe "Hello world!\nHello you!"
          File(src / "a3").slurp() shouldBe "Hello you!"
          File(src / "a4").slurp() shouldBe "Greetings all!"
          File(src / "b1").slurp() shouldBe "Goodbye world!"
      }
    }

  }
}
