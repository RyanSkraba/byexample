package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec

import scala.reflect.io.{Directory, File}
import scala.util.Using

/** Unit tests for [[QueryTask]] */
class QueryTaskSpec extends DocoptCliGoSpec(MarkdGo, Some(QueryTask)) {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)
  // TODO(rskraba): Tmp should be in the DocoptCliGoSpec

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOpt(Seq("file"))
  }

  val Basic: File = File(Tmp / "basic.md")
  Basic.writeAll("""# A
      !## B
      !Text in A.B
      !### C
      !Text in A.B.C
      !### C2
      !Text in A.B.C2
      !## B2
      !Text in A.B2
      !""".stripMargin('!'))

  describe("The basic scenario") {
    it("should read from a file") {
      withGoMatching(TaskCmd, "--query", "#A/B/C", Basic) { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe "Text in A.B.C"
      }
    }

    it("should read from stdin") {
      Using(Basic.inputStream()) { in =>
        Console.withIn(in) {
          withGoMatching(TaskCmd, "--query", "#A/B/C", "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe "Text in A.B.C"
          }
        }
      }
    }

    it("should fail with un unrecognized query") {
      Using(Basic.inputStream()) { in =>
        Console.withIn(in) {
          withGoMatching(TaskCmd, "--query", "!!A/B/C", "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe "Text in A.B.C"
          }
        }
      }
    }
  }
}
