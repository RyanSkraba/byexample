package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import com.tinfoiled.markd.{Header, Markd, Paragraph}

import scala.reflect.io.{Directory, File}
import scala.util.Using

/** Unit tests for [[QueryTask]] */
class QueryTaskSpec extends MultiTaskMainSpec(MarkdGo, Some(QueryTask)) {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)
  // TODO(rskraba): Tmp should be in the DocoptCliGoSpec

  describe(s"${Main.Name} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)
    itShouldThrowOnIncompleteArgs(Seq("file"))
  }

  val BasicMd: Markd = Markd.parse("""# A
      !## B
      !Hello AB
      !### C
      !Hello ABC
      !### C2
      !Hello ABC2
      !## B2
      !Hello AB2
      !""".stripMargin('!'))

  val Basic: File = File(Tmp / "basic.md")
  Basic.writeAll(BasicMd.build().toString)

  describe("The basic scenario") {
    it("should read from a file") {
      withGoMatching(TaskCmd, "--query", "A.B.C[*]", Basic) { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe "Hello ABC"
      }
    }

    it("should read from stdin") {
      Using(Basic.inputStream()) { in =>
        Console.withIn(in) {
          withGoMatching(TaskCmd, "--query", "A.B.C[*]", "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe "Hello ABC"
          }
        }
      }.get
    }

    it("should fail with un unrecognized query") {
      interceptGo[RuntimeException](
        TaskCmd,
        "--query",
        "A.B.C[",
        Basic
      ).getMessage shouldBe "Unrecognized query: A.B.C["
    }
  }
}
