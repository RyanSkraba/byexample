package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
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

  val BasicTxt: String =
    """# A
      !## B
      !Text in A.B
      !### C
      !Text in A.B.C
      !### C2
      !Text in A.B.C2
      !## B2
      !Text in A.B2
      !""".stripMargin('!')

  val Basic: File = File(Tmp / "basic.md")
  Basic.writeAll(BasicTxt)

  describe("The basic scenario") {
    it("should read from a file") {
      withGoMatching(TaskCmd, "--query", "A.B.C", Basic) { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe "Text in A.B.C"
      }
    }

    it("should read from stdin") {
      Using(Basic.inputStream()) { in =>
        Console.withIn(in) {
          withGoMatching(TaskCmd, "--query", "A.B.C", "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe "Text in A.B.C"
          }
        }
      }.get
    }

    ignore("should fail with un unrecognized query") {
      interceptGo[RuntimeException](TaskCmd, "--query", "A.B.C", Basic).getMessage shouldBe "Unrecognized query: A.B.C"
    }
  }

  describe("More complex queries") {

    /** Shortcut for running the task with STDIN
      * @param query
      *   The specific query to execute
      * @param markdown
      *   The markdown text to read
      * @return
      *   the result of applying the query to the text
      */
    def queryTask(query: String, markdown: String): String =
      Using(new ByteArrayInputStream(markdown.getBytes(StandardCharsets.UTF_8))) { in =>
        Console.withIn(in) {
          withGoMatching(TaskCmd, "--query", query, "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout
          }
        }
      }.get

    it("should read extract entire sections") {
      queryTask("A.B.C", BasicTxt) shouldBe "Text in A.B.C"
      queryTask("A.B.C2", BasicTxt) shouldBe "Text in A.B.C2"
      queryTask("A.B", BasicTxt) shouldBe
        """Text in A.B
          |
          |### C
          |
          |Text in A.B.C
          |
          |### C2
          |
          |Text in A.B.C2
          |""".stripMargin.trim
      queryTask(".A.B2", BasicTxt) shouldBe "Text in A.B2"
    }
  }
}
