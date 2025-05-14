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

  val BasicMd: Header = Header.parse("""# A
      !## B
      !Text in A.B
      !### C
      !Text in A.B.C
      !### C2
      !Text in A.B.C2
      !## B2
      !Text in A.B2
      !""".stripMargin('!'))

  val Basic: File = File(Tmp / "basic.md")
  Basic.writeAll(BasicMd.build().toString)

  describe("The basic scenario") {
    it("should read from a file") {
      withGoMatching(TaskCmd, "--query", "A.B.C*", Basic) { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe "Text in A.B.C"
      }
    }

    it("should read from stdin") {
      Using(Basic.inputStream()) { in =>
        Console.withIn(in) {
          withGoMatching(TaskCmd, "--query", "A.B.C*", "-") { case (stdout, stderr) =>
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

  describe("The QueryTask.query method") {

    it("should extract elements from the section") {
      QueryTask.query(".", BasicMd) shouldBe List(BasicMd)
      QueryTask.query(".*", BasicMd) shouldBe BasicMd.mds
      QueryTask.query("*", BasicMd) shouldBe BasicMd.mds

      Header("", 0, QueryTask.query("A*", BasicMd)).build().toString shouldBe
        """B
          |------------------------------------------------------------------------------
          |
          |Text in A.B
          |
          |### C
          |
          |Text in A.B.C
          |
          |### C2
          |
          |Text in A.B.C2
          |
          |B2
          |------------------------------------------------------------------------------
          |
          |Text in A.B2
          |""".stripMargin

      QueryTask.query("A.B.C*", BasicMd) shouldBe List(Paragraph("Text in A.B.C"))
      Header("", 0, QueryTask.query("A.B*", BasicMd)).build().toString shouldBe
        """Text in A.B
          |
          |### C
          |
          |Text in A.B.C
          |
          |### C2
          |
          |Text in A.B.C2
          |""".stripMargin
    }

    it("should return empty on unmatched paths") {
      QueryTask.query("X", BasicMd) shouldBe empty
      QueryTask.query("X*", BasicMd) shouldBe empty
      QueryTask.query("A.X", BasicMd) shouldBe empty
      QueryTask.query("A.B.XX", BasicMd) shouldBe empty
    }

  }
}
