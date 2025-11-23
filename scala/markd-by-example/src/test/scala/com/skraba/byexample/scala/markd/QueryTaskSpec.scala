package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}
import com.tinfoiled.markd.{Header, Markd, Paragraph}

import scala.reflect.io.{Directory, File}
import scala.util.Using

/** Unit tests for [[QueryTask]] */
class QueryTaskSpec extends MultiTaskMainSpec(MarkdGo, Some(QueryTask)) with TmpDir {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnIncompleteArgs("--query", "..B")
    itShouldThrowOnIncompleteArgs("filename.md")
    itShouldThrowOnIncompleteArgs("-")
    itShouldThrowOnMissingOptValue("filename.md", "--query")
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
      withGoStdout(TaskCmd, "--query", "A.B.C[*]", Basic) shouldBe "Hello ABC"
    }

    it("should provide verbose information") {
      withGoStdout(TaskCmd, "--verbose", "--query", "A.B.C[*]", Basic) shouldBe
        """Query |A.B.C[*] Markd
        |Query A|.B.C[*] Markd
        |Query B|.C[*] Header
        |Query C[*] Header
        |Hello ABC""".stripMargin
    }

    it("should read from stdin") {
      Using(Basic.inputStream()) { in =>
        Console.withIn(in) {
          withGoStdout(TaskCmd, "--query", "A.B.C[*]", "-") shouldBe "Hello ABC"
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
