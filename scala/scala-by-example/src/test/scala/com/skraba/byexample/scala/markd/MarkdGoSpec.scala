package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec

/** Unit tests for [[MarkdGo]] */
class MarkdGoSpec extends DocoptCliGoSpec(MarkdGo) {

  describe(s"${Cli.Cli} docopt check") {
    it("should have less than 80 characters per string for readability.") {
      for (line <- Doc.split("\n")) {
        withClue("main" -> line) {
          line.length should be < 80
        }
      }
      for (
        task <- Cli.Tasks;
        line <- task.Doc.split("\n")
      ) {
        withClue(task.Cmd -> line) {
          line.length should be < 80
        }
      }
    }
  }

  describe(s"${Cli.Cli} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Cli.Doc
    }

    it("throws an exception with an unknown command") {
      val t = intercept[MarkdGo.InternalDocoptException] {
        withGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe MarkdGo.Doc
    }
  }
}
