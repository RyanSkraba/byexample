package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec

/** Unit tests for [[ScalatraGo]] */
class ScalatraGoSpec extends DocoptCliGoSpec(ScalatraGo) {

  describe(s"${Cli.Cli} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Cli.Doc
    }

    it("throws an exception with an unknown command") {
      val t = intercept[ScalatraGo.InternalDocoptException] {
        withGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe ScalatraGo.Doc
    }
  }
}
