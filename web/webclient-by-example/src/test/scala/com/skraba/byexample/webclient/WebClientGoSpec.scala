package com.skraba.byexample.webclient

import com.skraba.docoptcli.DocoptCliGoSpec

/** Unit tests for [[WebClientGo]] */
class WebClientGoSpec extends DocoptCliGoSpec(WebClientGo) {

  describe(s"${Cli.Cli} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Cli.Doc
    }

    it("throws an exception with an unknown command") {
      val t = intercept[WebClientGo.InternalDocoptException] {
        withGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe WebClientGo.Doc
    }
  }
}
