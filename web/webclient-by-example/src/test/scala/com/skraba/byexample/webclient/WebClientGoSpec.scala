package com.skraba.byexample.webclient

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[WebClientGo]] */
class WebClientGoSpec extends MultiTaskMainSpec(WebClientGo) {

  describe(s"${Main.Name} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Main.Doc
    }

    it("throws an exception with an unknown command") {
      val t = interceptGoDocoptEx("garbage")
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe WebClientGo.Doc
    }
  }
}
