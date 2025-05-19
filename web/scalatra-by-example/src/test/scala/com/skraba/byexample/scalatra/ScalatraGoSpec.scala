package com.skraba.byexample.scalatra

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[ScalatraGo]] */
class ScalatraGoSpec extends MultiTaskMainSpec(ScalatraGo) {

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
      t.docopt shouldBe ScalatraGo.Doc
    }
  }
}
