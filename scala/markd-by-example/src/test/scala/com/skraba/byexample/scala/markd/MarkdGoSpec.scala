package com.skraba.byexample.scala.markd
import com.tinfoiled.docopt4s.DocoptException
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[MarkdGo]] */
class MarkdGoSpec extends MultiTaskMainSpec(MarkdGo) {

  describe(s"${Main.Name} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Doc
    }

    it("throws an exception with an unknown command") {
      val t = intercept[DocoptException] {
        withGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe MarkdGo.Doc
    }
  }
}
