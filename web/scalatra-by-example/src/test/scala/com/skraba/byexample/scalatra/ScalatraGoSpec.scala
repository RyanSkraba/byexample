package com.skraba.byexample.scalatra

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[ScalatraGo]] */
class ScalatraGoSpec extends MultiTaskMainSpec(ScalatraGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}
