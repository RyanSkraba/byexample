package com.skraba.byexample.webclient

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[WebClientGo]] */
class WebClientGoSpec extends MultiTaskMainSpec(WebClientGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}
