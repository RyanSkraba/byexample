package com.skraba.byexample.scala.markd
import com.tinfoiled.docopt4s.DocoptException
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[MarkdGo]] */
class MarkdGoSpec extends MultiTaskMainSpec(MarkdGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}
