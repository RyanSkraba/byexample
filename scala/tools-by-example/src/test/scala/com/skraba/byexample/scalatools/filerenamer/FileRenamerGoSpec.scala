package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[FileRenamerGo]] */
class FileRenamerGoSpec extends MultiTaskMainSpec(FileRenamerGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}
