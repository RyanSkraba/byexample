package com.skraba.byexample.scalatools.gitchecker

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec

/** Unit tests for [[GitCheckerGo]] */
class GitCheckerGoSpec extends MultiTaskMainSpec(GitCheckerGo) {
  describe(s"Standard $MainName command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnMissingTaskCommand("--debug")
    itShouldThrowOnUnknownTaskCommand()
  }
}
