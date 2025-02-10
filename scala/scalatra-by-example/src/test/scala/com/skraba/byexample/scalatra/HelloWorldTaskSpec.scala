package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec

/** Unit tests for [[HelloWorldTask]]. */
class HelloWorldTaskSpec extends DocoptCliGoSpec(ScalatraGo, Some(HelloWorldTask)) {
  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()
  }

  // TODO: start the service in another thread, make calls, stop it cleanly.
}
