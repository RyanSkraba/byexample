package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec
import org.docopt.DocoptExitException

/** Unit tests for [[BeautifyTask]] */
class BeautifyTaskSpec extends DocoptCliGoSpec(MarkdGo, Some(BeautifyTask)) {

  describe(s"${Cli.Cli} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq.empty)
  }

}
