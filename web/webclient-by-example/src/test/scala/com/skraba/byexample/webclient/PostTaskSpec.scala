package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.skraba.docoptcli.DocoptCliGoSpec
import play.api.libs.json.Json
import sttp.model.StatusCodes

/** Unit tests for [[PostTaskSpec]]. */
class PostTaskSpec extends DocoptCliGoSpec(WebClientGo, Some(PostTask)) with StatusCodes {

  val Srv = new ScalatraGoServer(Seq(RestTask.Cmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq())
    itShouldThrowOnMissingOpt(Seq("--pekko"))
    itShouldThrowOnMissingOpt(Seq("--pekko", "https://example.com"))
    itShouldThrowOnMissingOpt(Seq("--sttp"))
    itShouldThrowOnMissingOpt(Seq("--sttp", "https://example.com"))

    // TODO: Is this incompatible or missing?
    itShouldThrowOnMissingOpt(Seq("--pekko", "--sttp"))
    itShouldThrowOnMissingOpt(Seq("--pekko", "--sttp", "https://example.com"))

    // TODO: Incompatible, not missing
    itShouldThrowOnMissingOpt(Seq("--pekko", "--sttp", "https://example.com", "payload"))
  }

  for (cmd <- Seq("--sttp", "--pekko", "")) {
    describe(s"${Cli.Cli} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
      it("should get a URI") {
        val preargs = if (cmd.nonEmpty) Seq(TaskCmd, cmd) else Seq(TaskCmd)
        val postargs = Seq(Srv.base.withWholePath("product/").toString, """{"id": 1, "name": "one"}""")
        withGoMatching(preargs ++ postargs: _*) { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout.toInt shouldBe >(100)
        }
      }
    }
  }

  // TODO: Error handlings
}
