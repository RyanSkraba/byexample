package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.skraba.docoptcli.DocoptCliGoSpec
import play.api.libs.json.Json
import sttp.model.StatusCodes

/** Unit tests for [[GetTaskSpec]]. */
class GetTaskSpec extends DocoptCliGoSpec(WebClientGo, Some(GetTask)) with StatusCodes {

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
    itShouldThrowOnMissingOpt(Seq("--sttp"))

    // TODO: Is this incompatible or missing?
    itShouldThrowOnMissingOpt(Seq("--pekko", "--sttp"))

    // TODO: Incompatible, not missing
    itShouldThrowOnMissingOpt(Seq("--pekko", "--sttp", "https://example.com"))
  }

  for (cmd <- Seq("--sttp", "--pekko", "")) {
    describe(s"${Cli.Cli} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
      it("should get a URI") {
        val args = if (cmd.nonEmpty) Seq(GetTask.Cmd, cmd) else Seq(GetTask.Cmd)
        withGoMatching(args :+ Srv.base.withWholePath("product/101"): _*) { case (stdout, stderr) =>
          stderr shouldBe empty
          Json.parse(stdout) shouldBe Json.parse("""{"id": 1, "name": "one"}""")
        }
      }
    }
  }

  // TODO: Error handlings
}
