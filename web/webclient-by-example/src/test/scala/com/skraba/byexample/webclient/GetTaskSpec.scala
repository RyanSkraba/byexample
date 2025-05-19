package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import play.api.libs.json.Json
import sttp.model.StatusCodes

/** Unit tests for [[GetTask]]. */
class GetTaskSpec extends MultiTaskMainSpec(WebClientGo, Some(GetTask)) with StatusCodes {

  val Srv = new ScalatraGoServer(Seq(RestTask.Cmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"${Main.Name} $TaskCmd command line") {
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
    describe(s"${Main.Name} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
      it("should get a URI") {
        val preargs = if (cmd.nonEmpty) Seq(TaskCmd, cmd) else Seq(TaskCmd)
        val postargs = Seq(Srv.base.withWholePath("product/101"))
        withGoMatching(preargs ++ postargs: _*) { case (stdout, stderr) =>
          stderr shouldBe empty
          Json.parse(stdout) shouldBe Json.parse("""{"id": 1, "name": "one"}""")
        }
      }
    }
  }

  // TODO: Error handlings
}
