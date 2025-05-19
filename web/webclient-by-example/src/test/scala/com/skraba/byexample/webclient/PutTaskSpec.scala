package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import sttp.model.StatusCodes

/** Unit tests for [[PutTask]]. */
class PutTaskSpec extends MultiTaskMainSpec(WebClientGo, Some(PutTask)) with StatusCodes {

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
    describe(s"${Main.Name} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
      it("should get a URI") {
        val preargs = if (cmd.nonEmpty) Seq(TaskCmd, cmd) else Seq(TaskCmd)
        val postargs = Seq(Srv.base.withWholePath("product/101").toString, """{"id": 1, "name": "uno"}""")
        withGoMatching(preargs ++ postargs: _*) { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout.toInt shouldBe 101
        }
      }
    }
  }

  // TODO: Error handlings
}
