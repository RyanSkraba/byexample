package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import org.scalatest.BeforeAndAfterAll
import sttp.model.StatusCodes

/** Unit tests for [[PutTask]]. */
class PutTaskSpec extends MultiTaskMainSpec(WebClientGo, Some(PutTask)) with StatusCodes with BeforeAndAfterAll {

  val Srv = new ScalatraGoServer(Seq(RestTask.Cmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"${Main.Name} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq())
    itShouldThrowOnIncompleteArgs(Seq("--pekko"))
    itShouldThrowOnIncompleteArgs(Seq("--pekko", "https://example.com"))
    itShouldThrowOnIncompleteArgs(Seq("--sttp"))
    itShouldThrowOnIncompleteArgs(Seq("--sttp", "https://example.com"))

    // TODO: Is this incompatible or missing?
    itShouldThrowOnIncompleteArgs(Seq("--pekko", "--sttp"))
    itShouldThrowOnIncompleteArgs(Seq("--pekko", "--sttp", "https://example.com"))

    // TODO: Incompatible, not missing
    itShouldThrowOnIncompleteArgs(Seq("--pekko", "--sttp", "https://example.com", "payload"))
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
