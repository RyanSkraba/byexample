package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import org.scalatest.BeforeAndAfterAll
import sttp.model.StatusCodes

/** Unit tests for [[DeleteTask]]. */
class DeleteTaskSpec extends MultiTaskMainSpec(WebClientGo, Some(DeleteTask)) with StatusCodes with BeforeAndAfterAll {

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
    itShouldThrowOnIncompleteArgs(Seq("--sttp"))

    // TODO: Is this incompatible or missing?
    itShouldThrowOnIncompleteArgs(Seq("--pekko", "--sttp"))

    // TODO: Incompatible, not missing
    itShouldThrowOnIncompleteArgs(Seq("--pekko", "--sttp", "https://example.com"))
  }

  for (cmd <- Seq("--sttp", "--pekko", "")) {
    describe(s"${Main.Name} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
      // Create the product so we can delete it
      val product = SttpClient.post(Srv.base.withWholePath("product/").toString(), """{"id": 9, "name": "nine"}""")
      it("should get a URI") {
        val preargs = if (cmd.nonEmpty) Seq(TaskCmd, cmd) else Seq(TaskCmd)
        val postargs = Seq(Srv.base.withWholePath(s"product/${product.body}"))
        withGoMatching(preargs ++ postargs: _*) { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe empty
        }
      }
    }
  }

  // TODO: Error handlings
}
