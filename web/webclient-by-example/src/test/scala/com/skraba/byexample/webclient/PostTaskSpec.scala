package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{RestTask, ScalatraGoServer}
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import org.scalatest.BeforeAndAfterAll
import sttp.model.StatusCodes

/** Unit tests for [[PostTask]]. */
class PostTaskSpec extends MultiTaskMainSpec(WebClientGo, Some(PostTask)) with StatusCodes with BeforeAndAfterAll {

  val Srv = new ScalatraGoServer(Seq(RestTask.Cmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
    itShouldThrowOnIncompleteArgs("--pekko")
    itShouldThrowOnIncompleteArgs("--pekko", "https://example.com")
    itShouldThrowOnIncompleteArgs("--sttp")
    itShouldThrowOnIncompleteArgs("--sttp", "https://example.com")
    // TODO: Is this incompatible or missing?
    itShouldThrowOnIncompatibleOpts("--pekko", "--sttp")
    itShouldThrowOnIncompatibleOpts("--pekko", "--sttp", "https://example.com")
    // TODO: Incompatible, not missing
    itShouldThrowOnIncompleteArgs("--pekko", "--sttp", "https://example.com", "payload")
  }

  for (cmd <- Seq("--sttp", "--pekko", "")) {
    describe(s"${Main.Name} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
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
