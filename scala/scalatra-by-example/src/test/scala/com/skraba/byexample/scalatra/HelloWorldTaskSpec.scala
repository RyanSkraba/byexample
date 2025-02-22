package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.model.StatusCodes

/** Unit tests for [[HelloWorldTask]]. */
class HelloWorldTaskSpec extends DocoptCliGoSpec(ScalatraGo, Some(HelloWorldTask)) with StatusCodes {

  val Srv = new ScalatraGoServer(Seq(TaskCmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()
  }

  describe(s"${Cli.Cli} $TaskCmd running as a server") {
    it("should have a health check") {
      val response = Srv.get("/_health")
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should greet when requested") {
      val response = Srv.get("/")
      response.code shouldBe Ok
      response.body shouldBe "Hello world"
    }
  }
}
