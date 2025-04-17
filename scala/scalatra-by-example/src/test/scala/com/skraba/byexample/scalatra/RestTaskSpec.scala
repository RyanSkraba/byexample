package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.model.StatusCodes

/** Unit tests for [[RestTask]]. */
class RestTaskSpec extends DocoptCliGoSpec(ScalatraGo, Some(RestTask)) with StatusCodes {

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
      val response = Srv.get("_health")
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should get all products") {
      val response = Srv.get("product/")
      response.code shouldBe Ok
      response.body shouldBe
        """[
          |  {"id": 1, "name": "one"},
          |  {"id": 2, "name": "two"}
          |]""".stripMargin
    }

    it("should get individual product 1") {
      val response = Srv.get("product/1")
      response.code shouldBe Ok
      response.body shouldBe """{"id": 1, "name": "one"}"""
    }

    it("should get individual product 2") {
      val response = Srv.get("product/2")
      response.code shouldBe Ok
      response.body shouldBe """{"id": 2, "name": "two"}"""
    }

    it("should return 404 when a product isn't found") {
      val response = Srv.get("product/3")
      response.code shouldBe NotFound
      response.body shouldBe "Product 3 not found"
    }
  }

}
