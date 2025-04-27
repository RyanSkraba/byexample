package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import play.api.libs.json.Json
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
      response.code shouldBe Ok.code
      response.body shouldBe "true"
    }
  }

  describe("When getting using the GET method") {
    it("should return all products") {
      val response = Srv.get("product/")
      response.code shouldBe Ok.code
      Json.parse(response.body) shouldBe Json.parse(
        """[
          |  {"id": 1, "name": "one"},
          |  {"id": 2, "name": "two"}
          |]""".stripMargin
      )
    }

    it("should get individual product 101") {
      val response = Srv.get("product/101")
      response.code shouldBe Ok.code
      Json.parse(response.body) shouldBe Json.parse("""{"id": 1, "name": "one"}""")
    }

    it("should get individual product 102") {
      val response = Srv.get("product/102")
      response.code shouldBe Ok.code
      Json.parse(response.body) shouldBe Json.parse("""{"id": 2, "name": "two"}""")
    }

    it("should return 404 when a product isn't found") {
      val response = Srv.get("product/999")
      response.code shouldBe NotFound.code
      response.body shouldBe "Product 999 not found"
    }

    it("should return 404 when an invalid product is requested") {
      val response = Srv.get("product/abc")
      response.code shouldBe NotFound.code
      response.body shouldBe "Product abc not found"
    }
  }

  describe("When creating using the POST method") {

    it("should create a product") {
      val response = Srv.post("product/", """{"id": 3, "name": "three"}""")
      response.code shouldBe Ok.code
      response.body shouldBe "103"
    }

    it("when invalid JSON is sent") {
      val response = Srv.post("product/", """{{{{""")
      response.code shouldBe BadRequest.code
      response.body shouldBe ""
      // TODO: Better errors?
    }

    it("when json is missing an attribute") {
      val response = Srv.post("product/", """{"id": 3}""")
      response.code shouldBe BadRequest.code
      response.body shouldBe ""
      // TODO: Better errors?
    }
  }

  describe("When updating using the PUT method") {
    it("should update a product using put") {
      val response = Srv.put("product/102", """{"id": 2, "name": "deux"}""")
      response.code shouldBe Ok.code
      response.body shouldBe "102"
      Json.parse(Srv.get("product/102").body) shouldBe Json.parse("""{"id": 2, "name": "deux"}""")
    }

    it("when invalid JSON is sent") {
      val response = Srv.put("product/102", """{{{{""")
      response.code shouldBe BadRequest.code
      response.body shouldBe ""
      // TODO: Better errors?
      Json.parse(Srv.get("product/102").body) shouldBe Json.parse("""{"id": 2, "name": "deux"}""")
    }

    it("when json is missing an attribute") {
      val response = Srv.put("product/102", """{"id": 3}""")
      response.code shouldBe BadRequest.code
      response.body shouldBe ""
      // TODO: Better errors?
      Json.parse(Srv.get("product/102").body) shouldBe Json.parse("""{"id": 2, "name": "deux"}""")
    }

    it("should return 404 when the product isn't found") {
      val response = Srv.put("product/999", """{"id": 4, "name": "quatre"}""")
      response.code shouldBe NotFound.code
      response.body shouldBe "Product 999 not found"
    }

    it("should return 404 when an invalid product is requested") {
      val response = Srv.put("product/abc", """{"id": 4, "name": "quatre"}""")
      response.code shouldBe NotFound.code
      response.body shouldBe "Product abc not found"
    }
  }

  describe("When deleting using the DELETE method") {
    it("should delete a product") {
      val response = Srv.delete("product/102")
      response.code shouldBe NoContent.code
      response.body shouldBe ""
    }

    it("should return 404 when the product isn't found") {
      val response = Srv.delete("product/999")
      response.code shouldBe NotFound.code
      response.body shouldBe "Product 999 not found"
    }

    it("should return 404 when an invalid product is requested") {
      val response = Srv.delete("product/abc")
      response.code shouldBe NotFound.code
      response.body shouldBe "Product abc not found"
    }
  }
}
