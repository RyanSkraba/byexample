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
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should get all products") {
      val response = Srv.get("product/")
      response.code shouldBe Ok
      Json.parse(response.body) shouldBe Json.parse(
        """[
          |  {"id": 1, "name": "one"},
          |  {"id": 2, "name": "two"}
          |]""".stripMargin
      )
    }

    it("should get individual product 101") {
      val response = Srv.get("product/101")
      response.code shouldBe Ok
      Json.parse(response.body) shouldBe Json.parse("""{"id": 1, "name": "one"}""")
    }

    it("should get individual product 102") {
      val response = Srv.get("product/102")
      response.code shouldBe Ok
      Json.parse(response.body) shouldBe Json.parse("""{"id": 2, "name": "two"}""")
    }

    describe("should return 404") {
      it("when a product isn't found") {
        val response = Srv.get("product/103")
        response.code shouldBe NotFound
        response.body shouldBe "Product 103 not found"
      }
      it("when an invalid product id is sent") {
        val response = Srv.get("product/three")
        response.code shouldBe NotFound
        response.body shouldBe "Product three not found"
      }
    }

    it("should create a product using post") {
      val response = Srv.post("product/", """{"id": 3, "name": "three"}""")
      response.code shouldBe Ok
      response.body shouldBe "103"
    }

    describe("should return 400") {
      // TODO: What about conflicting ids?
      it("when invalid JSON is sent") {
        val response = Srv.post("product/", """{{{{""")
        response.code shouldBe BadRequest
        response.body shouldBe ""
        // TODO: Better errors?
      }
      it("when json is missing an attribute") {
        val response = Srv.post("product/", """{"id": 3}""")
        response.code shouldBe BadRequest
        response.body shouldBe ""
        // TODO: Better errors?
      }
      it("when json is missing another attribute") {
        val response = Srv.post("product/", """{"name": "four"}""")
        response.code shouldBe BadRequest
        response.body shouldBe ""
        // TODO: should this actually work with an assigned id?
      }
    }

    it("should update a product using put") {
      val response = Srv.put("product/102", """{"id": 2, "name": "deux"}""")
      response.code shouldBe Ok
      response.body shouldBe "102"
      Json.parse(Srv.get("product/102").body) shouldBe Json.parse("""{"id": 2, "name": "deux"}""")
    }

    describe("should return 404 when updating a product") {
      it("isn't found") {
        val response = Srv.put("product/104", """{"id": 4, "name": "quatre"}""")
        response.code shouldBe NotFound
        response.body shouldBe "Product 104 not found"
      }
    }

    it("should delete a product") {
      val response = Srv.delete("product/102")
      response.code shouldBe NoContent
      response.body shouldBe ""
    }

    describe("should return 404 when deleting a product") {
      it("isn't found") {
        val response = Srv.delete("product/104")
        response.code shouldBe NotFound
        response.body shouldBe "Product 104 not found"
      }
    }

  }
}
