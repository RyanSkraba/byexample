package com.skraba.byexample.scalatra

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import org.scalatest.BeforeAndAfterAll
import sttp.model.MediaType.ApplicationXWwwFormUrlencoded
import sttp.model.StatusCodes

/** Unit tests for [[TwirlTask]]. */
class TwirlTaskSpec extends MultiTaskMainSpec(ScalatraGo, Some(TwirlTask)) with StatusCodes with BeforeAndAfterAll {

  val Srv = new ScalatraGoServer(Seq(TaskCmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    // TODO: when it exists: itShouldHandleHelpAndVersionFlags()
    itShouldThrowOnUnknownOptKey()
  }

  describe(s"${Main.Name} $TaskCmd running as a server") {
    it("should have a health check") {
      val response = Srv.get("_health")
      response.code shouldBe Ok.code
      response.body shouldBe "true"
    }

    it("should serve some resources from the jar") {
      Srv.get("css/style.css").code shouldBe Ok.code
    }

    it("should return an HTML page with GET") {
      val response = Srv.get("/")
      response.code shouldBe Ok.code
      response.body should include("<textarea")
      response.body should include("<button type=\"submit\">")
      response.body should include("<form method=\"post\"")
    }

    it("should return the text when POST") {
      val response = Srv.post("/", "userText=HelloWorld", contentType = ApplicationXWwwFormUrlencoded.toString)
      response.code shouldBe Ok.code
      response.body should include("<textarea")
      response.body should include("<button type=\"submit\">")
      response.body should include("<form method=\"post\"")
      response.body should include("You submitted:")
      response.body should include("HelloWorld")
      response.body should include("<p><strong>Character count:</strong> 10</p>")
    }
  }
}
