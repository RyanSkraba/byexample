package com.skraba.byexample.scalatra

import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import org.scalatest.BeforeAndAfterAll
import sttp.model.StatusCodes

/** Unit tests for [[HelloWorldTask]]. */
class HelloWorldTaskSpec
    extends MultiTaskMainSpec(ScalatraGo, Some(HelloWorldTask))
    with BeforeAndAfterAll
    with StatusCodes {

  val Srv = new ScalatraGoServer(Seq(TaskCmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    // TODO: when it exists: itShouldHandleHelpAndVersionFlagsWithArgs()
    itShouldThrowOnUnknownOptKey()
  }

  describe(s"${Main.Name} $TaskCmd running as a server") {
    it("should have a health check") {
      val response = Srv.get("/_health")
      response.code shouldBe Ok.code
      response.body shouldBe "true"
    }

    it("should greet when requested") {
      val response = Srv.get("/")
      response.code shouldBe Ok.code
      response.body shouldBe "Hello world"
    }
  }
}
