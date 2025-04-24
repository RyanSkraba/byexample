package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.model.StatusCodes

/** Unit tests for [[ServeJarResourceTask]]. */
class ServeJarResourceTaskSpec extends DocoptCliGoSpec(ScalatraGo, Some(ServeJarResourceTask)) with StatusCodes {

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
      response.code shouldBe Ok.code
      response.body shouldBe "true"
    }

    it("should redirect on /") {
      val response = Srv.get("/")
      response.code shouldBe Ok.code
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should respond to /index.html") {
      val response = Srv.get("/index.html")
      response.code shouldBe Ok.code
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should return 404 when a location isn't found") {
      val response = Srv.get("/notfound")
      response.code shouldBe NotFound.code
      response.body shouldBe "Not found"
    }
  }
}
