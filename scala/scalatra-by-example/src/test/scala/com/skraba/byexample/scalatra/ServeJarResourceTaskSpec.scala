package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}
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
      val response: Response[String] = quickRequest.get(uri"${Srv.base}/_health").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should redirect on /") {
      val response: Response[String] = quickRequest.get(uri"${Srv.base}/").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should respond to /index.html") {
      val response: Response[String] = quickRequest.get(uri"${Srv.base}/index.html").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should return 404 when a location isn't found") {
      val response: Response[String] = quickRequest.get(uri"${Srv.base}/notfound").send(DefaultSyncBackend())
      response.code shouldBe NotFound
      response.body shouldBe "Not found"
    }
  }
}
