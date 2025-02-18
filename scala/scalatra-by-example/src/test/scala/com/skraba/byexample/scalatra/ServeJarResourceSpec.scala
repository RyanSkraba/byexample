package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import org.eclipse.jetty.http.HttpTester
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}
import sttp.model.StatusCodes

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}

/** Unit tests for [[ServeJarResourceTask]]. */
class ServeJarResourceSpec extends DocoptCliGoSpec(ScalatraGo, Some(ServeJarResourceTask)) with StatusCodes {

  /** Run a server in the background. */
  val Server: Future[(String, String)] = {
    // Launching the server
    val server = Future { withGo(TaskCmd, "--port", "8081") }
    // Wait until the server healthcheck succeeds (or until the server finishes prematurely).
    val start = Future.firstCompletedOf(
      Seq(
        server,
        Future {
          Iterator
            .continually(Try {
              val health = quickRequest.get(uri"http://localhost:8081/_health").send(DefaultSyncBackend())
              health.body shouldBe "true"
            })
            .collectFirst { case Success(_) => "Started" }
            .get
        }
      )
    )
    Await.result(start, 10.seconds) shouldBe "Started"
    server
  }

  override def afterAll(): Unit = {
    super.afterAll()
    val shutdown = quickRequest.get(uri"http://localhost:8081/_shutdown").send(DefaultSyncBackend())
    shutdown.code shouldBe Ok
    shutdown.body shouldBe "Goodbye"
    Await.result(Server, 10.seconds)
  }

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()
  }

  describe(s"${Cli.Cli} $TaskCmd running as a server") {
    it("should have a health check") {
      val response: Response[String] = quickRequest.get(uri"http://localhost:8081/_health").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should redirect on /") {
      val response: Response[String] = quickRequest.get(uri"http://localhost:8081/").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should respond to /index.html") {
      val response: Response[String] =
        quickRequest.get(uri"http://localhost:8081/index.html").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should return 404 when a location isn't found") {
      val response: Response[String] = quickRequest.get(uri"http://localhost:8081/notfound").send(DefaultSyncBackend())
      response.code shouldBe NotFound
      response.body shouldBe "Not found"
    }
  }
}
