package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}
import sttp.model.StatusCodes

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Success, Try}

/** Unit tests for [[HelloWorldTask]]. */
class HelloWorldTaskSpec extends DocoptCliGoSpec(ScalatraGo, Some(HelloWorldTask)) with StatusCodes {

  /** Run a server in the background. */
  val Server: Future[(String, String)] = {
    // Launching the server
    val server = Future { withGo(TaskCmd) }
    // Wait until the server healthcheck succeeds (or until the server finishes prematurely).
    val start = Future.firstCompletedOf(
      Seq(
        server,
        Future {
          Iterator
            .continually(Try {
              val health = quickRequest.get(uri"http://localhost:8080/_health").send(DefaultSyncBackend())
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
    val shutdown = quickRequest.get(uri"http://localhost:8080/_shutdown").send(DefaultSyncBackend())
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
      val response: Response[String] = quickRequest.get(uri"http://localhost:8080/_health").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should greet when requested") {
      val response: Response[String] = quickRequest.get(uri"http://localhost:8080/").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe "Hello world"
    }
  }
}
