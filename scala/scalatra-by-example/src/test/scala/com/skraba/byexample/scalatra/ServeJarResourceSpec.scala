package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}
import sttp.model.StatusCodes

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.reflect.io.Streamable
import scala.util.{Success, Try}

/** Unit tests for [[ServeJarResourceTask]]. */
class ServeJarResourceSpec extends DocoptCliGoSpec(ScalatraGo, Some(ServeJarResourceTask)) with StatusCodes {

  /** Run a server in the background. */
  val (server: Future[String], base: String) = {
    val started = "Standalone server started: (.*)\n".r

    val (server, base) = Streamable.closing(new ByteArrayOutputStream()) { out =>
      (
        Future {
          Console.withOut(out) {
            Cli.go(TaskCmd, "--port", "0");
            "Stopped"
          }
        },
        Iterator
          .continually {
            out.flush()
            new String(out.toByteArray, StandardCharsets.UTF_8)
          }
          .collectFirst { case started(uri) => uri }
          .get
      )
    }

    // Wait until the server healthcheck succeeds (or until the server finishes prematurely).
    val start = Future.firstCompletedOf(
      Seq(
        server,
        Future {
          Iterator
            .continually(Try {
              val health = quickRequest.get(uri"$base/_health").send(DefaultSyncBackend())
              health.body shouldBe "true"
            })
            .collectFirst { case Success(_) => "Started" }
            .get
        }
      )
    )
    Await.result(start, 10000.seconds) shouldBe "Started"
    (server, base)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    val shutdown = quickRequest.get(uri"$base/_shutdown").send(DefaultSyncBackend())
    shutdown.code shouldBe Ok
    shutdown.body shouldBe "Goodbye"
    Await.result(server, 10.seconds)
  }

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()
  }

  describe(s"${Cli.Cli} $TaskCmd running as a server") {
    it("should have a health check") {
      val response: Response[String] = quickRequest.get(uri"$base/_health").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    it("should redirect on /") {
      val response: Response[String] = quickRequest.get(uri"$base/").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should respond to /index.html") {
      val response: Response[String] = quickRequest.get(uri"$base/index.html").send(DefaultSyncBackend())
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should return 404 when a location isn't found") {
      val response: Response[String] = quickRequest.get(uri"$base/notfound").send(DefaultSyncBackend())
      response.code shouldBe NotFound
      response.body shouldBe "Not found"
    }
  }
}
