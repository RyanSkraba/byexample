package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.SimpleResponse
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import sttp.client4.{DefaultSyncBackend, UriContext, quickRequest}
import sttp.model.{StatusCodes, Uri}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.reflect.io.Streamable
import scala.util.{Success, Try}

/** A reusable helper for stopping and starting a server for unit testing. */
class ScalatraGoServer(args: Seq[String], timeout: Duration = 10.seconds) extends StatusCodes {

  private[this] val started = "Standalone server started: (.*)\n".r

  /** Run a server in the background, capturing the base URI. */
  val (server: Future[String], base: Uri) = Streamable.closing(new ByteArrayOutputStream()) { out =>
    (
      Future {
        Console.withOut(out) {
          ScalatraGo.go(args ++ Seq("--port", "0"): _*)
          "Stopped"
        }
      },
      Iterator
        .continually {
          out.flush()
          new String(out.toByteArray, StandardCharsets.UTF_8)
        }
        .collectFirst { case started(uri) => uri"$uri" }
        .get
    )
  }

  // Wait until the server healthcheck succeeds (or until the server finishes prematurely).
  Await.result(
    Future.firstCompletedOf(
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
    ),
    timeout
  ) shouldBe "Started"

  /** Make a GET request to the server. */
  def get(path: String): SimpleResponse = {
    val r = quickRequest.get(base.withWholePath(path)).send(DefaultSyncBackend())
    SimpleResponse(r.code.code, r.body)
  }

  /** Make a POST request to the server. */
  def post(path: String, payload: String): SimpleResponse = {
    val r = quickRequest.post(base.withWholePath(path)).body(payload).send(DefaultSyncBackend())
    SimpleResponse(r.code.code, r.body)
  }

  /** Make a PUT request to the server. */
  def put(path: String, payload: String): SimpleResponse = {
    val r = quickRequest.put(base.withWholePath(path)).body(payload).send(DefaultSyncBackend())
    SimpleResponse(r.code.code, r.body)
  }

  /** Make a DELETE request to the server. */
  def delete(path: String): SimpleResponse = {
    val r = quickRequest.delete(base.withWholePath(path)).send(DefaultSyncBackend())
    SimpleResponse(r.code.code, r.body)
  }

  /** Request the server be shut down. */
  def shutdown(): Unit = {
    val shutdown = quickRequest.get(uri"$base/_shutdown").send(DefaultSyncBackend())
    shutdown.code shouldBe Ok
    shutdown.body shouldBe "Goodbye"
    Await.result(server, 10.seconds)
  }
}
