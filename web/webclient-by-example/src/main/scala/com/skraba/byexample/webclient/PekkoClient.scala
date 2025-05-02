package com.skraba.byexample.webclient

import com.skraba.byexample.webclient.WebClientGo.SimpleResponse
import com.typesafe.config.ConfigFactory
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.util.ByteString
import org.apache.pekko.actor.ActorSystem

import java.net.URL
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{DurationInt, _}

object PekkoClient extends WebClientGo.SimpleClient {

  /** Ensure the actor system doesn't make a peep. */
  private val pekkoNoLogging = ConfigFactory.parseString("""
      pekko.loglevel = "WARNING"
      pekko.stdout-loglevel = "WARNING"
      """)

  /** Make a GET request to the server. */
  override def get(path: String): SimpleResponse = {
    implicit val system: ActorSystem = ActorSystem("my", pekkoNoLogging)
    import system.dispatcher

    val get = Await.result(
      Http()
        .singleRequest(HttpRequest(method = HttpMethods.GET, uri = new URL(path).toString))
        .flatMap(response => response.entity.toStrict(10.seconds).map(response.status -> _.data.utf8String)),
      Duration.Inf
    )

    val result = SimpleResponse(get._1.intValue(), get._2)
    Await.result(system.terminate(), Duration.Inf)
    result
  }

  /** Make a POST request to the server. */
  override def post(path: String, payload: String): SimpleResponse = {
    implicit val system: ActorSystem = ActorSystem("my", pekkoNoLogging)
    import system.dispatcher

    val get = Await.result(
      Http()
        .singleRequest(HttpRequest(method = HttpMethods.POST, uri = new URL(path).toString, entity = HttpEntity(ByteString(payload))))
        .flatMap(response => response.entity.toStrict(10.seconds).map(response.status -> _.data.utf8String)),
      Duration.Inf
    )

    val result = SimpleResponse(get._1.intValue(), get._2)
    Await.result(system.terminate(), Duration.Inf)
    result
  }

  /** Make a PUT request to the server. */
  override def put(path: String, payload: String): SimpleResponse = ???

  /** Make a DELETE request to the server. */
  override def delete(path: String): SimpleResponse = ???
}
