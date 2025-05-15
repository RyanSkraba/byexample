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
      pekko.loglevel = "OFF"
      pekko.stdout-loglevel = "OFF"
      """)

  /** Makes a single HTTP request using the Pekko actor system.
    * @param method
    *   The HTTP method to use
    * @param path
    *   A string representation of the path to request
    * @param entity
    *   The body (if any) to use for the request.
    * @return
    *   A SimpleResponse with the status code and response text.
    */
  private def request(method: HttpMethod, path: String, entity: RequestEntity = HttpEntity.Empty): SimpleResponse = {
    implicit val system: ActorSystem = ActorSystem("my", pekkoNoLogging)
    import system.dispatcher

    val get = Await.result(
      Http()
        .singleRequest(HttpRequest(method = method, uri = new URL(path).toString, entity = entity))
        .flatMap(response => response.entity.toStrict(10.seconds).map(response.status -> _.data.utf8String)),
      Duration.Inf
    )

    val result = SimpleResponse(get._1.intValue(), get._2)
    Await.result(system.terminate(), Duration.Inf)
    result
  }

  /** Make a GET request to the server. */
  override def get(path: String): SimpleResponse = request(HttpMethods.GET, path)

  /** Make a POST request to the server. */
  override def post(path: String, payload: String): SimpleResponse =
    request(HttpMethods.POST, path, HttpEntity(ByteString(payload)))

  /** Make a PUT request to the server. */
  override def put(path: String, payload: String): SimpleResponse =
    request(HttpMethods.PUT, path, HttpEntity(ByteString(payload)))

  /** Make a DELETE request to the server. */
  override def delete(path: String): SimpleResponse = request(HttpMethods.DELETE, path)
}
