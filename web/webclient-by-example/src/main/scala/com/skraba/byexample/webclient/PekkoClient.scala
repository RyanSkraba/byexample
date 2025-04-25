package com.skraba.byexample.webclient

import com.skraba.byexample.webclient.WebClientGo.SimpleResponse
import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model._

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt
import scala.concurrent.Await

object PekkoClient extends WebClientGo.SimpleClient {

  /** Make a GET request to the server. */
  override def get(path: String): SimpleResponse = {
    implicit val system: ActorSystem[Any] = ActorSystem(Behaviors.empty, "my-system")
    implicit val ec: ExecutionContextExecutor = system.executionContext

    val body = Await.result(
      Http()
        .singleRequest(HttpRequest(uri = path))
        .flatMap(response => response.entity.toStrict(10.seconds))
        .map(entity => entity.data.utf8String),
      10.seconds
    )

    system.terminate()
    SimpleResponse(0, body)
  }

  /** Make a POST request to the server. */
  override def post(path: String, payload: String): SimpleResponse = ???

  /** Make a PUT request to the server. */
  override def put(path: String, payload: String): SimpleResponse = ???

  /** Make a DELETE request to the server. */
  override def delete(path: String): SimpleResponse = ???
}
