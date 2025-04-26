package com.skraba.byexample.webclient

import com.skraba.byexample.webclient.WebClientGo.SimpleResponse
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.util.ByteString

import java.net.URL
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration.{DurationInt, _}

object PekkoClient extends WebClientGo.SimpleClient {

  /** Make a GET request to the server. */
  override def get(path: String): SimpleResponse = {
    import org.apache.pekko.actor.typed.ActorSystem
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
  override def post(path: String, payload: String): SimpleResponse = {
    import org.apache.pekko.actor.ActorSystem

    val url = new URL(path)

    implicit val system: ActorSystem = ActorSystem()
    import system.dispatcher

    val ct = ContentType.parse("application/vnd.schemaregistry.v1+json").toOption.get
    val entity = HttpEntity(ct, ByteString(payload))
    val request = HttpRequest(method = HttpMethods.POST, uri = url.toString, entity = entity)

    val responseFuture: Future[HttpResponse] = Http().singleRequest(request)
    val response = Await.result(responseFuture, Duration.Inf)

    val bodyFuture: Future[String] = response.entity.dataBytes.runFold(ByteString.empty)(_ ++ _).map(_.utf8String)
    val responseBody = Await.result(bodyFuture, Duration.Inf)

    val result = SimpleResponse(response.status.intValue(), responseBody)
    Await.result(system.terminate(), Duration.Inf)
    result
  }

  /** Make a PUT request to the server. */
  override def put(path: String, payload: String): SimpleResponse = ???

  /** Make a DELETE request to the server. */
  override def delete(path: String): SimpleResponse = ???
}
