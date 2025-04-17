package com.skraba.byexample.webclient

import com.skraba.docoptcli.DocoptCliGo.Task
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}

import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration.DurationInt

/** Command-line driver that gets a URI. */
object GetTask extends Task {

  val Cmd = "get"

  val Description = "Run a server saying Hello World."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${WebClientGo.Cli} $Cmd [--sttp|--pekko] URI
       |
       |Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  --sttp     Make the request with the STTP library (the default).
       |  --pekko    Make the request with the Pekko library.
       |  URI        The URI to GET
       |""".stripMargin.trim

  private def pekkoGet(path: String): String = {
    import org.apache.pekko.actor.typed.ActorSystem
    import org.apache.pekko.actor.typed.scaladsl.Behaviors
    import org.apache.pekko.http.scaladsl.Http
    import org.apache.pekko.http.scaladsl.model._
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
    body
  }

  private def sttpGet(path: String): Response[String] = quickRequest.get(uri"$path").send(DefaultSyncBackend())

  def go(opts: TaskOptions): Unit = {
    val uri = opts.getString("URI")
    Seq("--sttp", "--pekko").find(opts.getBoolean) match {
      case Some("--pekko") => print(pekkoGet(uri))
      case _               => print(sttpGet(uri).body)
    }
  }
}
