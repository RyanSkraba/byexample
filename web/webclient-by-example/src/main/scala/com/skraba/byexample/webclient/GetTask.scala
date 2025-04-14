package com.skraba.byexample.webclient

import com.skraba.docoptcli.DocoptCliGo.Task
import sttp.client4.{DefaultSyncBackend, Response, UriContext, quickRequest}

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

  private def pekkoGet(path: String): String = ??? // TODO
  private def sttpGet(path: String): Response[String] = quickRequest.get(uri"$path").send(DefaultSyncBackend())

  def go(opts: TaskOptions): Unit = {
    val uri = opts.getString("URI")
    Seq("--sttp", "--pekko").find(opts.getBoolean) match {
      case Some("--pekko") => pekkoGet(uri)
      case _               => println(sttpGet(uri).body)
    }
  }
}
