package com.skraba.byexample.webclient

import com.skraba.docoptcli.DocoptCliGo.Task

/** Command-line driver that gets a URI. */
object DeleteTask extends Task {

  val Cmd = "delete"

  val Description = "Make an HTTP DELETE request."

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
       |  URI        The URI to DELETE.
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    val uri = opts.getString("URI")
    Seq("--sttp", "--pekko").find(opts.getBoolean) match {
      case Some("--pekko") => print(PekkoClient.delete(uri).body)
      case _               => print(SttpClient.delete(uri).body)
    }
  }
}
