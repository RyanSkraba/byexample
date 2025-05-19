package com.skraba.byexample.webclient

import com.tinfoiled.docopt4s.Task

/** Command-line driver that puts to a URI. */
object PutTask extends Task {

  val Cmd = "put"

  val Description = "Make an HTTP PUT request."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${WebClientGo.Name} $Cmd [--sttp|--pekko] URI PAYLOAD
       |
       |Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  --sttp     Make the request with the STTP library (the default).
       |  --pekko    Make the request with the Pekko library.
       |  URI        The URI to PUT.
       |  PAYLOAD    The payload for the PUT request.
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    val uri = opts.getString("URI")
    val payload = opts.getString("PAYLOAD")
    Seq("--sttp", "--pekko").find(opts.getBoolean) match {
      case Some("--pekko") => print(PekkoClient.put(uri, payload).body)
      case _               => print(SttpClient.put(uri, payload).body)
    }
  }
}
