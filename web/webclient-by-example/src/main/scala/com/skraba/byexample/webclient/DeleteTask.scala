package com.skraba.byexample.webclient

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Command-line driver that gets a URI. */
object DeleteTask extends Task {

  val Cmd = "delete"

  val Description = "Make an HTTP DELETE request."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${WebClientGo.Name} $Cmd [--sttp|--pekko] URI
       |
       |Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  --sttp     Make the request with the STTP library (the default).
       |  --pekko    Make the request with the Pekko library.
       |  URI        The URI to DELETE.
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {
    val uri = opt.string.get("URI")
    Seq("--sttp", "--pekko").find(opt.flag) match {
      case Some("--pekko") => print(PekkoClient.delete(uri).body)
      case _               => print(SttpClient.delete(uri).body)
    }
  }
}
