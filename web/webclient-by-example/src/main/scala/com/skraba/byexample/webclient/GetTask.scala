package com.skraba.byexample.webclient

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Command-line driver that gets a URI. */
object GetTask extends Task {

  val Cmd = "get"

  val Description = "Make an HTTP GET request."

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
       |  URI        The URI to GET.
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {
    val uri = opt.string.get("URI")
    Seq("--sttp", "--pekko").find(opt.flag) match {
      case Some("--pekko") => print(PekkoClient.get(uri).body)
      case _               => print(SttpClient.get(uri).body)
    }
  }
}
