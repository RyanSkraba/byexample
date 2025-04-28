package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.skraba.docoptcli.DocoptCliGo.Task

import java.net.URLConnection

/** Command-line driver that launches a server that serves HTML from a Twirl template. */
object TwirlTask extends Task {

  val Cmd = "twirl"

  val Description = "Run a server that serves a Twirl template."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ScalatraGo.Cli} $Cmd [options]
       |
       |Options:
       |  -h --help    Show this screen.
       |  --version    Show version.
       |  --port=PORT  Port (Default: 8080)
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = ScalatraGo.runStandaloneServer(opts.getInt("--port", 8080), classOf[Srvlet])

  /** The object type stored in the fake database. */
  case class Product(id: Int, name: String)

  class Srvlet extends TestableServlet {

    get("/") {
      contentType = "text/html"
      com.skraba.byexample.scalatra.html.index()
    }

    post("/") {
      contentType = "text/html"
      val submittedText = params.getOrElse("userText", "")
      com.skraba.byexample.scalatra.html.index(Some(submittedText))
    }

    notFound {
      if (request.getMethod != "GET") halt(404, "Unsupported method")

      val requestPath = "Twirl" + request.getRequestURI.substring(request.getServletPath.length)

      Option(getClass.getResourceAsStream(requestPath)) match {
        case Some(stream) =>
          contentType =
            Option(URLConnection.guessContentTypeFromName(request.getRequestURI)).getOrElse("application/octet-stream")
          stream
        case None => halt(404, "Not found")
      }
    }
  }
}
