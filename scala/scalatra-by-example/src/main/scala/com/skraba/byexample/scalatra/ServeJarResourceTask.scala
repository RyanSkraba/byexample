package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGo.Task
import org.eclipse.jetty.ee10.webapp.WebAppContext
import org.eclipse.jetty.server.Server
import org.scalatra.ScalatraServlet

import java.net.URLConnection

/** Command-line driver that launches a server that serves resources in the JAR. */
object ServeJarResourceTask extends Task {

  val Cmd = "servejar"

  val Description = "Run a server that serves resources embedded in the jar."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ScalatraGo.Cli} $Cmd [--port=PORT]
       |
       |Options:
       |  -h --help    Show this screen.
       |  --version    Show version.
       |  --port=PORT  Port (Default: 8080)
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    val server = new Server(opts.getInt("PORT", 8080))
    val context = new WebAppContext()
    context.setContextPath("/")
    context.addServlet(classOf[Srvlet], "/*")
    server.setHandler(context)
    server.start()
  }

  class Srvlet extends ScalatraServlet {
    get("/") {
      redirect("/index.html")
    }

    notFound {
      if (request.getMethod != "GET") halt(404, "Unsupported method")

      val requestPath = "ServeResourceTask" + request.getRequestURI.substring(request.getServletPath.length)

      Option(getClass.getResourceAsStream(requestPath)) match {
        case Some(stream) =>
          contentType =
            Option(URLConnection.guessContentTypeFromName(request.getRequestURI)).getOrElse("application/octet-stream")
          stream
        case None => halt(404, "File not found")
      }
    }
  }
}
