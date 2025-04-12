package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.skraba.docoptcli.DocoptCliGo.Task

import java.net.URLConnection
import scala.reflect.io.{Directory, Path}

/** Command-line driver that launches a server that serves resources from the filesystem. */
object ServeFileTask extends Task {

  val Cmd = "servefile"

  val Description = "Run a server that serves resources from a directory."

  private var dir: Directory = Directory(".")

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ScalatraGo.Cli} $Cmd [--port=PORT] [--dir=DIR]
       |
       |Options:
       |  -h --help    Show this screen.
       |  --version    Show version.
       |  --port=PORT  Port (Default: 8080)
       |  --dir=DIR    Directory to serve (Default: .)
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    dir = Directory(opts.getString("--dir"))
    ScalatraGo.runStandaloneServer(opts.getInt("--port", 8080), classOf[Srvlet])
  }

  class Srvlet extends TestableServlet {
    notFound {
      if (request.getMethod != "GET") halt(404, "Unsupported method")

      val requestPath = dir + request.getRequestURI.substring(request.getServletPath.length)

      Path(requestPath) match {
        case p if !p.exists     => halt(404, "Not found")
        case p if p.isDirectory => redirect(request.getRequestURI + "/index.html")
        case p if p.isFile =>
          contentType =
            Option(URLConnection.guessContentTypeFromName(request.getRequestURI)).getOrElse("application/octet-stream")
          p.toFile.inputStream()
      }
    }
  }
}
