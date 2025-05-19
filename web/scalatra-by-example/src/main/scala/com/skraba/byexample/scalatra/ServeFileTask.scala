package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.tinfoiled.docopt4s.Task
import org.scalatra.servlet.ServletBase

import java.net.URLConnection
import scala.reflect.io.Directory

/** Command-line driver that launches a server that serves resources from the filesystem. */
object ServeFileTask extends Task {

  val Cmd = "servefile"

  val Description = "Run a server that serves resources from a directory."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${ScalatraGo.Name} $Cmd [--port=PORT] [--dir=DIR]
       |
       |Options:
       |  -h --help    Show this screen.
       |  --version    Show version.
       |  --port=PORT  Port (Default: 8080)
       |  --dir=DIR    Directory to serve (Default: .)
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {
    val dir = Directory(opts.getString("--dir"))
    ScalatraGo.runStandaloneServer(opts.getInt("--port", 8080), classOf[Srvlet], dir)
  }

  /** Applies a notFound that falls back to a static file resource.
    * @param sb
    *   The servlet to apply this rule to.
    * @param baseDir
    *   The base directory (absolute or relative to the current directory) to find files, or the current working
    *   directory.
    */
  def fallbackToFileSystem(sb: ServletBase, baseDir: => Option[Directory] = None): Unit = {
    sb.notFound {
      // Only GET is supported
      if (sb.request.getMethod != "GET") sb.halt(404, "Unsupported method")

      // The request prefix is prepended to the path being requested, or use the shortened Task class name, if any
      val requestPath = baseDir
        .orElse(Directory.Current)
        .getOrElse(Directory("."))
        .resolve(sb.request.getRequestURI.substring(sb.request.getServletPath.length + 1))

      requestPath match {
        case p if !p.exists     => sb.halt(404, "Not found")
        case p if p.isDirectory => sb.redirect(sb.request.getRequestURI + "/index.html")(sb.request, sb.response)
        case p if p.isFile =>
          sb.contentType = Option(URLConnection.guessContentTypeFromName(sb.request.getRequestURI))
            .getOrElse("application/octet-stream")
          p.toFile.inputStream()
      }
    }
  }

  class Srvlet extends TestableServlet[Directory] {
    fallbackToFileSystem(this, Some(Cfg))
  }
}
