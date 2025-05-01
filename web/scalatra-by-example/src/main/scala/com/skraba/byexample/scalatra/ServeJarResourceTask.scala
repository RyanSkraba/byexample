package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.skraba.docoptcli.DocoptCliGo.Task
import org.scalatra.servlet.ServletBase

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

  def go(opts: TaskOptions): Unit = ScalatraGo.runStandaloneServer(opts.getInt("--port", 8080), classOf[Srvlet])

  /** Applies a notFound that falls back to resource embedded in a jar
    * @param sb
    *   The servlet to apply this rule to.
    * @param rsrcPrefix
    *   A prefix to apply to the request path before fetching it as a string. If this doesn't start with "/" then the
    *   context of the servlet's package is used.
    */
  def fallbackToJar(sb: ServletBase, rsrcPrefix: Option[String] = None): Unit = {
    sb.notFound {
      // Only GET is supported
      if (sb.request.getMethod != "GET") sb.halt(404, "Unsupported method")

      // The request prefix is prepended to the path being requested, or use the shortened Task class name, if any
      val requestPath = rsrcPrefix.getOrElse("/" + sb.getClass.getName.split("(Task)?\\$").head.replace('.', '/')) +
        sb.request.getRequestURI.substring(sb.request.getServletPath.length)

      Option(sb.getClass.getResourceAsStream(requestPath)) match {
        case Some(stream) =>
          sb.contentType = Option(URLConnection.guessContentTypeFromName(sb.request.getRequestURI))
            .getOrElse("application/octet-stream")
          stream
        case None => sb.halt(404, "Not found")
      }
    }
  }

  class Srvlet extends TestableServlet {
    get("/") { redirect("/index.html") }

    fallbackToJar(this)
  }
}
