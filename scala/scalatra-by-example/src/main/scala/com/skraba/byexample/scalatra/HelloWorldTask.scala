package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGo.Task
import org.eclipse.jetty.ee10.webapp.WebAppContext
import org.eclipse.jetty.server.Server
import org.scalatra.ScalatraServlet

/** Command-line driver that launches a server that says hello. */
object HelloWorldTask extends Task {

  val Cmd = "helloworld"

  val Description = "Run a server saying Hello World."

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

  /** Internal flag to help turn off the server. */
  private[this] var Running = true

  def go(opts: TaskOptions): Unit = {
    val server = new Server(opts.getInt("PORT", 8080))
    val context = new WebAppContext()
    context.setContextPath("/")
    context.addServlet(classOf[Srvlet], "/*")
    context.setBaseResourceAsString("/")
    server.setHandler(context)
    Running = true
    server.start()
    while (Running) {
      Thread.sleep(1000L)
    }
    server.stop()
  }

  class Srvlet extends ScalatraServlet {
    get("/") { "Hello world" }

    get("/shutdown") {
      Running = false
      "Goodbye"
    }
  }
}
