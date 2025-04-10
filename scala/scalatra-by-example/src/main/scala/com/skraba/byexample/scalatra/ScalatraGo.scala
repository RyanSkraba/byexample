package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task
import jakarta.servlet.Servlet
import org.eclipse.jetty.ee10.webapp.WebAppContext
import org.eclipse.jetty.server.Server
import org.scalatra.ScalatraServlet

/** A driver for running a Scalatra web server.
  */
object ScalatraGo extends DocoptCliGo {
  override lazy val Cli: String = "ScalatraGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(HelloWorldTask, ServeJarResourceTask, ServeFileTask)
  override lazy val Doc: String = "A driver to launch a web server.\n\n" + SimpleDoc

  /** Internal flag to help turn off any running standalone servers. */
  private[this] var Running = true

  /** Runs a standalone servlet server that serves the single servlet.
    * @param port
    *   The port to run the HTTP server on
    * @param srvlet
    *   The class implementing the servlet
    */
  def runStandaloneServer(port: Int, srvlet: Class[_ <: Servlet]): Unit = {
    val server = new Server(port)
    val context = new WebAppContext()
    context.setContextPath("/")
    context.addServlet(srvlet, "/*")
    context.setBaseResourceAsString("/")
    server.setHandler(context)
    Running = true
    server.start()
    if (port == 0) println(s"Standalone server started: ${server.getURI}")
    while (Running) Thread.sleep(1000L)
    server.stop()
  }

  /** A servlet that can shut itself down for testability. */
  class TestableServlet extends ScalatraServlet {
    get("/_health") { true }

    get("/_shutdown") {
      Running = false
      "Goodbye"
    }
  }
}
