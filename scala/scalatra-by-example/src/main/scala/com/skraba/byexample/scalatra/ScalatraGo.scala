package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task
import jakarta.servlet.Servlet
import org.eclipse.jetty.ee10.webapp.WebAppContext
import org.eclipse.jetty.server.Server
import org.scalatra.ScalatraServlet

import java.util.concurrent.atomic.AtomicBoolean

/** A driver for running a Scalatra web server.
  */
object ScalatraGo extends DocoptCliGo {
  override lazy val Cli: String = "ScalatraGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(HelloWorldTask, RestTask, ServeJarResourceTask, ServeFileTask)
  override lazy val Doc: String = "A driver to launch a web server.\n\n" + SimpleDoc

  case class SimpleResponse(code: Int, body: String)

  trait SimpleClient {

    /** Make a GET request to the server. */
    def get(path: String): SimpleResponse

    /** Make a POST request to the server. */
    def post(path: String, payload: String): SimpleResponse

    /** Make a PUT request to the server. */
    def put(path: String, payload: String): SimpleResponse

    /** Make a DELLETE request to the server. */
    def delete(path: String): SimpleResponse
  }

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
    val running = new AtomicBoolean(true)
    context.setAttribute("__running", running)
    server.setHandler(context)
    server.start()
    if (port == 0) println(s"Standalone server started: ${server.getURI}")
    while (running.get()) Thread.sleep(1000L)
    server.stop()
  }

  /** A servlet that can shut itself down for testability. */
  class TestableServlet extends ScalatraServlet {
    get("/_health") { true }

    get("/_shutdown") {
      getServletContext.getAttribute("__running") match {
        case running: AtomicBoolean =>
          running.set(false)
          "Goodbye"
        case other => halt(404, s"Shutdown failed ($other).")
      }
    }
  }
}
