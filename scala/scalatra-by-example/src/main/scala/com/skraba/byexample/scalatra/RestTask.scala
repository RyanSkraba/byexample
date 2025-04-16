package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.skraba.docoptcli.DocoptCliGo.Task

/** Command-line driver that launches a server that has a basic REST API. */
object RestTask extends Task {

  val Cmd = "rest"

  val Description = "Run a server with a basic REST API."

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

  class Srvlet extends TestableServlet {

    before() {
      contentType = "application/json"
    }

    // TODO: Do a better in-memory DB
    // TODO: JSON binding maybe

    get("/product/") {
      """[
        |  {"id": 1, "name": "one"},
        |  {"id": 2, "name": "two"}
        |]""".stripMargin
    }

    get("/product/:id") {
      params("id") match {
        case "1"   => """{"id": 1, "name": "one"}"""
        case "2"   => """{"id": 2, "name": "two"}"""
        case other => halt(404, s"Product $other not found")
      }
    }
  }
}
