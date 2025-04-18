package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.skraba.docoptcli.DocoptCliGo.Task

import scala.collection.mutable

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

  case class Product(id: Int, name: String) {
    // TODO: JSON binding maybe
    lazy val json: String = s"""{"id": $id, "name": "$name"}"""
  }

  val db: mutable.Map[Int, Product] = mutable.SortedMap(1 -> Product(1, "one"), 2 -> Product(2, "two"))

  class Srvlet extends TestableServlet {

    before() {
      contentType = "application/json"
    }

    get("/product/") {
      db.values.map("  " + _.json).mkString("[\n", ",\n", "\n]")
    }

    get("/product/:id") {
      val paramId = params("id")
      val id = paramId.toIntOption.getOrElse(halt(404, s"Product $paramId not found"))
      db.getOrElse(id, halt(404, s"Product $paramId not found")).json
    }
  }
}
