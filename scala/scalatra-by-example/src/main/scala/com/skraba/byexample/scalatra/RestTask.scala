package com.skraba.byexample.scalatra

import com.skraba.byexample.scalatra.ScalatraGo.TestableServlet
import com.skraba.docoptcli.DocoptCliGo.Task
import play.api.libs.json.{JsArray, Json, OFormat}

import scala.collection.mutable
import scala.util.Try

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

  case class Product(id: Int, name: String)
  private implicit val productFormat: OFormat[Product] = Json.format[Product]

  val db: mutable.Map[Int, Product] = mutable.SortedMap(101 -> Product(1, "one"), 102 -> Product(2, "two"))

  class Srvlet extends TestableServlet {

    before() {
      contentType = "application/json"
    }

    get("/product/") {
      JsArray(db.values.map(Json.toJson(_)).toSeq)
    }

    get("/product/:pid") {
      val pid = params("pid")
      Json.toJson(params("pid").toIntOption.flatMap(db.get).getOrElse(halt(404, s"Product $pid not found")))
    }

    post("/product/") {
      val product = Try { Json.fromJson(Json.parse(request.body)).get }.getOrElse(halt(400))
      val nextPid = db.keys.max + 1
      db += nextPid -> product
      nextPid.toString
    }

    put("/product/:pid") {
      val pid = params("pid")
      pid.toIntOption.flatMap(db.get).getOrElse(halt(404, s"Product $pid not found"))
      val product = Try { Json.fromJson(Json.parse(request.body)).get }.getOrElse(halt(400))
      db += pid.toInt -> product
      pid.toInt.toString
    }
  }
}
