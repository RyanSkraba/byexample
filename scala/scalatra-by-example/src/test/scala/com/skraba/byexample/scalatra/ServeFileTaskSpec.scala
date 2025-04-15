package com.skraba.byexample.scalatra

import com.skraba.docoptcli.DocoptCliGoSpec
import org.scalatest.BeforeAndAfterAll
import sttp.model.StatusCodes

import scala.reflect.io.Directory

/** Unit tests for [[ServeFileTask]]. */
class ServeFileTaskSpec extends DocoptCliGoSpec(ScalatraGo, Some(ServeFileTask)) with StatusCodes {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  {
    (Tmp / "index.html").toFile.writeAll("""<html>
        |<body>
        |<h1>Hello world!</h1>
        |</body>
        |</html>
        |""".stripMargin.trim)
  }

  val Srv = new ScalatraGoServer(Seq(TaskCmd, "--dir", Tmp.toString))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }
  }

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()
  }

  describe(s"${Cli.Cli} $TaskCmd running as a server") {
    it("should have a health check") {
      val response = Srv.get("/_health")
      response.code shouldBe Ok
      response.body shouldBe "true"
    }

    ignore("should redirect on /") {
      val response = Srv.get("/")
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should respond to /index.html") {
      val response = Srv.get("/index.html")
      response.code shouldBe Ok
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should return 404 when a location isn't found") {
      val response = Srv.get("/notfound")
      response.code shouldBe NotFound
      response.body shouldBe "Not found"
    }
  }
}
