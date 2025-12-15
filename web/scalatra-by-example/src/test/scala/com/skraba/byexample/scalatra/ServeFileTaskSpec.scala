package com.skraba.byexample.scalatra

import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}
import sttp.model.StatusCodes

/** Unit tests for [[ServeFileTask]]. */
class ServeFileTaskSpec extends MultiTaskMainSpec(ScalatraGo, Some(ServeFileTask)) with StatusCodes with TmpDir {

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
    Srv.shutdown()
    super.afterAll()
  }

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
  }

  describe(s"${Main.Name} $TaskCmd running as a server") {
    it("should have a health check") {
      val response = Srv.get("/_health")
      response.code shouldBe Ok.code
      response.body shouldBe "true"
    }

    ignore("should redirect on /") {
      val response = Srv.get("/")
      response.code shouldBe Ok.code
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should respond to /index.html") {
      val response = Srv.get("/index.html")
      response.code shouldBe Ok.code
      response.body shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }

    it("should return 404 when a location isn't found") {
      val response = Srv.get("/notfound")
      response.code shouldBe NotFound.code
      response.body shouldBe "Not found"
    }
  }
}
