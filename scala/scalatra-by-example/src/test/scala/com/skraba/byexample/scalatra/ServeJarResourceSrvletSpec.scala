package com.skraba.byexample.scalatra

import org.eclipse.jetty.ee10.servlet.ServletContextHandler
import org.eclipse.jetty.http.HttpTester
import org.eclipse.jetty.server.{LocalConnector, Server}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[ServeJarResourceTask.Srvlet]]. */
class ServeJarResourceSrvletSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  describe(s"Basic scenario using LocalConnector") {

    val server = new Server
    val connector = new LocalConnector(server)
    server.addConnector(connector)
    val context = new ServletContextHandler("/")
    context.addServlet(classOf[ServeJarResourceTask.Srvlet], "/srvjar/*")
    server.setHandler(context)
    server.start()

    it("should redirect on a directory") {
      val request = HttpTester.newRequest
      request.setMethod("GET")
      request.setVersion("HTTP/1.0")
      request.setURI("/srvjar")

      val response = HttpTester.parseResponse(connector.getResponse(request.generate))
      response.getStatus shouldBe 302
      response.get("Location") shouldBe "http://localhost/index.html"
    }

    it("should respond to a correct request") {
      val request = HttpTester.newRequest
      request.setMethod("GET")
      request.setVersion("HTTP/1.0")
      request.setURI("/srvjar/index.html")

      val response = HttpTester.parseResponse(connector.getResponse(request.generate))
      response.getStatus shouldBe 200
      response.getContent shouldBe
        """<html>
          |<body>
          |<h1>Hello world!</h1>
          |</body>
          |</html>""".stripMargin
    }
  }
}
