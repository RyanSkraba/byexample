package com.skraba.byexample.scalatra

import org.eclipse.jetty.http.HttpTester
import org.eclipse.jetty.server.{LocalConnector, Server}
import org.eclipse.jetty.servlet.ServletContextHandler
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[HelloWorldTask.Srvlet]]. */
class HelloWorldTaskSrvletSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  describe(s"Basic scenario using LocalConnector") {

    val Server: Server = new Server
    val Connector: LocalConnector = new LocalConnector(Server)
    Server.addConnector(Connector)
    val context = new ServletContextHandler(Server, "/")
    context.addServlet(classOf[HelloWorldTask.Srvlet], "/hello")
    Server.start()

    it("should respond to a correct request") {
      val request = HttpTester.newRequest
      request.setMethod("GET")
      request.setVersion("HTTP/1.0")
      request.setURI("/hello")

      val response = HttpTester.parseResponse(Connector.getResponse(request.generate))

      response.getStatus shouldBe 200
      response.getContent shouldBe "Hello world"
    }
  }
}
