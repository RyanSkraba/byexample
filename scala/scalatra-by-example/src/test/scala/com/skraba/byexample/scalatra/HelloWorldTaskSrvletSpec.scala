package com.skraba.byexample.scalatra

import org.eclipse.jetty.ee10.servlet.ServletContextHandler
import org.eclipse.jetty.http.HttpTester
import org.eclipse.jetty.server.{LocalConnector, Server}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[HelloWorldTask.Srvlet]]. */
class HelloWorldTaskSrvletSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  describe(s"Basic scenario using LocalConnector") {

    val server = new Server
    val connector = new LocalConnector(server)
    server.addConnector(connector)
    val context = new ServletContextHandler("/")
    context.addServlet(classOf[HelloWorldTask.Srvlet], "/hello")
    server.setHandler(context)
    server.start()


    it("should respond to a correct request") {
      val request = HttpTester.newRequest
      request.setMethod("GET")
      request.setVersion("HTTP/1.0")
      request.setURI("/hello")

      val response = HttpTester.parseResponse(connector.getResponse(request.generate))

      response.getStatus shouldBe 200
      response.getContent shouldBe "Hello world"
    }
  }
}
