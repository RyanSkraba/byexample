package com.skraba.byexample.webapp.hello;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.eclipse.jetty.http.HttpTester;
import org.eclipse.jetty.server.LocalConnector;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/** Unit tests for {@link HelloServlet}. */
public class HelloServletTest {

  private static LocalConnector connector;

  @BeforeAll
  public static void setUp() throws Exception {
    Server server = new Server();
    connector = new LocalConnector(server);
    server.addConnector(connector);
    ServletContextHandler context = new ServletContextHandler(server, "/");
    context.addServlet(HelloServlet.class, "/hello");
    server.start();
  }

  @Test
  public void testHello() throws Exception {
    HttpTester.Request request = HttpTester.newRequest();
    request.setMethod("GET");
    request.setVersion("HTTP/1.0");
    request.setURI("/hello");

    HttpTester.Response response =
        HttpTester.parseResponse(connector.getResponse(request.generate()));

    // assertTrue(response.getMethod() == null);
    assertThat(response.getStatus(), is(200));
    assertThat(response.getContent().trim(), is("<h1>Hello world!</h1>"));
  }
}
