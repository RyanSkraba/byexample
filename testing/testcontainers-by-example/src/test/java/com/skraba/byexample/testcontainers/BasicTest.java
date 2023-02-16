package com.skraba.byexample.testcontainers;

import static io.restassured.RestAssured.when;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItems;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.BindMode;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

/** Basic tests and assertions. */
@Testcontainers
class BasicTest {

  @Container
  @SuppressWarnings("resource")
  private static GenericContainer<?> httpd =
      new GenericContainer<>(DockerImageName.parse("httpd:2.4"))
          .withFileSystemBind(
              System.getProperty("user.dir") + "/src/test/resources/htdocs/",
              "/usr/local/apache2/htdocs/",
              BindMode.READ_ONLY)
          .withExposedPorts(80);

  @BeforeAll
  static void setup() {
    RestAssured.baseURI = getHttpdUri().toString();
  }

  private static URI getHttpdUri() throws RuntimeException {
    try {
      return new URL("http", httpd.getHost(), httpd.getFirstMappedPort(), "/").toURI();
    } catch (URISyntaxException | MalformedURLException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  void testBasic() throws IOException, InterruptedException {
    HttpClient client = HttpClient.newHttpClient();
    HttpRequest request =
        HttpRequest.newBuilder().uri(getHttpdUri()).header("Accept", "text/html").GET().build();

    HttpResponse<String> httpResponse = client.send(request, HttpResponse.BodyHandlers.ofString());
    assertThat(httpResponse.statusCode()).isEqualTo(200);
    assertThat(httpResponse.body()).isEqualTo("<html><body><h1>It works!</h1></body></html>\n");
  }

  @Test
  public void testBasicJson() {
    when()
        .get("/lotto/{id}.json", 5)
        .then()
        .statusCode(200)
        .body("lotto.lottoId", equalTo(5), "lotto.winners.winnerId", hasItems(23, 54));
  }

  @Test
  public void testBasicJsonBreakdown() {
    // Get the response of the GET
    Response r = when().get("/lotto/{id}.json", 5);

    // Use JSON assertions on the response
    r.then()
        .statusCode(200)
        .body("lotto.lottoId", equalTo(5), "lotto.winners.winnerId", hasItems(23, 54));

    // But you can also get the response as a String (for example)
    String body = r.asString();
    assertThat(body).contains("\"lottoId\":5");
  }
}
