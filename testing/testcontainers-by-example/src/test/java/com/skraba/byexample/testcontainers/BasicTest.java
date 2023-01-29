package com.skraba.byexample.testcontainers;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasItems;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
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
  private GenericContainer<?> httpd =
      new GenericContainer<>(DockerImageName.parse("httpd:2.4"))
          .withFileSystemBind(
              System.getProperty("user.dir") + "/src/test/resources/htdocs/",
              "/usr/local/apache2/htdocs/",
              BindMode.READ_ONLY)
          .withExposedPorts(80);

  private URI getHttdUri() throws IOException, URISyntaxException {
    return new URL("http", httpd.getHost(), httpd.getFirstMappedPort(), "/").toURI();
  }

  @Test
  void testBasic() throws IOException, URISyntaxException, InterruptedException {
    HttpClient client = HttpClient.newHttpClient();
    HttpRequest request =
        HttpRequest.newBuilder().uri(getHttdUri()).header("Accept", "text/html").GET().build();

    HttpResponse<String> httpResponse = client.send(request, HttpResponse.BodyHandlers.ofString());
    assertThat(httpResponse.statusCode()).isEqualTo(200);
    assertThat(httpResponse.body()).isEqualTo("<html><body><h1>It works!</h1></body></html>\n");
  }

  @Test
  public void testJson() throws IOException, URISyntaxException {
    given()
        .baseUri(getHttdUri().toString())
        .when()
        .get("/lotto/{id}.json", 5)
        .then()
        .statusCode(200)
        .body("lotto.lottoId", equalTo(5), "lotto.winners.winnerId", hasItems(23, 54));
  }
}
