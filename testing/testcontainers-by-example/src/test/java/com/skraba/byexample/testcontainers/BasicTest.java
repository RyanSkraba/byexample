package com.skraba.byexample.testcontainers;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

/** Basic tests and assertions. */
@Testcontainers
class BasicTest {

  @Container
  @SuppressWarnings("resource")
  public GenericContainer<?> httpd =
      new GenericContainer<>(DockerImageName.parse("httpd:2.4")).withExposedPorts(80);

  @Test
  public void testBasic() throws IOException, URISyntaxException, InterruptedException {
    HttpClient client = HttpClient.newHttpClient();

    HttpRequest request =
        HttpRequest.newBuilder()
            .uri(new URL("http", httpd.getHost(), httpd.getFirstMappedPort(), "/").toURI())
            .header("Accept", "text/html")
            .GET()
            .build();

    HttpResponse<String> httpResponse = client.send(request, HttpResponse.BodyHandlers.ofString());
    assertThat(httpResponse.statusCode()).isEqualTo(200);
    assertThat(httpResponse.body()).isEqualTo("<html><body><h1>It works!</h1></body></html>\n");
  }
}
