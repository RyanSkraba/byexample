package com.skraba.byexample.json.gson;

import static org.assertj.core.api.Assertions.assertThat;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.skraba.byexample.json.base.JsonTestBase;
import com.skraba.byexample.json.base.JsonTestResources$;
import java.io.InputStreamReader;
import org.junit.jupiter.api.Test;

/** Unit tests for */
class GsonTest implements JsonTestBase {

  @Test
  public void testParseStringIntoJson() {
    JsonElement json = JsonParser.parseString(JsonTestResources$.MODULE$.JsonSimpleString());
    JsonObject map = json.getAsJsonObject();

    assertThat(map.size()).isEqualTo(3);
    assertThat(map.get("id").getAsInt()).isEqualTo(1);
    assertThat(map.get("name").getAsString()).isEqualTo("one");
    assertThat(map.get("translations").getAsJsonArray())
        .hasSize(2)
        .extracting(JsonElement::getAsJsonObject)
        .satisfiesExactly(
            obj -> assertThat(obj.get("fr")).isEqualTo(new JsonPrimitive("un")),
            obj -> assertThat(obj.get("es")).isEqualTo(new JsonPrimitive("uno")));
  }

  @Test
  public void testParseStreamIntoJson() {
    JsonElement json =
        JsonParser.parseReader(
            new InputStreamReader(JsonTestResources$.MODULE$.jsonSimplesStream()));
    JsonObject map = json.getAsJsonObject();

    assertThat(map.size()).isEqualTo(3);
    // etc. etc.
  }

  @Test
  public void testCanCreateGenericJsonObjects() {
    JsonObject map = new JsonObject();
    map.addProperty("id", 1);
    map.addProperty("name", "one");
    JsonArray translations = new JsonArray();
    map.add("translations", translations);
    JsonObject fr = new JsonObject();
    fr.addProperty("fr", "un");
    JsonObject es = new JsonObject();
    es.addProperty("es", "uno");
    translations.add(fr);
    translations.add(es);

    assertThat(map.size()).isEqualTo(3);

    Gson gson = new GsonBuilder().create();
    assertThat(gson.toJson(map))
        .isEqualTo(
            "{'id':1,'name':'one','translations':[{'fr':'un'},{'es':'uno'}]}".replace('\'', '"'));
  }
}
