package com.skraba.byexample.json.gson;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.skraba.byexample.json.base.JsonTestBase;
import com.skraba.byexample.json.base.JsonTestResources;
import com.skraba.byexample.json.base.JsonTestResources$;
import java.io.InputStreamReader;
import org.junit.jupiter.api.Test;

/** Unit tests for GSON */
class GsonTest implements JsonTestBase {

  private final JsonTestResources$ JSON_RESOURCES = JsonTestResources$.MODULE$;
  private final JsonTestResources<JsonElement> JSON =
      new JsonTestResources<>(JsonParser::parseString);

  @Test
  public void testParseStringIntoJson() {

    // Get the JSON object
    JsonElement json = JsonParser.parseString(JSON_RESOURCES.JsonSimpleString());

    // JsonElement is the parent type
    assertThat(json.isJsonArray()).isFalse();
    assertThat(json.isJsonObject()).isTrue();
    assertThat(json.isJsonPrimitive()).isFalse();
    assertThat(json.isJsonNull()).isFalse();

    // Each of those has a get that only succeeds if it is the correct type
    JsonObject obj = json.getAsJsonObject();
    assertThatThrownBy(json::getAsJsonArray)
        .isInstanceOf(IllegalStateException.class)
        .hasMessageStartingWith("Not a JSON Array");

    // Accessing fields in a JsonObject as a map
    assertThat(obj.asMap())
        .hasSize(3)
        .containsEntry("id", new JsonPrimitive(1))
        .containsEntry("name", new JsonPrimitive("one"))
        .hasEntrySatisfying(
            "translations", arr -> assertThat(arr.getAsJsonArray().asList()).hasSize(2));

    // But we can also get the types
    // This works if it's the right type or an array containing a single element of the right type
    assertThat(JSON.BTrue().getAsBoolean()).isTrue();
    assertThat(JSON.BArr1True().getAsBoolean()).isTrue();

    assertThat(JSON.IOne().getAsNumber())
        .isNotEqualTo(1)
        .satisfies(n -> assertThat(n.intValue()).isEqualTo(1));
    assertThat(JSON.IOne().getAsDouble()).isOne();
    assertThat(JSON.IOne().getAsFloat()).isOne();
    assertThat(JSON.IOne().getAsLong()).isOne();
    assertThat(JSON.IOne().getAsInt()).isOne();
    assertThat(JSON.IOne().getAsByte()).isOne();
    assertThat(JSON.IOne().getAsBigDecimal()).isOne();
    assertThat(JSON.IOne().getAsBigInteger()).isOne();
    assertThat(JSON.IOne().getAsShort()).isOne();

    // assertThat(....getAsString).isEqualTo(xxx);
    // assertThat(....getAsCharacter).isEqualTo(xxx);
    // assertThat(....toString).isEqualTo(xxx);

    assertThat(obj.size()).isEqualTo(3);

    // Primitives are all JsonPrimitive
    assertThat(obj.get("id")).isEqualTo(new JsonPrimitive(1));
    assertThat(obj.get("id").getAsInt()).isEqualTo(1);
    assertThat(obj.get("name")).isEqualTo(new JsonPrimitive("one"));
    assertThat(obj.get("name").getAsString()).isEqualTo("one");

    assertThat(obj.get("translations").getAsJsonArray())
        .hasSize(2)
        .extracting(JsonElement::getAsJsonObject)
        .satisfiesExactly(
            x -> assertThat(x.get("fr")).isEqualTo(new JsonPrimitive("un")),
            x -> assertThat(x.get("es")).isEqualTo(new JsonPrimitive("uno")));
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
