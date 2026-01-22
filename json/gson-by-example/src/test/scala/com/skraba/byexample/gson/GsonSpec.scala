package com.skraba.byexample.gson

import com.google.gson.JsonParser
import com.skraba.byexample.json.base._

import java.io.InputStreamReader

/** Scala example of using Gson */
class GsonSpec extends JsonSpecBase {

  override def testParseStringIntoJson(): Unit = {
    val json = JsonParser.parseString(JsonTestResources.JsonSimpleString)

    val map = json.getAsJsonObject
    map should have size 3
    map.get("id").getAsInt shouldBe 1
    map.get("name").getAsString shouldBe "one"
    map.get("translations").getAsJsonArray should have size 2
    map.get("translations").getAsJsonArray.get(0).getAsJsonObject should have size 1
    map.get("translations").getAsJsonArray.get(0).getAsJsonObject.get("fr").getAsString shouldBe "un"
    map.get("translations").getAsJsonArray.get(1).getAsJsonObject should have size 1
    map.get("translations").getAsJsonArray.get(1).getAsJsonObject.get("es").getAsString shouldBe "uno"
  }

  override def testParseStreamIntoJson(): Unit = {
    val json = JsonParser.parseReader(new InputStreamReader(JsonTestResources.jsonSimplesStream()))

    val map = json.getAsJsonObject
    map should have size 3
    map.get("id").getAsInt shouldBe 1
    map.get("name").getAsString shouldBe "one"
    map.get("translations").getAsJsonArray should have size 2
    map.get("translations").getAsJsonArray.get(0).getAsJsonObject should have size 1
    map.get("translations").getAsJsonArray.get(0).getAsJsonObject.get("fr").getAsString shouldBe "un"
    map.get("translations").getAsJsonArray.get(1).getAsJsonObject should have size 1
    map.get("translations").getAsJsonArray.get(1).getAsJsonObject.get("es").getAsString shouldBe "uno"
  }
}
