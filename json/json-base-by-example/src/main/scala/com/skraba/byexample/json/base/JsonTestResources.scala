package com.skraba.byexample.json.base

import com.skraba.byexample.json.base.JsonTestResources._

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

/** Create test resources in the form of the expected "base type" that a JSON SDK expects.
  * @param parse
  *   A function that parses an arbitrary JSON string into the base type.
  * @tparam T
  *   The base type that the JSON SDK returns when parsing a generic String. If there is no base type, you can use Any.
  */
case class JsonTestResources[T](parse: String => T) {
  def JsonObject = parse(JsonSimpleString)
  def JsonBooleanTrue = parse("true")
  def JsonBooleanFalse: T = parse("false")
}

object JsonTestResources {
  val JsonSimpleString: String =
    """{
      |  "id": 1,
      |  "name": "one",
      |  "translations": [
      |    {"fr": "un"},
      |    {"es": "uno"}
      |  ]
      |}
    """.stripMargin

  def jsonSimplesStream(): InputStream = new ByteArrayInputStream(JsonSimpleString.getBytes(StandardCharsets.UTF_8))

  /** YAML is not JSON, but JSON is YAML. */
  val YamlSimpleString: String =
    """
      |id: 1
      |name: one
      |translations:
      |  - fr: un
      |  - es: uno
    """.stripMargin

  def yamlSimplesStream(): InputStream = new ByteArrayInputStream(YamlSimpleString.getBytes(StandardCharsets.UTF_8))

  /** Utility to apply scala conversions from Java on nested collections.
    *
    * @param obj
    *   The java instance to convert. It may contain nested structures of maps and lists.
    * @return
    *   The same structure with Scala maps and lists replacing the Java collections.
    */
  def asScala(obj: Any): Any = {
    obj match {
      case dict: java.util.Map[_, _] => dict.asScala.view.mapValues(asScala).toMap
      case list: java.util.List[_]   => list.asScala.map(asScala).toList
      case other                     => other
    }
  }

  /** Utility to apply scala conversions from Java on nested collections, returning a string-keyed map. */
  def toMap(obj: Any): Map[String, Any] = {
    asScala(obj).asInstanceOf[Map[String, Any]]
  }
}
