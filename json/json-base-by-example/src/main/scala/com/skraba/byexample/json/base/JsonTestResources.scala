package com.skraba.byexample.json.base

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import scala.jdk.CollectionConverters._

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
