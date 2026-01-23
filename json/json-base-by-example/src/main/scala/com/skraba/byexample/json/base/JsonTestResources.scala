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
  def Obj = parse(JsonSimpleString)

  // Booleans
  lazy val BTrue: T = parse("true")
  lazy val BFalse: T = parse("false")
  lazy val BArr1True: T = parse("[true]")
  lazy val BArr1False: T = parse("[true]")
  lazy val BArr: T = parse("[true, false]")
  lazy val BArrTruthy: T = parse("[True, tRue, TRUE, true")
  lazy val BArrFalsey: T = parse("[False, fAlse, FALSE, false")

  // Integers
  lazy val IMin: T = parse(Int.MinValue.toString)
  lazy val INegOne: T = parse("-1")
  lazy val IZero: T = parse("0")
  lazy val IOne: T = parse("1")
  lazy val IMax: T = parse(Int.MaxValue.toString)
  lazy val IArr1: T = parse(s"[${Int.MaxValue.toString}]")
  lazy val IArr: T = parse(s"[${Int.MinValue.toString}, -1, 0, 1, 12345, ${Int.MaxValue.toString}]")

  // Longs
  lazy val LMin: T = parse(Long.MinValue.toString)
  lazy val LMax: T = parse(Long.MaxValue.toString)
  lazy val LArr1: T = parse(s"[${Long.MaxValue.toString}]")
  lazy val LArr: T = parse(s"[${Long.MinValue.toString}, -1, 0, 1, 12345678910, ${Long.MaxValue.toString}]")

  // Floats
  lazy val FMinInf: T = parse(Float.NegativeInfinity.toString)
  lazy val FMin: T = parse(Float.MinValue.toString)
  lazy val FNegOne: T = parse("-1.0")
  lazy val FZero: T = parse("0.0")
  lazy val FOne: T = parse("1.0")
  lazy val FMinPos: T = parse(Float.MinPositiveValue.toString)
  lazy val FMax: T = parse(Float.MaxValue.toString)
  lazy val FInf: T = parse(Float.PositiveInfinity.toString)
  lazy val FArr1: T = parse(s"[${Float.MaxValue.toString}]")
  lazy val FArr: T = parse(s"[${Float.MinValue.toString}, -1, 0, 1, 12345, ${Float.MaxValue.toString}]")

  // Doubles
  lazy val DMinInf: T = parse(Double.NegativeInfinity.toString)
  lazy val DMin: T = parse(Double.MinValue.toString)
  lazy val DMinPos: T = parse(Double.MinPositiveValue.toString)
  lazy val DMax: T = parse(Double.MaxValue.toString)
  lazy val DInf: T = parse(Double.PositiveInfinity.toString)
  lazy val DArr1: T = parse(s"[${Double.MaxValue.toString}]")
  lazy val DArr: T = parse(s"[${Double.MinValue.toString}, -1, 0, 1, 12345, ${Double.MaxValue.toString}]")
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
