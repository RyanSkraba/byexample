package com.skraba.byexample.json.base

import com.skraba.byexample.json.base.JsonTestResources._

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

/** Create test resources in the form of the expected "base type" that a JSON SDK expects.
  *
  * There are testable data values that correspond to valid Java, as well as invalid Java (especially around numbers).
  *
  * @param parse
  *   A function that parses an arbitrary JSON string into the base type.
  * @tparam T
  *   The base type that the JSON SDK returns when parsing a generic String. If there is no base type, you can use Any.
  */
case class JsonTestResources[T](parse: String => T) {
  def Obj = parse(JsonSimpleString)

  // Booleans
  lazy val BTrue: T = parse(JsonStrings("BTrue"))
  lazy val BFalse: T = parse(JsonStrings("BFalse"))
  lazy val BArr1True: T = parse(JsonStrings("BArr1True"))
  lazy val BArr1False: T = parse(JsonStrings("BArr1False"))
  lazy val BArr: T = parse(JsonStrings("BArr"))
  lazy val BArrTruthy: T = parse(JsonStrings("BArrTruthy"))
  lazy val BArrFalsey: T = parse(JsonStrings("BArrFalsey"))

  // Integers
  lazy val IMin: T = parse(JsonStrings("IMin"))
  lazy val INegOne: T = parse(JsonStrings("INegOne"))
  lazy val IZero: T = parse(JsonStrings("IZero"))
  lazy val IOne: T = parse(JsonStrings("IOne"))
  lazy val IMax: T = parse(JsonStrings("IMax"))
  lazy val IArr1: T = parse(JsonStrings("IArr1"))
  lazy val IArr: T = parse(JsonStrings("IArr"))

  // Longs
  lazy val LMin: T = parse(JsonStrings("LMin"))
  lazy val LMax: T = parse(JsonStrings("LMax"))
  lazy val LArr1: T = parse(JsonStrings("LArr1"))
  lazy val LArr: T = parse(JsonStrings("LArr"))

  // Floats
  lazy val FMinInf: T = parse(JsonStrings("FMinInf"))
  lazy val FMin: T = parse(JsonStrings("FMin"))
  lazy val FNegOne: T = parse(JsonStrings("FNegOne"))
  lazy val FZero: T = parse(JsonStrings("FZero"))
  lazy val FOne: T = parse(JsonStrings("FOne"))
  lazy val FMinPos: T = parse(JsonStrings("FMinPos"))
  lazy val FMax: T = parse(JsonStrings("FMax"))
  lazy val FInf: T = parse(JsonStrings("FInf"))
  lazy val FArr1: T = parse(JsonStrings("FArr1"))
  lazy val FArr: T = parse(JsonStrings("FArr"))

  // Doubles
  lazy val DMinInf: T = parse(JsonStrings("DMinInf"))
  lazy val DMin: T = parse(JsonStrings("DMin"))
  lazy val DMinPos: T = parse(JsonStrings("DMinPos"))
  lazy val DMax: T = parse(JsonStrings("DMax"))
  lazy val DInf: T = parse(JsonStrings("DInf"))
  lazy val DArr1: T = parse(JsonStrings("DArr1"))
  lazy val DArr: T = parse(JsonStrings("DArr"))
}

/** Constants to reuse as strings containing JSON data. */
object JsonTestResources {

  val JsonStrings: Map[String, String] = Map(
    // Booleans
    "BTrue" -> "true",
    "BFalse" -> "false",
    "BArr1True" -> "[true]",
    "BArr1False" -> "[true]",
    "BArr" -> "[true, false]",
    "BArrTruthy" -> "[True, tRue, TRUE, true",
    "BArrFalsey" -> "[False, fAlse, FALSE, false",

    // Integers
    "IMin" -> Int.MinValue.toString,
    "INegOne" -> "-1",
    "IZero" -> "0",
    "IOne" -> "1",
    "IMax" -> Int.MaxValue.toString,
    "IArr1" -> s"[${Int.MaxValue.toString}]",
    "IArr" -> s"[${Int.MinValue.toString}, -1, 0, 1, 12345, ${Int.MaxValue.toString}]",

    // Longs
    "LMin" -> Long.MinValue.toString,
    "LMax" -> Long.MaxValue.toString,
    "LArr1" -> s"[${Long.MaxValue.toString}]",
    "LArr" -> s"[${Long.MinValue.toString}, -1, 0, 1, 12345678910, ${Long.MaxValue.toString}]",

    // Floats
    "FMinInf" -> Float.NegativeInfinity.toString,
    "FMin" -> Float.MinValue.toString,
    "FNegOne" -> "-1.0",
    "FZero" -> "0.0",
    "FOne" -> "1.0",
    "FMinPos" -> Float.MinPositiveValue.toString,
    "FMax" -> Float.MaxValue.toString,
    "FInf" -> Float.PositiveInfinity.toString,
    "FArr1" -> s"[${Float.MaxValue.toString}]",
    "FArr" -> s"[${Float.MinValue.toString}, -1, 0, 1, 12345, ${Float.MaxValue.toString}]",

    // Doubles
    "DMinInf" -> Double.NegativeInfinity.toString,
    "DMin" -> Double.MinValue.toString,
    "DMinPos" -> Double.MinPositiveValue.toString,
    "DMax" -> Double.MaxValue.toString,
    "DInf" -> Double.PositiveInfinity.toString,
    "DArr1" -> s"[${Double.MaxValue.toString}]",
    "DArr" -> s"[${Double.MinValue.toString}, -1, 0, 1, 12345, ${Double.MaxValue.toString}]"
  )

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
