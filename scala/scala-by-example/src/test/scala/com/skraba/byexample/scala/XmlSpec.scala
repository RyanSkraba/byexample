package com.skraba.byexample.scala

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.xml._
import scala.xml.transform.{RewriteRule, RuleTransformer}

/** Using [[XML]] in Scala.
  *
  * @see
  *   [[https://github.com/scala/scala-xml/wiki]]
  */
class XmlSpec extends AnyFunSpecLike with Matchers {

  val InkNs: String = "https://www.inkscape.org/namespaces/inkscape"

  val ExampleSvg: Elem = XML.loadString(s"""<svg xmlns:inkscape="$InkNs" width="200" height="200">
    |  <g style="display:inline" inkscape:groupmode="layer" inkscape:label="Layer1">
    |    <rect x="10" y="10" width="180" height="180" fill="blue" />
    |  </g>
    |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="Layer2">
    |    <line x1="0" y1="0" x2="200" y2="200" stroke="red" />
    |    <circle cx="100" cy="100" r="50" fill="green" />
    |  </g>
    |</svg>""".stripMargin)

  describe("Using the XPath-like selectors") {

    it("Should find direct children nodes with '\\'") {
      val groups: NodeSeq = ExampleSvg \ "g"
      groups should have size 2
      groups.head.label shouldBe "g"
      groups.head.attribute("style") shouldBe Some(Text("display:inline"))
      groups.head.attribute(InkNs, "label") shouldBe Some(Text("Layer1"))
      groups.head.child should have size 3 // including newlines and spaces
      groups.head.child(1).label shouldBe "rect"
      groups(1).label shouldBe "g"
      groups(1).attribute("style") shouldBe Some(Text("display:none"))
      groups(1).attribute(InkNs, "label") shouldBe Some(Text("Layer2"))
      groups(1).child should have size 5 // including newlines and spaces

      // But there isn't any top level rectangle
      (ExampleSvg \ "rect") shouldBe empty
    }

    it("Should find deep children nodes with '\\\\'") {
      val groups: NodeSeq = ExampleSvg \\ "g"
      groups should have size 2
      groups.head.label shouldBe "g"
      groups.head.attribute("style") shouldBe Some(Text("display:inline"))
      groups.head.attribute(InkNs, "label") shouldBe Some(Text("Layer1"))
      groups.head.child should have size 3 // including newlines and spaces
      groups.head.child(1).label shouldBe "rect"
      groups(1).label shouldBe "g"
      groups(1).attribute("style") shouldBe Some(Text("display:none"))
      groups(1).attribute(InkNs, "label") shouldBe Some(Text("Layer2"))
      groups(1).child should have size 5 // including newlines and spaces

      // It finds this
      (ExampleSvg \\ "rect") should have size 1
    }

    it("Should search all elements for an attribute") {
      val radius = (ExampleSvg \\ "_").filter(node => (node \ "@r").toString == "50")
      radius should have size 1
      radius.head.label shouldBe "circle"

      val filled = (ExampleSvg \\ "_").filter(_.attribute("fill").isDefined)
      filled should have size 2
      filled.map(_.label) shouldBe Seq("rect", "circle")

      val fills = (ExampleSvg \\ "_").flatMap(_.attribute("fill")).map(_.text).distinct
      fills shouldBe Seq("blue", "green")
    }

    it("Should find attributes") {
      (ExampleSvg \\ "rect" \ "@width") shouldBe Group(Text("180"))
      (ExampleSvg \\ "rect" \@ "width") shouldBe "180"
    }
  }

  private def isLayer(n: Node): Boolean =
    n.label == "g" && (n \@ s"{$InkNs}groupmode") == "layer"

  private def isTargetLayer(name: String)(n: Node): Boolean =
    isLayer(n) && (n \@ s"{$InkNs}label") == name

  private def setLayerVisibility(elem: Elem, visible: Boolean): Elem = {
    val displayStyle = if (visible) "display:inline" else "display:none"
    elem.copy(attributes = elem.attributes.append(Attribute(null, "style", displayStyle, Null)))
  }

  private def findLayers(in: Node): Seq[Node] = {
    in match {
      case elem: Elem if isLayer(elem) => Seq(elem) ++ elem.child.flatMap(findLayers)
      case elem: Elem                  => elem.child.flatMap(findLayers)
      case other                       => Seq.empty
    }
  }

  private def transformLayerVisilityManual(in: Node, name: String, visible: Boolean = true): Node = {
    in match {
      case elem: Elem if isTargetLayer(name)(elem) => setLayerVisibility(elem, visible)
      case elem: Elem => elem.copy(child = elem.child.map(transformLayerVisilityManual(_, name, visible)))
      case other      => other
    }
  }

  private def transformLayerVisilityRule(in: Node, name: String, visible: Boolean = true): Node = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if isTargetLayer(name)(elem) => setLayerVisibility(elem, visible)
        case other                                   => other
      }
    }
    new RuleTransformer(rule).transform(in).head
  }

  private def removeLayerRule(in: Node, name: String): Node = {
    val rule: RewriteRule = new RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if isTargetLayer(name)(elem) => Seq.empty
        case other                                   => other
      }
    }
    new RuleTransformer(rule).transform(in).head
  }

  describe("Transforming an internal element") {

    val inNode: Node = XML.loadString(s"""<svg xmlns:inkscape="$InkNs">
         |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
         |</svg>""".stripMargin)

    it("should find an Inkscape layer") {
      val outNode: Seq[Node] = findLayers(inNode)
      outNode.map(_.toString) shouldBe Seq(
        s"""<g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer" xmlns:inkscape="https://www.inkscape.org/namespaces/inkscape"/>"""
      )
    }

    it("should find multiple Inkscape layers") {
      val outNode: Seq[Node] = findLayers(ExampleSvg)
      outNode.map(_.toString) shouldBe Seq(
        """<g style="display:inline" inkscape:groupmode="layer" inkscape:label="Layer1" xmlns:inkscape="https://www.inkscape.org/namespaces/inkscape">
          |    <rect x="10" y="10" width="180" height="180" fill="blue"/>
          |  </g>""".stripMargin,
        """<g style="display:none" inkscape:groupmode="layer" inkscape:label="Layer2" xmlns:inkscape="https://www.inkscape.org/namespaces/inkscape">
          |    <line x1="0" y1="0" x2="200" y2="200" stroke="red"/>
          |    <circle cx="100" cy="100" r="50" fill="green"/>
          |  </g>""".stripMargin
      )
    }

    it("should enable or disable an Inkscape layer manually traversing the element") {
      val outNode: Node = transformLayerVisilityManual(inNode, "My Layer")
      outNode.toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:inline" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |</svg>""".stripMargin

      transformLayerVisilityManual(inNode, "My Layer", visible = false).toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |</svg>""".stripMargin
    }

    it("should enable or disable an Inkscape layer") {
      val outNode: Node = transformLayerVisilityRule(inNode, "My Layer")
      outNode.toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:inline" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |</svg>""".stripMargin

      transformLayerVisilityRule(inNode, "My Layer", visible = false).toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |</svg>""".stripMargin
    }

    it("should delete an Inkscape layer") {
      val outNode: Node = removeLayerRule(inNode, "My Layer")
      // Note that the trailing line space is significant
      outNode.toString shouldBe s"""<svg xmlns:inkscape="$InkNs">\n  \n</svg>"""
    }
  }
}
