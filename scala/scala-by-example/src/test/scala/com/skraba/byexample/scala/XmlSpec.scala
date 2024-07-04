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

  val InkNs: String = "http://www.inkscape.org/namespaces/inkscape"

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
        s"""<g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer" xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"/>"""
      )
    }

    it("should find multiple Inkscape layers") {
      val moreNode = XML.loadString(s"""<svg xmlns:inkscape="$InkNs">
          |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
          |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer2"/>
          |</svg>""".stripMargin)
      val outNode: Seq[Node] = findLayers(moreNode)
      outNode.map(_.toString) shouldBe Seq(
        s"""<g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer" xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"/>""",
        s"""<g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer2" xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"/>"""
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
