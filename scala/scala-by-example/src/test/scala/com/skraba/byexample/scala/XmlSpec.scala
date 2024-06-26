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

  private def isTargetLayer(name: String)(n: Node): Boolean =
    n.label == "g" && (n \@ s"{$InkNs}groupmode") == "layer" && (n \@ s"{$InkNs}label") == name

  private def setLayerVisibility(elem: Elem, visible: Boolean): Elem = {
    val displayStyle = if (visible) "display:inline" else "display:none"
    elem.copy(attributes = elem.attributes.append(Attribute(null, "style", displayStyle, Null)))
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

  describe("Transforming an internal element") {

    val inNode: Node = XML.loadString(s"""<svg xmlns:inkscape="$InkNs">
         |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
         |  </svg>""".stripMargin)

    it("should enable or disable an Inkscape group manually traversing the element") {
      val outNode: Node = transformLayerVisilityManual(inNode, "My Layer")
      outNode.toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:inline" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |  </svg>""".stripMargin

      transformLayerVisilityManual(inNode, "My Layer", visible = false).toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |  </svg>""".stripMargin
    }

    it("should enable or disable an Inkscape group") {
      val outNode: Node = transformLayerVisilityRule(inNode, "My Layer")
      outNode.toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:inline" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |  </svg>""".stripMargin

      transformLayerVisilityRule(inNode, "My Layer", visible = false).toString shouldBe
        s"""<svg xmlns:inkscape="$InkNs">
           |  <g style="display:none" inkscape:groupmode="layer" inkscape:label="My Layer"/>
           |  </svg>""".stripMargin
    }
  }
}
