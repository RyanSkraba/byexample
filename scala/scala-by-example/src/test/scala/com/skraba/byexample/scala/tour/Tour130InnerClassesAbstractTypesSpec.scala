package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour130InnerClassesAbstractTypesSpec
    extends AnyFunSpecLike
    with Matchers {

  describe("Inner classes") {
    // In Java, inner classes are always bound to the outer class.  In scala, they are bound to the
    // outer *object* or instance by default.
    it("are bound to the instance, not the outer class") {
      // Simple graph data type with an inner class.
      class Graph {
        class Node {
          var connectedNodes: List[Node] = Nil

          def connectTo(node: Node): Unit = {
            if (!connectedNodes.exists(node.equals)) {
              connectedNodes = node :: connectedNodes
            }
          }
        }

        var nodes: List[Node] = Nil

        def newNode: Node = {
          val res = new Node
          nodes = res :: nodes
          res
        }
      }

      // Using the inner classes.  Note that the type of the nodes is bound to the graph1 instance.
      val graph1: Graph = new Graph
      val node1: graph1.Node = graph1.newNode
      val node2: graph1.Node = graph1.newNode
      val node3: graph1.Node = graph1.newNode
      node1.connectTo(node2)
      node3.connectTo(node1)
      node1.connectedNodes should contain(node2)

      val graph2: Graph = new Graph
      val otherNode: graph2.Node = graph2.newNode
      // This is not acceptable, since node1 is of type graph1.Node and otherNode is of type
      // graph2.Node
      "otherNode.connectTo(node1)" shouldNot compile
    }

    it("can use the # notation to accept inner classes from other instances") {
      // The same graph as above, but the Graph#Node notation refers to inner classes created on
      // any instance.
      class Graph {
        class Node {
          var connectedNodes: List[Graph#Node] = Nil

          def connectTo(node: Graph#Node): Unit = {
            if (!connectedNodes.exists(node.equals)) {
              connectedNodes = node :: connectedNodes
            }
          }
        }

        var nodes: List[Node] = Nil

        def newNode: Node = {
          val res = new Node
          nodes = res :: nodes
          res
        }
      }

      // Using the inner classes.  Note that the type of the nodes is bound to the graph1 instance.
      val graph1: Graph = new Graph
      val node1: graph1.Node = graph1.newNode
      val node2: graph1.Node = graph1.newNode
      val node3: graph1.Node = graph1.newNode
      node1.connectTo(node2)
      node3.connectTo(node1)
      node1.connectedNodes should contain(node2)

      val graph2: Graph = new Graph
      val otherNode: graph2.Node = graph2.newNode
      // This is now accepted, but now the graph is in a weird state.
      otherNode.connectTo(node1)
      otherNode.connectedNodes should contain(node1)

      // In this example, newNode still only tracks the Node instances for the Graph instance.
    }
  }

  describe("Abstract types") {
    it("is simple") {
      // Trait with an abstract type.
      trait Buffer {
        type T
        val element: T
      }
      // An abstract class that refines the abstract type to enforce an upper bound.
      abstract class SeqBuffer extends Buffer {
        type U
        type T <: Seq[U]

        def length: Int = element.length
      }
      // Still abstract, but the upper bound is now defined.
      abstract class IntSeqBuffer extends SeqBuffer {
        type U = Int
      }
      // Create a concrete instance (anonymous) of the buffer.
      def newIntSeqBuf(elem1: Int, elem2: Int): IntSeqBuffer =
        new IntSeqBuffer {
          type T = List[U]
          val element: List[Int] = List(elem1, elem2)
        }

      val buf = newIntSeqBuf(7, 8)
      buf.length shouldBe 2
      buf.element shouldBe List(7, 8)
    }

    it("can be equivalent to type parameters") {
      // The equivalent of the above.  Choose between type parameters and abstract types depending
      // of functionality.
      abstract class Buffer[+T] {
        val element: T
      }
      abstract class SeqBuffer[U, +T <: Seq[U]] extends Buffer[T] {
        def length: Int = element.length
      }
      def newIntSeqBuf(e1: Int, e2: Int): SeqBuffer[Int, Seq[Int]] =
        new SeqBuffer[Int, List[Int]] {
          val element: List[Int] = List(e1, e2)
        }

      val buf = newIntSeqBuf(7, 8)
      buf.length shouldBe 2
      buf.element shouldBe List(7, 8)
    }
  }
}
