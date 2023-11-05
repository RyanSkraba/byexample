package com.skraba.byexample.scala.collections
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.Sorting

/** Collections, [[Sorting]] and [[Ordering]]
  *
  * @see
  *   [[https://www.scala-lang.org/api/2.12.6/scala/math/Ordering.html]]
  */
class SortingOrderingSpec extends AnyFunSpecLike with Matchers {

  case class Issue(prj: String, num: Int, priorities: Int*)

  def issueSample(): Array[Issue] = Array(
    Issue("a", 500, 2),
    Issue("b", 100, 3, 2, 1),
    Issue("c", 300),
    Issue("d", 300),
    Issue("e", 200, 1, 99, 50, 40)
  )

  def prj(in: Seq[Issue]): Seq[String] = in.map(_.prj)

  describe("When sorting a collection") {
    val xs = issueSample().toSeq

    it("the sorted method requires an Ordering") {
      val ys = xs.sorted(Ordering.by[Issue, Int](_.num))
      prj(xs) shouldBe Seq("a", "b", "c", "d", "e")
      prj(ys) shouldBe Seq("b", "e", "c", "d", "a")
    }
  }

  describe("Sorting.quickSort") {
    it("can be used to sort common arrays") {
      val xs = Array(3, 2, 1)
      Sorting.quickSort(xs) // Unit
      // The array is modified in place
      xs shouldBe Array(1, 2, 3)
    }
  }

  describe("Sorting.stableSort") {

    it("can be used to sort common arrays") {
      val xs = Array(3, 2, 1)
      Sorting.stableSort(xs)
      // The array is modified in place
      xs shouldBe Array(1, 2, 3)
    }

    it("can be used to sort immutable sequences") {
      val xs = Seq(3, 2, 1)
      // The sequence is not modified, but the return value is.
      Sorting.stableSort(xs) shouldBe Seq(1, 2, 3)
      xs shouldBe Seq(3, 2, 1)
    }

    it("can be used to sort tuples") {
      // The tuples are sorted using the first to last elements in order.
      Sorting.stableSort(Seq((3, 0), (2, 99), (1, 1), (1, 0))) shouldBe Seq(
        (1, 0),
        (1, 1),
        (2, 99),
        (3, 0)
      )
    }
  }

  describe("Ordering") {

    it("can be customized for tuples") {
      val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))

      // sort by 2nd element
      Sorting.quickSort(pairs)(Ordering.by[(String, Int, Int), Int](_._2))
      pairs shouldBe Seq(("b", 1, 3), ("c", 3, 1), ("a", 5, 2))

      // sort by the 3rd element, then 1st
      Sorting.quickSort(pairs)(Ordering[(Int, String)].on(x => (x._3, x._1)))
      pairs shouldBe Seq(("c", 3, 1), ("a", 5, 2), ("b", 1, 3))
    }

    it("can be customized for case classes") {
      // Sort by num
      val xs = issueSample()
      Sorting.quickSort(xs)(_.num compare _.num)
      prj(xs) shouldBe Seq("b", "e", "c", "d", "a")

      // Sort by the number of priorities
      Sorting.quickSort(xs)(_.priorities.length compare _.priorities.length)
      prj(xs) shouldBe Seq("c", "d", "a", "b", "e")

      // Sort by project
      Sorting.stableSort(xs, (x: Issue, y: Issue) => x.prj < y.prj)
      prj(xs) shouldBe Seq("a", "b", "c", "d", "e")
    }

    it("can be added for a case class") {
      object IssueOrderByNum extends Ordering[Issue] {
        def compare(a: Issue, b: Issue): Int = a.num.compare(b.num)
      }

      val xs = issueSample()
      Sorting.quickSort(xs)(IssueOrderByNum)
      prj(xs) shouldBe Seq("b", "e", "c", "d", "a")
    }

    it("can be added implicitly for a case class") {
      implicit object IssueOrderByPrio extends Ordering[Issue] {
        def compare(a: Issue, b: Issue): Int = a.priorities.maxOption
          .getOrElse(Int.MinValue)
          .compare(b.priorities.maxOption.getOrElse(Int.MinValue))
      }

      val xs = issueSample()
      // We don't need to provide the implicit ordering.
      Sorting.quickSort(xs)
      prj(xs) shouldBe Seq("c", "d", "a", "b", "e")
      // We can even modify the implicit ordering without knowing it!
      Sorting.quickSort(xs)(implicitly[Ordering[Issue]].reverse)
      prj(xs) shouldBe Seq("e", "b", "a", "c", "d")
    }
  }
}
