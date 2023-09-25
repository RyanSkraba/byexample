package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** The [[LazyList]] replaces the old Stream collect, and is useful for lists
  * that are programmatically calculated, especially infinite. An element of a
  * LazyList shouldn't be calculated until it is necessary.
  *
  * @see
  *   [[https://www.scala-lang.org/api/2.13.4/scala/collection/immutable/LazyList.html]]
  */
class LazyListSpec extends AnyFunSpecLike with Matchers {

  describe("LazyList") {
    it("can be infinitely repeating") {
      val x: LazyList[Int] = LazyList.continually(0)
      x.knownSize shouldBe -1

      // Many operations like size, min, contains will never finish
      x.contains(0) shouldBe true
      // x.size shouldBe 0 // Infinite
      // x.contains(1) shouldBe false // Infinite
      // x.toSet shouldBe Set(0)
      x.isEmpty shouldBe false
      x(99) shouldBe 0
      // x(Int.MaxValue) shouldBe 0 // Not infinite, but it will try to evaluate 2B values

      // It can be partially evaluated internally and treated like a Seq
      val y = x.updated(3, 1)
      y shouldBe a[LazyList[_]]

      // Subsequences are lazily evalutated as well, even if they're bounded
      val y5 = y.take(5)
      y5 shouldBe a[LazyList[_]]
      y5 shouldBe Seq(0, 0, 0, 1, 0)
      y5.size shouldBe 5

      // knownSize is only ever 0 if the list is known to be empty, otherwise it's -1
      x.knownSize shouldBe -1
      y.knownSize shouldBe -1
      y5.knownSize shouldBe -1
      y5.dropWhile(_ => true).knownSize shouldBe -1
      LazyList().knownSize shouldBe 0

      y.drop(5) shouldBe a[LazyList[_]]
    }
  }
}
