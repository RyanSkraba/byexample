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
    it("looks a lot like a Seq, but can be infinite") {
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

  it("can be iterate over a state and detect loops detection") {
    // This mystery function takes and returns an Int.
    def mystery(in: Int): Int =
      if (in == 83) 47 else if (in == 1099) 1095 else in + 1

    // states is the list of repeatedly applying the mystery function to the previous value, starting at 0
    val states = LazyList.iterate(0)(mystery)

    // We can mostly treat it as a sequence
    states.take(3) shouldBe Seq(0, 1, 2)
    states(99) shouldBe 62

    // If we wanted to find the billionth element in the list, we'd have to apply the mystery function a billion times.
    val index = 1000000000
    // states(index) shouldBe 75 // Very very slow

    // Iterate from the start until we find a state we've already seen
    val cache = collection.mutable.Set[Int]()
    val repeat = states.dropWhile(cache.add)

    // The length of the cycle is found by counting until the head is found again
    val repeat1 = repeat.head
    val repeatLen = 1 + repeat.tail.takeWhile(_ != repeat1).size
    // The cache includes the first cycle, so repeat0 is the number of element before the cycle starts
    val repeat0 = cache.size - repeatLen

    // After skipping the number of repeated cycles, we still need to iterate a few times
    val remaining = (index - repeat0) % repeatLen

    // Find a cycle
    val end = repeat.drop(remaining).head

    // This is the equivalent value for states(1000000000)
    end shouldBe 75
  }
}
