package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour050ClassCompositionWithMixinsSpec extends AnyFunSpecLike with Matchers {

  describe("Class composition") {

    it("can have multiple inheritance of traits") {
      abstract class A {
        val message: String
      }
      class B extends A {
        val message = "I'm an instance of class B"
      }
      trait C extends A {
        def loudMessage: String = message.toUpperCase()
      }
      class D extends B with C

      // Abstract class, class, trait.
      // D can have as many traits as it wants, but only one super class.
      // Traits can extend classes but then can only be applied to implementations of that class.

      val d = new D
      d.message shouldBe "I'm an instance of class B"
      d.loudMessage shouldBe "I'M AN INSTANCE OF CLASS B"
    }

    it("can have interesting examples") {

      // Abstract class has an abstract type (defined by subclass).
      abstract class AbsIterator {
        type T

        def hasNext: Boolean

        def next(): T
      }

      // Implementation gives the type as well.
      class StringIterator(s: String) extends AbsIterator {
        type T = Char
        private var i = 0

        def hasNext: Boolean = i < s.length

        def next(): Char = {
          val ch = s charAt i
          i += 1
          ch
        }
      }

      // Classic iterator
      val si = new StringIterator("Hello")
      si.hasNext shouldBe true
      si.next shouldBe a[Character]
      si.next shouldBe 'e'

      // Add a foreach implementation
      trait RichIterator extends AbsIterator {
        def foreach(f: T => Unit): Unit = while (hasNext) f(next())
      }

      // Iter class has both methods.
      class Iter(s: String) extends StringIterator(s) with RichIterator

      val ri = new Iter("Hello")
      ri.hasNext shouldBe true
      ri.next shouldBe a[Character]
      ri.next shouldBe 'e'
      // The rest of the values should be chars too.
      ri.foreach(_ shouldBe a[Character])
      ri.hasNext shouldBe false
    }
  }
}
