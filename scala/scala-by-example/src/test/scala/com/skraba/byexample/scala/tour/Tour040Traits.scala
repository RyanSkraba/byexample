package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour040Traits extends AnyFunSpecLike with Matchers {

  describe("Traits") {

    it("can be very simple") {
      trait HairColor

      // Created an anonymous implementation
      val x: HairColor = new HairColor {}
      x shouldBe a[HairColor]
    }

    it("can be implemented with extends and generics") {
      trait Iterator[A] {
        def hasNext: Boolean

        def next(): A
      }

      class IntIterator(to: Int) extends Iterator[Int] {
        private var current = 0

        override def hasNext: Boolean = current < to

        override def next(): Int = {
          if (hasNext) {
            val t = current
            current += 1
            t
          } else 0
        }
      }

      val iterator = new IntIterator(10)
      iterator.next() shouldBe 0
      iterator.next() shouldBe 1

    }

    it("can be subtyped") {
      import scala.collection.mutable.ArrayBuffer

      trait Pet {
        val name: String
      }

      class Cat(val name: String) extends Pet
      class Dog(val name: String) extends Pet

      val dog = new Dog("Harry")
      val cat = new Cat("Sally")

      val animals = ArrayBuffer.empty[Pet]
      animals.append(dog)
      animals.append(cat)

      animals(0).name shouldBe "Harry"
      animals(0) shouldBe a[Dog]
      animals(1).name shouldBe "Sally"
      animals(1) shouldBe a[Cat]
    }
  }
}
