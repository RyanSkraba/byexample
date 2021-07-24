package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Examples from the scala collections doc.  Each spec covers a page.
  *
  * Seq is an Iterable with a length.
  *
  * LinearSeq has efficient head and tail operations: linked list (List) / Stream
  *
  * IndexedSeq has efficient apply, length, and (if mutable) update: Array, ArrayBuffer, Vector
  *
  * Buffer is a type of mutable sequence that permits removing items (among others): ListBuffer, ArrayBuffer
  *
  * Mutable sequences can have their elements changed, but not necessary grow or shrink.
  *
  * @see https://docs.scala-lang.org/overviews/collections/introduction.html
  */
class Collections030SeqSpec extends AnyFunSpecLike with Matchers {

  describe("Seq") {

    // Instance for testing.
    val xs = Seq(1, 2, 3)
    val xsInf = Stream.from(1)

    it("has a default implementation of immutable.List") {
      xs shouldBe a[List[_]]
      xs shouldBe a[PartialFunction[_, _]] // Int, T
    }

    it("supports indexing and length") {
      // Partial function
      xs(1) shouldBe 2
      xs.apply(1) shouldBe 2 // alias
      xs.isDefinedAt(1) shouldBe true
      xs.isDefinedAt(99) shouldBe false

      xs.length shouldBe 3
      xs.size shouldBe 3 // alias

      xs.hasDefiniteSize shouldBe true
      xsInf.hasDefiniteSize shouldBe false
      // xsInf.length shouldBe 3) // Infinite

      // Useful for comparing to an infinite sequence, returns -1, 0, 1
      xs.lengthCompare(3) shouldBe 0
      xsInf lengthCompare 42 shouldBe 1
      xs.indices shouldBe Range(0, 3, 1)
    }

    it("supports nested sequences") {
      val ys = Seq(Seq(0, 1, 2), Seq(3, 4, 5))
      ys(1)(1) shouldBe 4
      ys.flatten shouldBe Seq(0, 1, 2, 3, 4, 5)
    }

    it("supports index search") {
      xs indexOf 1 shouldBe 0
      xs lastIndexOf 1 shouldBe 0
      xs indexOfSlice Seq(2, 3) shouldBe 1
      xs lastIndexOfSlice Seq(2, 3) shouldBe 1
      xs indexWhere (_ % 2 == 0) shouldBe 1
      xs segmentLength (_ < 3, 0) shouldBe 2
      xs segmentLength (_ < 3, 1) shouldBe 1
      xs prefixLength (_ < 3) shouldBe 2
    }

    it("support additions") {
      0 +: xs shouldBe Seq(0, 1, 2, 3)
      xs :+ 0 shouldBe Seq(1, 2, 3, 0)
      xs.padTo(5, 99) shouldBe Seq(1, 2, 3, 99, 99)

      // Note that padTo doesn't trim the size of the Seq
      xs.padTo(2, 99) shouldBe Seq(1, 2, 3)
    }

    it("support updates") {
      xs patch (0, Seq(99, 98, 97), 2) shouldBe Seq(99, 98, 97, 3)
      xs patch (1, Seq(99, 98, 97, 96), 1) shouldBe Seq(1, 99, 98, 97, 96, 3)
      xs updated (2, 99) shouldBe Seq(1, 2, 99)

      // mutable only -- updated for non-mutable and update for mutable.
      val xsMut = mutable.Seq(1, 2, 3)
      xsMut(2) = 99
      xsMut.update(1, 98) // equivalent
      xsMut shouldBe Seq(1, 98, 99)
    }

    it("supports sorting") {
      xs.sorted shouldBe Seq(1, 2, 3)
      (100 +: xs).sortWith(_ < _) shouldBe Seq(1, 2, 3, 100)
      (100 +: xs).sortBy(_.toString) shouldBe Seq(1, 100, 2, 3)
    }

    it("supports reversals") {
      xs.reverse shouldBe Seq(3, 2, 1)
      xs.reverseIterator sameElements Seq(3, 2, 1).iterator shouldBe true
      xs.reverseMap(100 - _) shouldBe Seq(97, 98, 99)
    }

    it("supports comparisons") {
      xs.startsWith(Seq(1, 2)) shouldBe true
      xs.startsWith(Seq(2, 3), 1) shouldBe true
      xs.endsWith(Seq(2, 3)) shouldBe true
      xs.containsSlice(Seq(2, 3)) shouldBe true
      (xs corresponds Seq(2, 4, 6))(_ * 2 == _) shouldBe true
    }

    it("supports multi-set operations") {
      xs intersect Seq(3, 2, 4) shouldBe Seq(2, 3) // preserves order in xs
      xs diff Seq(3, 4) shouldBe Seq(1, 2) // preserves order in xs
      xs union Seq(3, 4) shouldBe Seq(1, 2, 3, 3, 4) // same as ++
      (xs union Seq(3, 4)).distinct shouldBe Seq(1, 2, 3, 4)
    }
  }

  describe("Buffer") {

    it("has a default implementation of mutable.ArrayBuffer") {
      val buf = mutable.Buffer(1, 2, 3)
      buf shouldBe a[ArrayBuffer[_]]
    }

    it("supports additions") {
      val buf = mutable.Buffer(1, 2, 3)

      // These operations return itself.

      // Append
      (buf += 4) shouldBe mutable.Buffer(1, 2, 3, 4)
      (buf += (5, 6, 7)) shouldBe mutable.Buffer(1, 2, 3, 4, 5, 6, 7)
      (buf ++= Seq(8, 9)) shouldBe mutable.Buffer(1, 2, 3, 4, 5, 6, 7, 8, 9)

      // Prepend
      (0 +=: buf) shouldBe mutable.Buffer(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      (Seq(-2, -1) ++=: buf) shouldBe mutable.Buffer(-2, -1, 0, 1, 2, 3, 4, 5,
        6, 7, 8, 9)

      // Insert does not return itself
      buf insert (2, 100)
      buf shouldBe mutable.Buffer(-2, -1, 100, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

      buf insertAll (2, Seq(97, 98, 99))
      buf shouldBe mutable.Buffer(-2, -1, 97, 98, 99, 100, 0, 1, 2, 3, 4, 5, 6,
        7, 8, 9)
    }

    it("supports removal") {
      val buf =
        ArrayBuffer(-2, -1, 97, 98, 99, 100, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

      (buf -= 100) shouldBe mutable.Buffer(-2, -1, 97, 98, 99, 0, 1, 2, 3, 4, 5,
        6, 7, 8, 9)

      buf remove 4 shouldBe 99
      buf shouldBe mutable.Buffer(-2, -1, 97, 98, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

      buf remove (2, 3)
      buf shouldBe mutable.Buffer(-2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9)

      buf trimStart 2
      buf shouldBe mutable.Buffer(1, 2, 3, 4, 5, 6, 7, 8, 9)

      buf trimEnd 6
      buf shouldBe mutable.Buffer(1, 2, 3)

      buf.clear
      buf shouldBe empty
    }

    it("supports cloning") {
      val buf = mutable.Buffer(1, 2, 3)
      val c = buf.clone

      c shouldBe buf
      c should not be theSameInstanceAs(buf)
    }
  }
}
