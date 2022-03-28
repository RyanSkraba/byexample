package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.{SortedMap, immutable, mutable}

/** Examples from the scala collections doc. Each spec covers a page.
  *
  * @see
  *   https://docs.scala-lang.org/overviews/collections/introduction.html
  */
class Collections050MapSpec extends AnyFunSpecLike with Matchers {

  describe("Maps") {

    // Instance for testing.
    val ms = Map(1 -> "one", 2 -> "two", 3 -> "three")

    it("has a default implementation of immutable.TreeMap") {
      // Any smaller maps may have a specialized implementation.
      ms + (4 -> "four", 5 -> "five") shouldBe a[immutable.HashMap[_, _]]

      // Of course they aren't necessarily in order... we don't know if the following is true.
      // xs.takeRight(1) shouldBe Map(1 -> "one"))

      // Because of the apply method, a set can be used as a function.
      List(2, 3, 4).collect(ms) shouldBe List("two", "three")

      // And a composable function (What is the string length of the input size?)
      List(1, 2, 3).collect(ms.andThen(_.length)) shouldBe List(3, 3, 5)
    }

    it("support lookups") {
      // Get as option
      ms.get(1) shouldBe Some("one")
      ms.get(4) shouldBe None

      // Get directly with an exception if not found.
      ms(1) shouldBe "one"
      ms.apply(1) shouldBe "one" // alias
      intercept[NoSuchElementException] {
        ms(4)
      }

      // Using an indicator for not found.
      ms.getOrElse(4, -1) shouldBe -1

      ms.contains(4) shouldBe false
      ms.isDefinedAt(4) shouldBe false // alias, partial function
    }

    it("support additions and updates") {
      ms + (4 -> "four") shouldBe Map(
        1 -> "one",
        2 -> "two",
        3 -> "three",
        4 -> "four"
      )
      ms updated (4, "four") shouldBe Map(
        1 -> "one",
        2 -> "two",
        3 -> "three",
        4 -> "four"
      ) // alias
      ms + (4 -> "four", 5 -> "five") shouldBe Map(
        1 -> "one",
        2 -> "two",
        3 -> "three",
        4 -> "four",
        5 -> "five"
      )
      ms ++ Map(99 -> "ninety-nine") shouldBe Map(
        1 -> "one",
        2 -> "two",
        3 -> "three",
        99 -> "ninety-nine"
      )
    }

    it("supports removals") {
      ms - 2 shouldBe Map(1 -> "one", 3 -> "three")
      ms - (2, 3) shouldBe Map(1 -> "one")
      ms -- List(1, 2) shouldBe Map(3 -> "three")
    }

    it("supports subcollections") {
      ms.keys.toSet shouldBe Set(1, 2, 3) // Actually returns an Iterable
      ms.keySet shouldBe Set(1, 2, 3)
      ms.keysIterator.toSet shouldBe Set(
        1,
        2,
        3
      ) // Actually returns an Iterator
      ms.values.toSet shouldBe Set(
        "one",
        "two",
        "three"
      ) // Actually returns an Iterable
      ms.valuesIterator.toSet shouldBe Set(
        "one",
        "two",
        "three"
      ) // Actually returns an Iterator
    }

    it("supports transformations") {
      // both are map views
      ms.filterKeys(_ % 2 == 1) shouldBe Map(1 -> "one", 3 -> "three")
      ms.mapValues(_.reverse) shouldBe Map(1 -> "eno", 2 -> "owt", 3 -> "eerht")
    }
  }

  describe("Sorted maps") {
    it("has a default implementation of immutable.TreeMap") {
      // Any smaller sets may have a specialized implementation.
      val xs = SortedMap(5 -> "A", 4 -> "A", 3 -> "B", 2 -> "X", 1 -> "A")
      xs shouldBe a[immutable.TreeMap[_, _]]
      // And it has an order
      xs.toSeq shouldBe Seq(1 -> "A", 2 -> "X", 3 -> "B", 4 -> "A", 5 -> "A")
    }

    it("can be created from an existing map") {
      val xs = SortedMap(5 -> "A", 4 -> "A", 3 -> "B", 2 -> "X", 1 -> "A")
      val sortedXs = SortedMap.empty[Int, String] ++ xs
      sortedXs shouldBe a[immutable.TreeMap[_, _]]
      sortedXs.toSeq shouldBe Seq(
        1 -> "A",
        2 -> "X",
        3 -> "B",
        4 -> "A",
        5 -> "A"
      )
    }
  }

  describe("Mutable maps") {

    it("support additions and updates") {
      val ms = mutable.Map(1 -> "one", 2 -> "two", 3 -> "three")
      ms(1) = "un"
      ms.update(2, "deux") // alias
      ms shouldBe Map(1 -> "un", 2 -> "deux", 3 -> "three")

      (ms += (4 -> "four")) shouldBe Map(
        1 -> "un",
        2 -> "deux",
        3 -> "three",
        4 -> "four"
      )
      (ms += (4 -> "quatre", 3 -> "trois")) shouldBe Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "quatre"
      )

      (ms ++= Map(5 -> "cinq")) shouldBe Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "quatre",
        5 -> "cinq"
      )
      ms shouldBe Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "quatre",
        5 -> "cinq"
      )

      ms.put(4, "cat") shouldBe Some("quatre")
      ms.put(6, "six") shouldBe None
      ms shouldBe Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "cat",
        5 -> "cinq",
        6 -> "six"
      )

      ms.getOrElseUpdate(6, "SIXSIX") shouldBe "six"
      ms.getOrElseUpdate(7, "sept") shouldBe "sept"
      ms shouldBe Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "cat",
        5 -> "cinq",
        6 -> "six",
        7 -> "sept"
      )
    }

    it("support removals") {
      var ms = mutable.Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "quatre",
        5 -> "cinq",
        6 -> "six",
        7 -> "sept"
      )
      (ms -= 1) shouldBe Map(
        2 -> "deux",
        3 -> "trois",
        4 -> "quatre",
        5 -> "cinq",
        6 -> "six",
        7 -> "sept"
      )

      (ms -= (1, 2, 3)) shouldBe Map(
        4 -> "quatre",
        5 -> "cinq",
        6 -> "six",
        7 -> "sept"
      )

      (ms --= Seq(2, 3, 7)) shouldBe Map(4 -> "quatre", 5 -> "cinq", 6 -> "six")

      ms shouldBe Map(4 -> "quatre", 5 -> "cinq", 6 -> "six")

      (ms remove 2) shouldBe None
      (ms remove 4) shouldBe Some("quatre")
      ms shouldBe Map(5 -> "cinq", 6 -> "six")

      ms = mutable.Map(
        1 -> "un",
        2 -> "deux",
        3 -> "trois",
        4 -> "quatre",
        5 -> "cinq",
        6 -> "six",
        7 -> "sept"
      )
      (ms retain ((k, v) => k % 2 == 0)) shouldBe Map(
        2 -> "deux",
        4 -> "quatre",
        6 -> "six"
      )
      ms.keySet shouldBe Set(2, 4, 6)

      ms.clear()
      ms shouldBe Map()
    }

    it("supports transformations and cloning") {
      val ms = mutable.Map(1 -> "one", 2 -> "two", 3 -> "three")
      // values only, but with key as a paramter
      ms.transform((k, v) => v.reverse + k) shouldBe Map(
        1 -> "eno1",
        2 -> "owt2",
        3 -> "eerht3"
      )
      ms.clone shouldBe ms
      ms.clone should not be theSameInstanceAs(ms)
    }
  }
}
