package com.skraba.byexample.scala.scalatest

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Matchers and assertions on collections.
  *
  * Remember, strings, Option and Array are also collections.
  *
  * @see
  *   [[https://www.scalatest.org/user_guide/using_matchers]]
  */
class CollectionsSpec extends AnyFunSpecLike with Matchers {

  describe("A simple list") {

    val vide: List[Int] = List()
    val xs: List[Int] = List(1, 2, 3)

    it("can assert emptiness") {
      // Uses the boolean property of isEmpty to determine if there are contents
      vide shouldBe Symbol("empty")
      xs should not be Symbol("empty")

      // Using the empty keyword works is better
      vide shouldBe empty
      xs should not be empty

      // You can check for emptiness by looking at the size, or length.
      vide should have size 0
      xs should not have size(0)
      vide should have length 0
      xs shouldNot have length 0

      // Or manually assert the boolean value
      xs.isEmpty shouldBe false
      assert(xs.nonEmpty)
    }

    it("can assert for equality") {
      // The same as any other basic assertion, including return values on methods
      xs shouldBe List(1, 2, 3)
      xs.head shouldBe 1
      xs.max shouldBe 3
      vide.size shouldBe 0
    }

    it("can assert for contents") {
      xs should contain(2)
      xs should not contain (99)
      xs should contain noneOf (99, 999)
      xs should contain allOf (1, 3)

      // oneOf is exactly one (and only one)
      xs should contain oneOf (3, 99, 999)
      xs should not contain oneOf(2, 3, 99)

      xs should contain atLeastOneOf (2, 3, 99)
      xs should contain atMostOneOf (2, 99, 999)
      xs should contain atMostOneOf (99, 999, 9999)

      // theSameElementsAs can take other collections as arguments.  Order doesn't matter
      // but the number of elements does.
      xs should contain theSameElementsAs Set(1, 2, 3)
      List(1, 1, 2, 3) should contain theSameElementsAs Vector(2, 3, 1, 1)
    }

    it("can assert for order") {
      // Simply sorted
      xs shouldBe sorted

      // Check that only
      List(1, 2, 2, 3, 3) should contain inOrderOnly (1, 2, 3)
      List(99, 1, 2, 2, 99, 3, 3, 3, 99) should contain inOrder (1, 2, 3)
      List(1, 1, 2, 3) should contain theSameElementsInOrderAs Vector(
        1,
        1,
        2,
        3
      )
    }

    it("can assert for singleton using loneElement") {
      import org.scalatest.LoneElement.convertToCollectionLoneElementWrapper

      val set = Set(1)
      set.loneElement should be <= 10
    }

    it("can assert using the Inspector methods on multidimensional objects") {
      import org.scalatest.Inspectors.forAll

      val table = List(List(0, 1, 2), List(3, 4), List(5, 6, 7))

      // We can iterate over multidimensional objects like this:
      forAll(table) { row =>
        forAll(row) { col =>
          col should be >= 0
        }
      }

      // It can be used over one dimension
      forAll(xs) { x => x should be < 10 }
      // But this is more practical.
      all(xs) should be < 10
    }

    it("can assert using helpful methods ") {
      // Demonstrate on a String, which is also a sequence of characters.
      val ys: Seq[Char] = "bcdef"

      all(ys) should be > 'a'
      atMost(2, ys) should be >= 'e'
      atLeast(3, ys) should be < 'f'
      between(2, 3, ys) should (be > 'b' and be < 'f')
      exactly(2, ys) should be <= 'c'
      every(ys) should be < 'j'
    }
  }

  describe("Maps") {
    val map = Map(1 -> "one", 2 -> "two", 3 -> "three")
    it("are collections of key value tuples") {
      map should contain(1 -> "one")
      map should not contain (2 -> "one")
      map should (contain key 2 and not contain value("seven"))
    }
  }
}
