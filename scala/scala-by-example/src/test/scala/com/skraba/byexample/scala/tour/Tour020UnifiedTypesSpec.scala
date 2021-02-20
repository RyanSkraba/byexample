package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * {{{
  * Any <-- AnyVal
  * AnyVal <-- Double <- Nothing
  *        <-- Float
  *        <-- Long
  *        <-- Int
  *        <-- Short
  *        <-- Byte
  *        <-- Unit
  *        <-- Boolean
  *        <-- Char
  * Any    <-- AnyRef (java.lang.object)
  * AnyRef <-- List <-- Null <-- Nothing
  *        <-- Option
  *        <-- YourClass
  * }}}
  *
  * Any is the base with equals() hashCode() and toString()
  *
  * AnyVal has 9 predefined value types.
  * * Unit is the equivalent of void, and is denoted `()`
  *
  * AnyRef is the reference type.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour020UnifiedTypesSpec extends AnyFunSpecLike with Matchers {

  describe("UnifiedTypes") {

    it("should have all values as base.") {
      val list: List[Any] = List(
        "a string",
        732, // an integer
        'c', // a character
        true, // a boolean value
        () => "an anonymous function returning a string"
      )

      list shouldBe a[List[_]]

      val stringy = list.map(_.toString)
      stringy should have size 5
      stringy(1) shouldBe "732"
    }

    // You can cast Byte -> Short -> Int -> Long -> Float -> Double
    // and Char -> Int
    // but not in the other direction
    it("should have casting") {
      val x: Long = 987654321
      val y: Float = x
      // 9.8765434E8 (note that some precision is lost in this case)
      assert(y === 987654321f +- 50)
      """val z: Long = y""" shouldNot compile

      val face: Char = '☺'
      val number: Int = face // 9786
      assert(number === 9786)
    }
  }
}
