package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer

/** Collections and [[CanBuildFrom]]
  *
  * @see https://www.scala-lang.org/blog/2017/05/30/tribulations-canbuildfrom.html
  */
class CanBuildFromSpec extends AnyFunSpecLike with Matchers {

  describe("CanBuildFrom") {

    it("is used to ensure that a String can be mapped to a String") {

      // A string is a implicitly sequence of characters.
      val orig = "Hello world"
      orig shouldBe a[String]
      "val x: Seq[Char] = orig" should compile

      // So you can map it to a sequence of integers.
      "val x: Seq[Int] = orig.map(_.toInt)" should compile

      // Or magically to a String (or sequence of characters, whatever.
      "val x: Seq[Char] = orig.map(_.toUpper)" should compile
      "val x: String = orig.map(_.toUpper)" should compile

      // This magic comes from the Predefs StringCanBuildFrom!
      val scbf : CanBuildFrom[String, Char, String] = implicitly
      scbf shouldBe StringCanBuildFrom
    }
  }
}
