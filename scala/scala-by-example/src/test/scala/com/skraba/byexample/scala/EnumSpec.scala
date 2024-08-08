package com.skraba.byexample.scala

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.Try

/** Using [[Enumeration]] in Scala. These are significantly improved in Scala 3! */
class EnumSpec extends AnyFunSpecLike with Matchers {

  /** A basic enumeration with just values. */
  object Dir extends Enumeration {
    type Dir = Value
    val East, South, West, North = Value
  }
  import Dir._

  describe("Using a basic Dir enum") {

    it("should have a max value") { Dir.maxId shouldBe 4 }

    it("should list its values") { Dir.values shouldBe Set(East, South, West, North) }

    describe("when converting from strings") {

      it("should generate a string correctly") { West.toString shouldBe "West" }

      it("should interpret a string correctly") { Dir.withName("East") shouldBe East }

      it("should throw an exception on a bad string") {
        intercept[NoSuchElementException] {
          Dir.withName("Norst")
        }.getMessage shouldBe "No value found for 'Norst'"
      }

      it("should wrap in Try to avoid an exception") { Try(Dir.withName("Norst")).toOption shouldBe None }
    }

    describe("when converting as ordinal integers") {

      it("should generate an integer correctly") { West.id shouldBe 2 }

      it("should interpret a integer correctly") { Dir(1) shouldBe South }

      it("should throw an exception on a bad string") {
        intercept[NoSuchElementException] {
          Dir(100)
        }.getMessage shouldBe "key not found: 100"
      }

      it("should wrap in Try to avoid an exception") { Try(Dir(100)).toOption shouldBe None }
    }
  }
}
