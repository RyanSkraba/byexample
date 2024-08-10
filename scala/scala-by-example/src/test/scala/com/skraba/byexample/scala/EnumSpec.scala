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

  /** A enumeration with associated information for each value. */
  object GoldenGirl extends Enumeration {
    protected case class MyVal(actress: String, birthday: String) extends super.Val

    import scala.language.implicitConversions
    implicit def valuesToMyVal(x: Value): MyVal = x.asInstanceOf[MyVal]

    val Dorothy: MyVal = MyVal("Bea Arthur", "May 13, 1922")
    val Rose: MyVal = MyVal("Betty White", "January 17, 1922")
    val Blanche: MyVal = MyVal("Rue McClanahan", "February 21, 1934")
    val Sophia: MyVal = MyVal("Estelle Getty", "July 25, 1923")
  }

  /** As an alternative to the enum, scala devs often use sealed traits. Note that some of the implementation (such as
    * the id and number) needs to be manually implemented if required.
    */
  sealed trait Greeter {
    def id: Int

    def greeting(name: String): String
  }

  object Greeter {

    case object Friendly extends Greeter { val id = 0; def greeting(name: String): String = s"Hello $name!" }

    case object Casual extends Greeter { val id = 1; def greeting(name: String): String = s"Hi $name." }

    case object Formal extends Greeter { val id = 2; def greeting(name: String): String = s"Salutations, $name." }
    case object Aimable extends Greeter { val id = 3; def greeting(name: String): String = s"Bonjour $name!" }

    val maxId = 4

    def values: Set[Greeter] = Set(Friendly, Casual, Formal, Aimable)

    def apply(i: Int): Greeter = values.find(_.id == i).getOrElse {
      throw new NoSuchElementException(s"key not found: $i")
    }

    def withName(s: String): Greeter = values.find(_.toString == s).getOrElse {
      throw new NoSuchElementException(s"No value found for '$s'")
    }
  }

  describe("Using a basic Dir enum") {

    import Dir._

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

  describe("Using an enhanced GoldenGirl enum") {

    import GoldenGirl._

    it("should have a max value") { GoldenGirl.maxId shouldBe 4 }

    it("should list its values") { GoldenGirl.values shouldBe Set(Dorothy, Rose, Blanche, Sophia) }

    describe("when converting from strings") {

      it("should generate a string correctly") { Rose.toString shouldBe "Rose" }

      it("should interpret a string correctly") { GoldenGirl.withName("Dorothy") shouldBe Dorothy }

      it("should throw an exception on a bad string") {
        intercept[NoSuchElementException] {
          GoldenGirl.withName("Stan")
        }.getMessage shouldBe "No value found for 'Stan'"
      }

      it("should wrap in Try to avoid an exception") { Try(GoldenGirl.withName("Stan")).toOption shouldBe None }
    }

    describe("when converting as ordinal integers") {

      it("should generate an integer correctly") { Blanche.id shouldBe 2 }

      it("should interpret a integer correctly") { GoldenGirl(1) shouldBe Rose }

      it("should throw an exception on a bad string") {
        intercept[NoSuchElementException] {
          GoldenGirl(100)
        }.getMessage shouldBe "key not found: 100"
      }

      it("should wrap in Try to avoid an exception") { Try(GoldenGirl(100)).toOption shouldBe None }
    }

    describe("when getting enum data") {
      it("should generate an integer correctly") {
        Rose.actress shouldBe "Betty White"
        Rose.birthday shouldBe "January 17, 1922"
      }
    }
  }

  describe("Using a sealed trait Greeting") {

    import Greeter._

    it("should have a max value") { Greeter.maxId shouldBe 4 }

    it("should list its values") { Greeter.values shouldBe Set(Friendly, Casual, Formal, Aimable) }

    describe("when converting from strings") {

      it("should generate a string correctly") { Casual.toString shouldBe "Casual" }

      it("should interpret a string correctly") { Greeter.withName("Formal") shouldBe Formal }

      it("should throw an exception on a bad string") {
        intercept[NoSuchElementException] {
          Greeter.withName("Aggro")
        }.getMessage shouldBe "No value found for 'Aggro'"
      }

      it("should wrap in Try to avoid an exception") { Try(Greeter.withName("Aggro")).toOption shouldBe None }
    }

    describe("when converting as ordinal integers") {

      it("should generate an integer correctly") { Aimable.id shouldBe 3 }

      it("should interpret a integer correctly") { Greeter(1) shouldBe Casual }

      it("should throw an exception on a bad string") {
        intercept[NoSuchElementException] {
          Greeter(100)
        }.getMessage shouldBe "key not found: 100"
      }

      it("should wrap in Try to avoid an exception") { Try(Greeter(100)).toOption shouldBe None }
    }

    describe("when getting enum data") {
      it("should generate an integer correctly") {
        Aimable.greeting("Betty") shouldBe "Bonjour Betty!"
      }
    }
  }
}
