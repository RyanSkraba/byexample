package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, mutable}
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

      // Or magically to a String (or sequence of characters, whatever).
      "val x: Seq[Char] = orig.map(_.toUpper)" should compile
      "val x: String = orig.map(_.toUpper)" should compile

      // This magic comes from the Predefs StringCanBuildFrom!
      val scbf: CanBuildFrom[String, Char, String] = implicitly
      scbf shouldBe StringCanBuildFrom
    }
  }

  /** A custom list implementation that I want to behave the same way.
    *
    * @param dlg Delegate all of the actual logic here (but we'd probably actually want some custom logic).
    */
  case class MagicList(dlg: Seq[Boolean] = Seq.empty) extends Seq[Boolean] {
    override def apply(idx: Int): Boolean = dlg(idx)

    override def length: Int = dlg.length

    override def iterator: Iterator[Boolean] = dlg.iterator
  }

  describe("MagicList") {

    it("looks like a Seq but doesn't have the same magic.") {
      val orig: MagicList = MagicList(Seq(true, false, true))
      "val x: Seq[Boolean] = orig" should compile

      // So you can map it to a sequence of integers.
      orig.map(if (_) 1 else 0) shouldBe Seq(1, 0, 1)

      // Or magically to a String (or sequence of characters, whatever.
      orig.map(!_) shouldBe Seq(false, true, false)
      // But there is no coversion
      "val x: MagicList = orig.map(!_)" shouldNot compile

      // Because there's no way to build it.
      "implicitly[CanBuildFrom[MagicList, Boolean, MagicList]]" shouldNot compile
    }

    it("can act like a sequence and MagicList.") {

      // We can add the magic with an implicit CanBuildFrom
      implicit val mcbf: CanBuildFrom[Seq[Boolean], Boolean, MagicList] =
        new CanBuildFrom[Seq[Boolean], Boolean, MagicList] {
          override def apply(
              from: Seq[Boolean]
          ): mutable.Builder[Boolean, MagicList] = apply()

          override def apply(): mutable.Builder[Boolean, MagicList] =
            new mutable.LazyBuilder[Boolean, MagicList] {
              override def result(): MagicList =
                MagicList(parts.flatMap(_.toSeq))

            }
        }

      val orig: MagicList = MagicList(Seq(true, false, true))
      "val x: Seq[Boolean] = orig" should compile

      // This used to fail
      orig.map(!_) shouldBe MagicList(Seq(false, true, false))

      // MagicList can be used magically!
      orig ++ orig shouldBe MagicList(Seq(true, false, true, true, false, true))
      orig :+ false shouldBe MagicList(Seq(true, false, true, false))
      false +: orig shouldBe MagicList(Seq(false, true, false, true))

      // Because there's now a way to build it.
      "implicitly[CanBuildFrom[MagicList, Boolean, MagicList]]" should compile
    }
  }

}
