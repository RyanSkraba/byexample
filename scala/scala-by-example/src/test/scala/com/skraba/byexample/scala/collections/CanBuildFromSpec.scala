package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

// TODO: This is deprecated and this class requires a rewrite
// import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/** Collections and [[CanBuildFrom]]
  *
  * @see
  *   [[https://www.scala-lang.org/blog/2017/05/30/tribulations-canbuildfrom.html]]
  */
class CanBuildFromSpec extends AnyFunSpecLike with Matchers {
// TODO
//  describe("CanBuildFrom") {
//
//    it("is used to ensure that a String can be mapped to a String") {
//
//      // A string is a implicitly sequence of characters.
//      val orig = "Hello world"
//      orig shouldBe a[String]
//      "val check: Seq[Char] = orig" should compile
//
//      // So you can map it to a sequence of integers.
//      "val check: Seq[Int] = orig.map(_.toInt)" should compile
//
//      // Or magically to a String (or sequence of characters, whatever).
//      "val check: Seq[Char] = orig.map(_.toUpper)" should compile
//      "val check: String = orig.map(_.toUpper)" should compile
//
//      // This magic comes from the Predefs StringCanBuildFrom!
//      val scbf: CanBuildFrom[String, Char, String] = implicitly
//      scbf shouldBe StringCanBuildFrom
//    }
//  }
//
//  /** A custom list implementation that I want to behave the same way.
//    *
//    * @param dlg
//    *   Delegate all of the actual logic here (but we'd probably actually want
//    *   some custom logic).
//    */
//  case class MagicList(private val dlg: Boolean*) extends Seq[Boolean] {
//    override def apply(idx: Int): Boolean = dlg(idx)
//
//    override def length: Int = dlg.length
//
//    override def iterator: Iterator[Boolean] = dlg.iterator
//  }
//
//  describe("MagicList") {
//
//    it("looks like a Seq but doesn't have the same magic") {
//      val orig: MagicList = MagicList(true, false, true)
//      "val check: Seq[Boolean] = orig" should compile
//
//      // So you can map it to a sequence of integers.
//      orig.map(if (_) 1 else 0) shouldBe Seq(1, 0, 1)
//
//      // Or magically to a String (or sequence of characters, whatever.
//      orig.map(!_) shouldBe Seq(false, true, false)
//      // But there is no coversion
//      "val x: MagicList = orig.map(!_)" shouldNot compile
//
//      // Because there's no way to build it.
//      "implicitly[CanBuildFrom[MagicList, Boolean, MagicList]]" shouldNot compile
//    }
//
//    it("can act like a sequence and MagicList") {
//
//      // We can add the magic with an implicit CanBuildFrom
//      implicit val mcbf: CanBuildFrom[Seq[Boolean], Boolean, MagicList] =
//        new CanBuildFrom[Seq[Boolean], Boolean, MagicList] {
//          override def apply(
//              from: Seq[Boolean]
//          ): mutable.Builder[Boolean, MagicList] = apply()
//
//          override def apply(): mutable.Builder[Boolean, MagicList] =
//            new mutable.LazyBuilder[Boolean, MagicList] {
//              override def result(): MagicList =
//                MagicList(parts.flatMap(_.toSeq): _*)
//            }
//        }
//
//      val orig: MagicList = MagicList(true, false, true)
//      "val check: Seq[Boolean] = orig" should compile
//
//      // This used to fail
//      val mapped: MagicList = orig.map(!_)
//      mapped shouldBe MagicList(false, true, false)
//
//      // MagicList can be used magically!
//      orig ++ orig shouldBe MagicList(true, false, true, true, false, true)
//      orig ++ orig shouldBe a[MagicList]
//      orig :+ false shouldBe MagicList(true, false, true, false)
//      false +: orig shouldBe MagicList(false, true, false, true)
//
//      // Because there's now a way to build it.
//      implicitly[CanBuildFrom[MagicList, Boolean, MagicList]] shouldBe mcbf
//    }
//  }
//
//  /** Another custom list implementation that adds state to the list. This
//    * requires some fussing with [[equals()]] and [[hashCode()]] and should
//    * probably be avoided by preferring composition to inheritance.
//    *
//    * @param name
//    *   the name of the list.
//    * @param dlg
//    *   Delegate all of the list logic here.
//    */
//  case class NamedList(name: String, private val dlg: Boolean*)
//      extends Seq[Boolean] {
//    override def apply(idx: Int): Boolean = dlg(idx)
//
//    override def length: Int = dlg.length
//
//    override def iterator: Iterator[Boolean] = dlg.iterator
//
//    override def equals(that: Any): Boolean =
//      // Necessary because equals isn't generated since Seq already has an implementation.
//      that.isInstanceOf[NamedList] && that
//        .asInstanceOf[NamedList]
//        .name
//        .equals(name) && super.equals(that)
//
//    override def hashCode(): Int = name.hashCode * 31 + super.hashCode
//  }
//
//  object NamedList {
//
//    // Retain the name if building from a known collection.
//    implicit val SeqBooleanCanBuildFrom
//        : CanBuildFrom[Seq[Boolean], Boolean, NamedList] =
//      new CanBuildFrom[Seq[Boolean], Boolean, NamedList] {
//        override def apply(
//            from: Seq[Boolean]
//        ): mutable.Builder[Boolean, NamedList] =
//          new mutable.LazyBuilder[Boolean, NamedList] {
//            override def result(): NamedList = {
//              from match {
//                case nl: NamedList =>
//                  NamedList(nl.name, parts.flatMap(_.toSeq): _*)
//                case _ => NamedList("", parts.flatMap(_.toSeq): _*)
//              }
//
//            }
//          }
//
//        override def apply(): mutable.Builder[Boolean, NamedList] = apply(
//          NamedList("")
//        )
//      }
//  }
//
//  describe("NamedList") {
//
//    val orig: NamedList = NamedList("orig", true, false)
//
//    it("implements equals and hashCode") {
//      orig shouldBe NamedList("orig", true, false)
//      orig should not be NamedList("orig", false)
//      orig should not be NamedList("", true, false)
//      orig.hashCode should not be NamedList("", true, false).hashCode
//    }
//
//    it("can act like a sequence and NamedList") {
//
//      // This is still true.
//      "val check: Seq[Boolean] = orig" should compile
//
//      // This used to fail
//      orig.map(!_) shouldBe NamedList("orig", false, true)
//
//      val mapped: NamedList = orig.map(!_)
//      mapped shouldBe NamedList("orig", false, true)
//
//      // NamedList can be used magically!
//      import NamedList._
//      orig ++ orig shouldBe NamedList("orig", true, false, true, false)
//      orig :+ false shouldBe NamedList("orig", true, false, false)
//      false +: orig shouldBe NamedList("orig", false, true, false)
//
//      // But here's the weirdness and why you should avoid using lists with state.
//      orig should not be Seq(true, false)
//      Seq(true, false) shouldBe orig
//    }
//  }
}
