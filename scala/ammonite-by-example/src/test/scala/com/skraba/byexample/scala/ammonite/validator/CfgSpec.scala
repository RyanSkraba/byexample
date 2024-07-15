package com.skraba.byexample.scala.ammonite.validator

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Test the [[Cfg]] instances. */
class CfgSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  val props: Map[String, String] = Map("string" -> "ValueFromMap", "empty" -> "")

  describe("String configuration using Cfg.str") {
    it("should take any explicitly set value") {
      val cfg = Cfg.str(Some("Explicit"), props, "string", "Doc")("default")
      cfg.get shouldBe "Explicit"
      cfg.isDefault shouldBe false
    }

    it("should fetch values from properties") {
      val cfg = Cfg.str(None, props, "string", "Doc")("default")
      cfg.get shouldBe "ValueFromMap"
      cfg.isDefault shouldBe false
    }

    it("should even fetch values if they are empty") {
      val cfg = Cfg.str(None, props, "empty", "Doc")("default")
      cfg.get shouldBe ""
      cfg.isDefault shouldBe false
    }

    it("should fall back to a default value when the property doesn't exist") {
      val cfg = Cfg.str(None, props, "no-exist", "Doc")("default")
      cfg.get shouldBe "default"
      cfg.isDefault shouldBe true
    }
  }
}
