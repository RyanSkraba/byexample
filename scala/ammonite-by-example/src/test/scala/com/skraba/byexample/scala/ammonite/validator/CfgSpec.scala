package com.skraba.byexample.scala.ammonite.validator

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Test the [[Cfg]] instances. */
class CfgSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  val props: Map[String, String] =
    Map("string" -> "ValueFromMap", "empty" -> "", "booleanFalse" -> "false", "booleanTrue" -> "true")

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

    it("should detect the default regardless of how it finds the value") {
      Cfg.str(Some("Explicit"), props, "string", "Doc")("Explicit").isDefault shouldBe true
      Cfg.str(None, props, "string", "Doc")("ValueFromMap").isDefault shouldBe true
      Cfg.str(None, props, "empty", "Doc")("").isDefault shouldBe true
      Cfg.str(None, props, "no-exist", "Doc")("default").isDefault shouldBe true
    }
  }

  describe("Boolean configuration using Cfg.bool") {
    it("should take any explicitly set value") {
      val cfg1 = Cfg.bool(Some(true), props, "booleanTrue", "Doc")(d = false)
      cfg1.get shouldBe true
      cfg1.isDefault shouldBe false
      val cfg2 = Cfg.bool(Some(false), props, "booleanFalse", "Doc")(d = true)
      cfg2.get shouldBe false
      cfg2.isDefault shouldBe false
    }

    it("should fetch values from properties") {
      val cfg1 = Cfg.bool(None, props, "booleanTrue", "Doc")(d = false)
      cfg1.get shouldBe true
      cfg1.isDefault shouldBe false
      val cfg2 = Cfg.bool(None, props, "booleanFalse", "Doc")(d = true)
      cfg2.get shouldBe false
      cfg2.isDefault shouldBe false
    }

    it("should fall back to a default value when the property doesn't exist") {
      val cfg1 = Cfg.bool(None, props, "no-exist", "Doc")(d = true)
      cfg1.get shouldBe true
      cfg1.isDefault shouldBe true
      val cfg2 = Cfg.bool(None, props, "no-exist", "Doc")(d = false)
      cfg2.get shouldBe false
      cfg2.isDefault shouldBe true
    }

    it("should detect the default regardless of how it finds the value") {
      Cfg.bool(Some(true), props, "booleanFalse", "Doc")(d = true).isDefault shouldBe true
      Cfg.bool(Some(false), props, "booleanTrue", "Doc")(d = false).isDefault shouldBe true
      Cfg.bool(None, props, "booleanTrue", "Doc")(d = true).isDefault shouldBe true
      Cfg.bool(None, props, "booleanFalse", "Doc")(d = false).isDefault shouldBe true
      Cfg.bool(None, props, "no-exist", "Doc")(d = true).isDefault shouldBe true
      Cfg.bool(None, props, "no-exist", "Doc")(d = false).isDefault shouldBe true
    }
  }
}
