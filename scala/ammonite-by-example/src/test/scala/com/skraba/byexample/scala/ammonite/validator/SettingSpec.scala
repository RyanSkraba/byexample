package com.skraba.byexample.scala.ammonite.validator

import com.skraba.docoptcli.AnsiConsole
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.io.AnsiColor._

/** Test the [[Setting]] instances. */
class SettingSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  val props: Map[String, String] = Map(
    "string" -> "ValueFromMap",
    "empty" -> "",
    "booleanFalse" -> "false",
    "booleanTrue" -> "true",
    "pathRel" -> "propsRel",
    "pathAbs" -> "/props/abs"
  )

  describe("Generating property files") {
    it("should show whether values are default or not") {
      Setting.properties(
        out = AnsiConsole(),
        cfgs = Setting.str(Some("Explicit"), props, "key1", "Doc")("default"),
        Setting.str(None, props, "key2", "Doc")("default")
      ) shouldBe
        s"""${CYAN}key1$RESET=${MAGENTA}Explicit$RESET
           |${CYAN}key2$RESET=${GREEN}default$RESET""".stripMargin
    }
  }

  describe("String configuration using Setting.str") {
    it("should take any explicitly set value") {
      val x = Setting.str(Some("Explicit"), props, "string", "Doc")("default")
      x.get shouldBe "Explicit"
      x.isDefault shouldBe false
    }

    it("should fetch values from properties") {
      val x = Setting.str(None, props, "string", "Doc")("default")
      x.get shouldBe "ValueFromMap"
      x.isDefault shouldBe false
    }

    it("should even fetch values if they are empty") {
      val x = Setting.str(None, props, "empty", "Doc")("default")
      x.get shouldBe ""
      x.isDefault shouldBe false
    }

    it("should fall back to a default value when the property doesn't exist") {
      val x = Setting.str(None, props, "no-exist", "Doc")("default")
      x.get shouldBe "default"
      x.isDefault shouldBe true
    }

    it("should detect the default regardless of how it finds the value") {
      Setting.str(Some("Explicit"), props, "string", "Doc")("Explicit").isDefault shouldBe true
      Setting.str(None, props, "string", "Doc")("ValueFromMap").isDefault shouldBe true
      Setting.str(None, props, "empty", "Doc")("").isDefault shouldBe true
      Setting.str(None, props, "no-exist", "Doc")("default").isDefault shouldBe true
    }
  }

  describe("Boolean configuration using Setting.bool") {
    it("should take any explicitly set value") {
      val x1 = Setting.bool(Some(true), props, "booleanTrue", "Doc")(d = false)
      x1.get shouldBe true
      x1.isDefault shouldBe false
      val x2 = Setting.bool(Some(false), props, "booleanFalse", "Doc")(d = true)
      x2.get shouldBe false
      x2.isDefault shouldBe false
    }

    it("should fetch values from properties") {
      val x1 = Setting.bool(None, props, "booleanTrue", "Doc")(d = false)
      x1.get shouldBe true
      x1.isDefault shouldBe false
      val x2 = Setting.bool(None, props, "booleanFalse", "Doc")(d = true)
      x2.get shouldBe false
      x2.isDefault shouldBe false
    }

    it("should fall back to a default value when the property doesn't exist") {
      val x1 = Setting.bool(None, props, "no-exist", "Doc")(d = true)
      x1.get shouldBe true
      x1.isDefault shouldBe true
      val x2 = Setting.bool(None, props, "no-exist", "Doc")(d = false)
      x2.get shouldBe false
      x2.isDefault shouldBe true
    }

    it("should detect the default regardless of how it finds the value") {
      Setting.bool(Some(true), props, "booleanFalse", "Doc")(d = true).isDefault shouldBe true
      Setting.bool(Some(false), props, "booleanTrue", "Doc")(d = false).isDefault shouldBe true
      Setting.bool(None, props, "booleanTrue", "Doc")(d = true).isDefault shouldBe true
      Setting.bool(None, props, "booleanFalse", "Doc")(d = false).isDefault shouldBe true
      Setting.bool(None, props, "no-exist", "Doc")(d = true).isDefault shouldBe true
      Setting.bool(None, props, "no-exist", "Doc")(d = false).isDefault shouldBe true
    }
  }

  describe("path configuration using Setting.path") {
    val base = os.Path("/base")
    val other = os.Path("/other")

    it("should take any explicitly set value") {
      val xRel = Setting.path(base, Some("rel"), props, "pathAbs", "Doc")(other)
      xRel.get shouldBe base / "rel"
      xRel.isDefault shouldBe false
      val xAbs = Setting.path(base, Some("/path/abs"), props, "pathRel", "Doc")(other)
      xAbs.get shouldBe os.Path("/path/abs")
      xAbs.isDefault shouldBe false
    }

    it("should fetch values from properties") {
      val xRel = Setting.path(base, None, props, "pathRel", "Doc")(other)
      xRel.get shouldBe base / "propsRel"
      xRel.isDefault shouldBe false
      val xAbs = Setting.path(base, None, props, "pathAbs", "Doc")(other)
      xAbs.get shouldBe os.Path("/props/abs")
      xAbs.isDefault shouldBe false
    }

    it("should fall back to a default value when the property doesn't exist") {
      val x = Setting.path(base, None, props, "no-exist", "Doc")(other)
      x.get shouldBe other
      x.isDefault shouldBe true
    }

    it("should detect the default regardless of how it finds the value") {
      Setting.path(base, Some("rel"), props, "pathAbs", "Doc")(base / "rel").isDefault shouldBe true
      Setting.path(base, Some("/path/abs"), props, "pathRel", "Doc")(os.Path("/path/abs")).isDefault shouldBe true
      Setting.path(base, None, props, "pathRel", "Doc")(base / "propsRel").isDefault shouldBe true
      Setting.path(base, None, props, "pathAbs", "Doc")(os.Path("/props/abs")).isDefault shouldBe true
      Setting.path(base, None, props, "no-exist", "Doc")(other).isDefault shouldBe true
    }
  }
}
