package com.skraba.byexample.scala.ammonite.validator

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Test the [[Cfg]] instances. */
class CfgSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  val props: Map[String, String] = Map("string" -> "ValueFromMap")

  describe("String configuration using Cfg.str") {
    it("should take any explicitly set value") {
      val cfg = Cfg.str(Some("Explicit"), props, "string", "Documentation")("default")
      cfg.get shouldBe "Explicit"
      cfg.isDefault shouldBe false
    }
  }
}
