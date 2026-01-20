package com.skraba.byexample.json.base

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Base class to demonstrate how to do things in Scala */
abstract class JsonSpecBase extends AnyFunSpecLike with Matchers {

  // TODO: Can we share an interface between Scala and Java?

  def testParseStringIntoJson(): Unit

  def testParseStreamIntoJson(): Unit

  describe("Parsing existing data") {
    it("should read JSON from a String") {
      testParseStringIntoJson()
    }

    it("should read JSON from a Stream") {
      testParseStreamIntoJson()
    }
  }
}
