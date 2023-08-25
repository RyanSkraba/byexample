package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase.withConsoleMatch
import mainargs.Flag
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import scala.io.AnsiColor._
import scala.reflect.io.Streamable

/** Test the [[ColourCfg]] helper. */
class ColourCfgSpec
    extends AnyFunSpecLike
    with BeforeAndAfterAll
    with Matchers {

  describe("Providing ANSI colours") {

    describe("via the style method") {
      it("controls whether bold, reset and colour control codes are added") {
        val cfg = ColourCfg()
        cfg.style("-") shouldBe s"$WHITE-$RESET"
        cfg.style("-", bold = false, reset = false) shouldBe s"$WHITE-"
        cfg.style("-", bold = false, reset = true) shouldBe s"$WHITE-$RESET"
        cfg.style("-", bold = true, reset = false) shouldBe s"$BOLD$WHITE-"
        cfg.style("-", bold = true, reset = true) shouldBe s"$BOLD$WHITE-$RESET"
        cfg.style("-", "", "*") shouldBe s"*-$RESET"
        cfg.style("-", "", "*", bold = false, reset = false) shouldBe s"*-"
        cfg.style("-", "", "*", bold = false, reset = true) shouldBe s"*-$RESET"
        cfg.style("-", "", "*", bold = true, reset = false) shouldBe s"$BOLD*-"
        cfg.style(
          "-",
          "",
          "*",
          bold = true,
          reset = true
        ) shouldBe s"$BOLD*-$RESET"
        cfg.style("-", "x") shouldBe s"$BOLD$WHITE-$RESET$WHITE x$RESET"
        cfg.style(
          "-",
          "x",
          bold = false,
          reset = false
        ) shouldBe s"$BOLD$WHITE-$RESET$WHITE x"
        cfg.style(
          "-",
          "x",
          bold = false,
          reset = true
        ) shouldBe s"$BOLD$WHITE-$RESET$WHITE x$RESET"
        cfg.style(
          "-",
          "x",
          bold = true,
          reset = false
        ) shouldBe s"$BOLD$WHITE- x"
        cfg.style(
          "-",
          "x",
          bold = true,
          reset = true
        ) shouldBe s"$BOLD$WHITE- x$RESET"
      }

      it("disables control codes") {
        val cfg = ColourCfg(plain = Flag(true))
        cfg.style("-") shouldBe "-"
        cfg.style("-", bold = false, reset = false) shouldBe "-"
        cfg.style("-", bold = false, reset = true) shouldBe "-"
        cfg.style("-", bold = true, reset = false) shouldBe "-"
        cfg.style("-", bold = true, reset = true) shouldBe "-"
        cfg.style("-", "", "*") shouldBe "-"
        cfg.style("-", "", "*", bold = false, reset = false) shouldBe "-"
        cfg.style("-", "", "*", bold = false, reset = true) shouldBe "-"
        cfg.style("-", "", "*", bold = true, reset = false) shouldBe "-"
        cfg.style("-", "", "*", bold = true, reset = true) shouldBe "-"
        cfg.style("-", "x", "*") shouldBe "- x"
        cfg.style("-", "x", "*", bold = false, reset = false) shouldBe "- x"
        cfg.style("-", "x", "*", bold = false, reset = true) shouldBe "- x"
        cfg.style("-", "x", "*", bold = true, reset = false) shouldBe "- x"
        cfg.style("-", "x", "*", bold = true, reset = true) shouldBe "- x"
      }
    }

    describe("for the 8 basic colours") {
      it("by default is activated and non-bold") {
        val cfg = ColourCfg()
        cfg.bold("-") shouldBe s"$BOLD-$RESET"
        cfg.black("-") shouldBe s"$BLACK-$RESET"
        cfg.red("-") shouldBe s"$RED-$RESET"
        cfg.green("-") shouldBe s"$GREEN-$RESET"
        cfg.yellow("-") shouldBe s"$YELLOW-$RESET"
        cfg.blue("-") shouldBe s"$BLUE-$RESET"
        cfg.magenta("-") shouldBe s"$MAGENTA-$RESET"
        cfg.cyan("-") shouldBe s"$CYAN-$RESET"
        cfg.white("-") shouldBe s"$WHITE-$RESET"
        cfg.blackBg("-") shouldBe s"$BLACK_B-$RESET"
        cfg.redBg("-") shouldBe s"$RED_B-$RESET"
        cfg.greenBg("-") shouldBe s"$GREEN_B-$RESET"
        cfg.yellowBg("-") shouldBe s"$YELLOW_B-$RESET"
        cfg.blueBg("-") shouldBe s"$BLUE_B-$RESET"
        cfg.magentaBg("-") shouldBe s"$MAGENTA_B-$RESET"
        cfg.cyanBg("-") shouldBe s"$CYAN_B-$RESET"
        cfg.whiteBg("-") shouldBe s"$WHITE_B-$RESET"
      }
      it("can be activated and bold") {
        val cfg = ColourCfg()
        cfg.black("-", bold = true) shouldBe s"$BOLD$BLACK-$RESET"
        cfg.red("-", bold = true) shouldBe s"$BOLD$RED-$RESET"
        cfg.green("-", bold = true) shouldBe s"$BOLD$GREEN-$RESET"
        cfg.yellow("-", bold = true) shouldBe s"$BOLD$YELLOW-$RESET"
        cfg.blue("-", bold = true) shouldBe s"$BOLD$BLUE-$RESET"
        cfg.magenta("-", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET"
        cfg.cyan("-", bold = true) shouldBe s"$BOLD$CYAN-$RESET"
        cfg.white("-", bold = true) shouldBe s"$BOLD$WHITE-$RESET"
        cfg.blackBg("-", bold = true) shouldBe s"$BOLD$BLACK_B-$RESET"
        cfg.redBg("-", bold = true) shouldBe s"$BOLD$RED_B-$RESET"
        cfg.greenBg("-", bold = true) shouldBe s"$BOLD$GREEN_B-$RESET"
        cfg.yellowBg("-", bold = true) shouldBe s"$BOLD$YELLOW_B-$RESET"
        cfg.blueBg("-", bold = true) shouldBe s"$BOLD$BLUE_B-$RESET"
        cfg.magentaBg("-", bold = true) shouldBe s"$BOLD$MAGENTA_B-$RESET"
        cfg.cyanBg("-", bold = true) shouldBe s"$BOLD$CYAN_B-$RESET"
        cfg.whiteBg("-", bold = true) shouldBe s"$BOLD$WHITE_B-$RESET"
      }
      it("can turn off the reset") {
        val cfg = ColourCfg()
        cfg.bold("-", reset = false) shouldBe s"$BOLD-"
        cfg.black("-", reset = false) shouldBe s"$BLACK-"
        cfg.red("-", reset = false) shouldBe s"$RED-"
        cfg.green("-", reset = false) shouldBe s"$GREEN-"
        cfg.yellow("-", reset = false) shouldBe s"$YELLOW-"
        cfg.blue("-", reset = false) shouldBe s"$BLUE-"
        cfg.magenta("-", reset = false) shouldBe s"$MAGENTA-"
        cfg.cyan("-", reset = false) shouldBe s"$CYAN-"
        cfg.white("-", reset = false) shouldBe s"$WHITE-"
        cfg.blackBg("-", reset = false) shouldBe s"$BLACK_B-"
        cfg.redBg("-", reset = false) shouldBe s"$RED_B-"
        cfg.greenBg("-", reset = false) shouldBe s"$GREEN_B-"
        cfg.yellowBg("-", reset = false) shouldBe s"$YELLOW_B-"
        cfg.blueBg("-", reset = false) shouldBe s"$BLUE_B-"
        cfg.magentaBg("-", reset = false) shouldBe s"$MAGENTA_B-"
        cfg.cyanBg("-", reset = false) shouldBe s"$CYAN_B-"
        cfg.whiteBg("-", reset = false) shouldBe s"$WHITE_B-"
      }
      for (bold <- Seq(false, true); reset <- Seq(false, true)) {
        it(s"can be deactivated for non-ansi use (bold: $bold, reset $reset)") {
          val cfg = ColourCfg(plain = Flag(true))
          cfg.bold("-", reset = reset) shouldBe "-"
          cfg.black("-", bold = bold, reset = reset) shouldBe "-"
          cfg.red("-", bold = bold, reset = reset) shouldBe "-"
          cfg.green("-", bold = bold, reset = reset) shouldBe "-"
          cfg.yellow("-", bold = bold, reset = reset) shouldBe "-"
          cfg.blue("-", bold = bold, reset = reset) shouldBe "-"
          cfg.magenta("-", bold = bold, reset = reset) shouldBe "-"
          cfg.cyan("-", bold = bold, reset = reset) shouldBe "-"
          cfg.white("-", bold = bold, reset = reset) shouldBe "-"
          cfg.blackBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.redBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.greenBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.yellowBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.blueBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.magentaBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.cyanBg("-", bold = bold, reset = reset) shouldBe "-"
          cfg.whiteBg("-", bold = bold, reset = reset) shouldBe "-"
        }
      }
    }

    describe("for some semantic colours") {
      it("by default is activated and non-bold") {
        val cfg = ColourCfg()
        cfg.ok("-") shouldBe s"$GREEN-$RESET"
        cfg.warn("-") shouldBe s"$YELLOW-$RESET"
        cfg.error("-") shouldBe s"$RED-$RESET"
        cfg.left("-") shouldBe s"$CYAN-$RESET"
        cfg.right("-") shouldBe s"$MAGENTA-$RESET"
        cfg.kv("-", "x") shouldBe s"$MAGENTA-$RESET : x"
      }
      it("can be activated and bold") {
        val cfg = ColourCfg()
        cfg.ok("-", bold = true) shouldBe s"$BOLD$GREEN-$RESET"
        cfg.warn("-", bold = true) shouldBe s"$BOLD$YELLOW-$RESET"
        cfg.error("-", bold = true) shouldBe s"$BOLD$RED-$RESET"
        cfg.left("-", bold = true) shouldBe s"$BOLD$CYAN-$RESET"
        cfg.right("-", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET"
        cfg.kv("-", "x", bold = true) shouldBe s"$BOLD$MAGENTA-$RESET : x"
      }
      it("can turn off the reset") {
        val cfg = ColourCfg()
        cfg.ok("-", reset = false) shouldBe s"$GREEN-"
        cfg.warn("-", reset = false) shouldBe s"$YELLOW-"
        cfg.error("-", reset = false) shouldBe s"$RED-"
        cfg.left("-", reset = false) shouldBe s"$CYAN-"
        cfg.right("-", reset = false) shouldBe s"$MAGENTA-"
        cfg.kv("-", "x", reset = false) shouldBe s"$MAGENTA- : x"
      }
      for (bold <- Seq(false, true); reset <- Seq(false, true)) {
        it(s"can be deactivated for non-ansi use (bold: $bold, reset $reset)") {
          val cfg = ColourCfg(plain = Flag(true))
          cfg.ok("-", bold = bold, reset = reset) shouldBe "-"
          cfg.warn("-", bold = bold, reset = reset) shouldBe "-"
          cfg.error("-", bold = bold, reset = reset) shouldBe "-"
          cfg.left("-", bold = bold, reset = reset) shouldBe "-"
          cfg.right("-", bold = bold, reset = reset) shouldBe "-"
          cfg.kv("-", "x", bold = bold, reset = reset) shouldBe "- : x"
        }
      }
    }
  }

  describe("The ask() method") {

    it("should not prompt the user when the yes flag is set") {
      val cfg = ColourCfg(yes = Flag(true))
      // The short version
      cfg.ask("What's the magic word?") {
        "Please"
      } shouldBe Some("Please")

      // The long version, where the additional methods are never called
      cfg.ask("What's the magic word?")(
        yFn = "Please",
        nFn = fail("nFn"),
        qFn = fail("qFn"),
        otherFn = _ => fail("otherFn")
      ) shouldBe Some("Please")
    }

    it("should happen when the yes flag is not set") {
      val cfg = ColourCfg()
      Streamable.closing(new ByteArrayInputStream("y\n".getBytes)) { in =>
        Console.withIn(in) {
          withConsoleMatch(cfg.ask("What's the magic word?") {
            "Please"
          }) { case (result, out, err) =>
            err shouldBe empty
            out shouldBe "What's the magic word? (Y/n/q): "
          }
        }
      }
    }
  }
}
