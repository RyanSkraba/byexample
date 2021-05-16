package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.ScalatagsGoSpec.{
  withScalatagsGo,
  withScalatagsGoMatch
}
import org.docopt.DocoptExitException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.xml.XML

/** Unit tests for the [[DuolingoCnCheatsheetTask]] CLI.
  */
class DuolingoCnCheatsheetTaskSpec extends AnyFunSpecLike with Matchers {

  describe("ScalatagsGo cheatsheet valid commands") {

    it("should print help with --help") {
      val t = intercept[DocoptExitException] {
        withScalatagsGo(DuolingoCnCheatsheetTask.Cmd, "--help")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe DuolingoCnCheatsheetTask.Doc
    }

    it("should print an SVG document when run without arguments ") {
      withScalatagsGoMatch(DuolingoCnCheatsheetTask.Cmd) {
        case (stdout, stderr) =>
          stderr shouldBe ""
          val svg = XML.loadString(stdout)
          svg.label shouldBe "svg"
          svg.child.size shouldBe 1
          svg.child.head.label shouldBe "g"
          svg.child.head.child.size shouldBe 88
      }
    }

    it("should filter on lessons") {
      withScalatagsGoMatch(DuolingoCnCheatsheetTask.Cmd, "--section=3") {
        case (stdout, stderr) =>
          stderr shouldBe ""
          val svg = XML.loadString(stdout)
          svg.label shouldBe "svg"
          svg.child.size shouldBe 1
          svg.child.head.label shouldBe "g"
          svg.child.head.child.size shouldBe 13
      }
    }

    it("should filter on individual words") {
      withScalatagsGoMatch(DuolingoCnCheatsheetTask.Cmd, "--word=一二三") {
        case (stdout, stderr) =>
          stderr shouldBe ""
          val svg = XML.loadString(stdout)
          svg.label shouldBe "svg"
          svg.child.size shouldBe 1
          svg.child.head.label shouldBe "g"
          svg.child.head.child.size shouldBe 1
      }
      // Comma-delimit some matches.
      withScalatagsGoMatch(DuolingoCnCheatsheetTask.Cmd, "--word=不客气,对不起,没关系") {
        case (stdout, stderr) =>
          stderr shouldBe ""
          val svg = XML.loadString(stdout)
          svg.label shouldBe "svg"
          svg.child.size shouldBe 1
          svg.child.head.label shouldBe "g"
          svg.child.head.child.size shouldBe 1
      }
    }
  }
}
