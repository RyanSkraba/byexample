package com.skraba.byexample.scalatags

import com.skraba.byexample.scalatags.ScalatagsGo.{InternalDocoptException, go}
import org.docopt.DocoptExitException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/**
  * Unit tests for the helpers in the [[ScalatagsGo]] project.
  */
class ScalatagsGoSpec extends AnyFunSpecLike with Matchers {

  describe("ScalatagsGo docopt check") {
    it("should have less than 80 characters per string for readability.") {
      for (line <- ScalatagsGo.Doc.split("\n")) {
        withClue("main" -> line) {
          line.length should be < 80
        }
      }
      for (
        task <- ScalatagsGo.Tasks;
        line <- task.doc.split("\n")
      ) {
        withClue(task.cmd -> line) {
          line.length should be < 80
        }
      }
    }
  }

  describe("ScalatagsGo valid commands") {
    it("throw an exception with --version") {
      val t = intercept[DocoptExitException] {
        go("--version")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe ScalatagsGo.Version
    }

    it("throw an exception with --help") {
      val t = intercept[DocoptExitException] {
        go("--help")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe ScalatagsGo.Doc
    }

    it("throw an exception like --help when run bare") {
      val t = intercept[DocoptExitException] {
        go()
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe ScalatagsGo.Doc
    }
  }

  describe("ScalatagsGo command line options") {
    it("throw an exception like --help when run without a command") {
      val t = intercept[InternalDocoptException] {
        go("--debug")
      }
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe ScalatagsGo.Doc
    }

    for (
      args <- Seq(
        Seq("--garbage"),
        Seq("--debug", "--garbage"),
        Seq("--garbage", "--debug"),
        Seq("--garbage", "garbage")
      )
    ) it(s"throw an exception with unknown option $args") {
      val t = intercept[DocoptExitException] {
        go(args: _*)
      }
      t.getExitCode shouldBe 1
      t.getMessage shouldBe null
    }

    for (
      args <- Seq(
        Seq("garbage"),
        Seq("--debug", "garbage")
      )
    ) it(s"throw an exception when an unknown command is sent $args") {
      val t = intercept[InternalDocoptException] {
        go("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe ScalatagsGo.Doc
    }
  }
}
