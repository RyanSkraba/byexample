package com.skraba.byexample.scala

import com.skraba.byexample.scala.ScalaGo.{withScalaGo, withScalaGoMatch}
import org.docopt.DocoptExitException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

/** Unit tests for [[ScalaGo]]
  */
class ScalaGoSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  describe("ScalaGo docopt check") {
    it("should have less than 80 characters per string for readability.") {
      for (line <- ScalaGo.Doc.split("\n")) {
        withClue("main" -> line) {
          line.length should be < 80
        }
      }
    }
  }

  describe("ScalaGo invalid command lines") {
    it("throw an exception with --version") {
      val t = intercept[DocoptExitException] {
        withScalaGo("--version")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe ScalaGo.Version
    }

    it("throw an exception with --help") {
      val t = intercept[DocoptExitException] {
        withScalaGo("--help")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe ScalaGo.Doc
    }

    it("should fail when --name doesn't have a value") {
      val t = intercept[DocoptExitException] {
        withScalaGo("--name")
      }
      t.getExitCode shouldBe 1
      t.getMessage shouldBe "--name requires argument"
    }

    it("should fail when --count doesn't have a value") {
      val t = intercept[DocoptExitException] {
        withScalaGo("--count")
      }
      t.getExitCode shouldBe 1
      t.getMessage shouldBe "--count requires argument"
    }

    it(s"throw an exception with unknown options") {
      for (
        args <- Seq(
          Seq("--garbage"),
          Seq("--debug", "--garbage"),
          Seq("--garbage", "--debug"),
          Seq("--garbage", "garbage")
        )
      ) withClue(s"Using: $args") {
        val t = intercept[DocoptExitException] {
          withScalaGo(args: _*)
        }
        t.getExitCode shouldBe 1
        t.getMessage shouldBe null
      }
    }
  }

  describe("ScalaGo valid command lines") {
    it("should match stdout and stderr using withScalaGoMatch.") {
      withScalaGoMatch() { case (stdout, stderr) =>
        stdout shouldBe "Hello, JavaScalaGo!\n" * 10
        stderr shouldBe ""
      }
    }

    it("should return stdout and stderr using withScalaGo") {
      withScalaGo() shouldBe ("Hello, JavaScalaGo!\n" * 10, "")
    }

    it("should be the same with the --scala option") {
      withScalaGo("--scala") shouldBe ("Hello, JavaScalaGo!\n" * 10, "")
    }

    it("should choose a name with the --name option") {
      withScalaGo("--name", "world") shouldBe ("Hello, world!\n" * 10, "")
      withScalaGo("--name=world") shouldBe ("Hello, world!\n" * 10, "")
    }

    it("should change the count with the --count option") {
      withScalaGo("--count", "3") shouldBe ("Hello, JavaScalaGo!\n" * 3, "")
      withScalaGo("--count=3") shouldBe ("Hello, JavaScalaGo!\n" * 3, "")
    }
  }
}
