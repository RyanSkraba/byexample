package com.skraba.byexample.scala

import com.skraba.byexample.scala.ScalaGo.{
  withConsoleMatch,
  withScalaGo,
  withScalaGoMatch
}
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

  describe("ScalaGo match helpers") {

    it("should match stdout and stderr using withConsoleMatch.") {
      withConsoleMatch {
        // Run any arbritrary code here.
        Console.out.println("Hello")
        println("World")
        Console.err.println("WARNING!")
        99
      } { case (x, stdout, stderr) =>
        x shouldBe 99
        stdout shouldBe "Hello\nWorld\n"
        stderr shouldBe "WARNING!\n"
      }
    }

    it("should match stdout and stderr using withScalaGoMatch.") {
      withScalaGoMatch() { case (stdout, stderr) =>
        stdout shouldBe "Hello, JavaScalaGo!\n" * 10
        stderr shouldBe ""
      }
    }

    it("should return stdout and stderr using withScalaGo") {
      withScalaGo() shouldBe ("Hello, JavaScalaGo!\n" * 10, "")
    }
  }

  describe("ScalaGo valid commands") {
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
  }

  describe("ScalaGo command line options") {
    for (
      args <- Seq(
        Seq("--garbage"),
        Seq("--debug", "--garbage"),
        Seq("--garbage", "--debug"),
        Seq("--garbage", "garbage")
      )
    ) it(s"throw an exception with unknown option $args") {
      val t = intercept[DocoptExitException] {
        withScalaGo(args: _*)
      }
      t.getExitCode shouldBe 1
      t.getMessage shouldBe null
    }
  }
}
